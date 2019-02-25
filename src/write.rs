#![warn(bare_trait_objects)]
#![deny(missing_debug_implementations)]
#![warn(missing_docs)]
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
use std::fmt;
use std::io;

use crate::partial_from_utf8;
use arrayvec::ArrayVec;

/// Returns true if a byte is a UTF-8 continuation byte
#[inline]
fn is_continuation_byte(b: u8) -> bool {
    // TODO: const fn
    // TODO: there's probably a faster way to do this with bitwise operations
    b >= 0b1000_0000 && b <= 0b1011_1111
}

#[cfg(test)]
#[test]
fn test_is_continuation_byte() {
    let mut bytes = "a😀b".as_bytes().iter().cloned();

    assert!(!is_continuation_byte(bytes.next().unwrap()));
    assert!(!is_continuation_byte(bytes.next().unwrap()));
    assert!(is_continuation_byte(bytes.next().unwrap()));
    assert!(is_continuation_byte(bytes.next().unwrap()));
    assert!(is_continuation_byte(bytes.next().unwrap()));
    assert!(!is_continuation_byte(bytes.next().unwrap()));
    assert_eq!(bytes.next(), None)
}

/// This trait is designed to provide a similar interface to io::Write, but
/// is designed to operate on strings. Unlike fmt::Write, it can report partial
/// writes, but it guarantees that a whole number of code points were written
/// with every write. Generally this involves buffering partial code points
/// as necessary.
pub trait StrWrite {
    /// Write a str to the writer. Like [`io::Write::write`], this function returns
    /// the number of bytes written; it additionally guarantees that the number
    /// of written bytes encompasses a whole number of code points. Like with
    /// io::Write, it also guarantees that errors mean that 0 bytes were written,
    /// and that the write can therefore (potentially) be retried.
    fn write(&mut self, buf: &str) -> io::Result<usize>;

    /// Flush this output stream, ensuring that all intermediately buffered
    /// contents reach their destination.
    fn flush(&mut self) -> io::Result<()>;

    /// Continuously write buf to the stream until either the whole thing is
    /// written or an error occurs. An implementation of this function is
    /// provided by default, which calls write in a loop, but in general
    /// implementors should wrap the underlying write_all method, if available.
    fn write_all(&mut self, mut buf: &str) -> io::Result<()> {
        while !buf.is_empty() {
            match self.write(buf) {
                Ok(0) => return Err(io::ErrorKind::WriteZero.into()),
                Ok(n) => buf = &buf[n..],
                Err(ref err) if err.kind() == io::ErrorKind::Interrupted => {}
                Err(err) => return Err(err),
            }
        }
        Ok(())
    }

    /// Write some format arguments to the stream. This makes StrWrite compatible
    /// with write!, println!, etc.
    fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()>;

    /// Get a mutable reference to this writer
    fn by_ref(&mut self) -> &mut Self {
        self
    }
}

pub trait IntoStrWrite: io::Write {
    fn into_str_write(self) -> StrWriter<Self>
    where
        Self: Sized,
    {
        StrWriter::wrap(self)
    }

    fn str_writer_by_ref(&mut self) -> StrWriter<&mut Self> {
        StrWriter::wrap(self)
    }
}

impl<W: io::Write> IntoStrWrite for W {}

/// This struct adapts an io::Write into a StrWrite
#[derive(Debug, Clone, Default)]
pub struct StrWriter<W: io::Write> {
    writer: W,
    code_point_buffer: ArrayVec<[u8; 3]>,
}

impl<W: io::Write> StrWriter<W> {
    pub fn wrap(writer: W) -> Self {
        StrWriter {
            writer,
            code_point_buffer: ArrayVec::new(),
        }
    }
}

impl<W: io::Write> StrWriter<W> {
    // Attempt to flush a code_point_buffer, retrying interrupts
    #[inline]
    fn flush_buffer(&mut self) -> io::Result<()> {
        while !self.code_point_buffer.is_empty() {
            match self.writer.write(&self.code_point_buffer) {
                Ok(0) => return Err(io::ErrorKind::WriteZero.into()),
                Ok(n) => self.code_point_buffer.drain(..n),
                Err(ref err) if err.kind() == io::ErrorKind::Interrupted => continue,
                Err(err) => return Err(err),
            };
        }

        Ok(())
    }
}

impl<W: io::Write> StrWrite for StrWriter<W> {
    fn write(&mut self, buf: &str) -> io::Result<usize> {
        // Variant flush buffer: return interrupted errors and Ok(0) without modification
        while !self.code_point_buffer.is_empty() {
            match self.writer.write(&self.code_point_buffer) {
                Ok(n) if n > 0 => self.code_point_buffer.drain(..n),
                result => return result,
            };
        }

        let buf_bytes = buf.as_bytes();
        let mut written = self.writer.write(buf_bytes)?;

        self.code_point_buffer.extend(
            buf_bytes[written..]
                .into_iter()
                .cloned()
                .filter(move |&b| is_continuation_byte(b)),
        );
        written += self.code_point_buffer.len();

        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.flush_buffer()?;
        self.writer.flush()
    }

    fn write_all(&mut self, buf: &str) -> io::Result<()> {
        // Because write_all doesn't specify how much of the output was written, we're
        // under no obligation to specify that only a fixed number of code points were
        // written, so we can use the underlyting write_all directly.
        self.flush_buffer()?;
        self.writer.write_all(buf.as_bytes())
    }

    fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
        // TODO: is there a way to prefix fmt::Arguments with some raw bytes?
        self.flush_buffer()?;
        self.writer.write_fmt(args)
    }
}

impl<W: io::Write> Drop for StrWriter<W> {
    fn drop(&mut self) {
        let _result = self.flush_buffer();

        // Don't call self.flush, since we rely on the underlying writer to
        // flush itself on drop.
    }
}

// StrWriter is automatically implemented for all fmt::Write types
impl<'a, T: fmt::Write> StrWrite for T {
    fn write(&mut self, buf: &str) -> io::Result<usize> {
        match self.write_str(buf) {
            Err(err) => Err(io::Error::new(io::ErrorKind::Other, err)),
            Ok(()) => Ok(buf.len()),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }

    fn write_fmt(&mut self, args: fmt::Arguments) -> io::Result<()> {
        fmt::Write::write_fmt(self, args)
            .map_err(move |err| io::Error::new(io::ErrorKind::Other, err))
    }
}

/// This struct adapts a StrWrite into an io::Write
#[derive(Debug, Clone, Default)]
pub struct IoStrWriter<W> {
    writer: W,
    unwritten_byte_buffer: ArrayVec<[u8; 4]>,
}

impl<W: StrWrite> io::Write for IoStrWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self.unwritten_byte_buffer.first().cloned() {
            // If the unwritten_byte_buffer is empty, then buf should start with a valid
            // UTF-8 string that may have been cut off. We attempt to write the part
            // which can be converted into a str, then store the trailing bytes.
            None => match partial_from_utf8(buf) {
                Err(err) => Err(io::Error::new(io::ErrorKind::InvalidData, err)),
                Ok(("", suffix)) => {
                    self.unwritten_byte_buffer.extend(suffix.iter().cloned());
                    Ok(suffix.len())
                }
                Ok((valid_utf8, _)) => {
                    // Note: we could check if write_str wrote all the bytes of valid_utf8, and if
                    // so, store the suffix bytes and report the whole thing as written. However,
                    // we'd rather if we never hit the code path for unwritten_byte_buffer.
                    let written = self.writer.write(valid_utf8)?;
                    debug_assert!(valid_utf8.is_char_boundary(written));
                    Ok(written)
                }
            },
            Some(b) => {
                // If there is content in the unwritten_byte_buffer, try to fill out a single
                // complete code point and write that
                let current_length = self.unwritten_byte_buffer.len();
                let target_length = match b & 0b1111_0000 {
                    0b1100_0000 => 2,
                    0b1110_0000 => 3,
                    0b1111_0000 => 4,
                    b => unreachable!(
                        "unwritten_byte_buffer has an invalid leading UTF-8 byte: {:#X}",
                        b
                    ),
                };

                self.unwritten_byte_buffer
                    .extend(buf.iter().cloned().take(target_length - current_length));

                match partial_from_utf8(&self.unwritten_byte_buffer) {
                    // In this case, the new bytes were invalid. Truncate them and return the error.
                    Err(err) => {
                        self.unwritten_byte_buffer.truncate(current_length);
                        Err(io::Error::new(io::ErrorKind::InvalidData, err))
                    }

                    // In this case, we didn't have enough new data. Mark the bytes as written
                    // (since we put them in the unwritten_byte_buffer)
                    Ok(("", _)) => Ok(self.unwritten_byte_buffer.len() - current_length),

                    // We have a complete code point. Write it with write_str. If there's an error,
                    // re-truncate and pass it back to the client.
                    Ok((code_point, &[])) => {
                        match self.writer.write(code_point) {
                            Err(err) => {
                                self.unwritten_byte_buffer.truncate(current_length);
                                Err(err)
                            }
                            Ok(0) => {
                                self.unwritten_byte_buffer.truncate(current_length);
                                Ok(0)
                            }
                            Ok(written) => {
                                // It shouldn't be possible for written to be a different length
                                // than unwritten_byte_buffer.len(), since write_str guarentees at
                                // least that an integer number of code points are written, and
                                // and unwritten_byte_buffer should contain exactly one code point
                                debug_assert_eq!(self.unwritten_byte_buffer.len(), written);

                                self.unwritten_byte_buffer.clear();

                                // Subtract current_len, since we already told the user that those bytes
                                // were written during a previous write call.
                                Ok(written - current_length)
                            }
                        }
                    }
                    // THis isn't possible, because it implies that we have ~1.5 code points in the
                    // buffer, and we made sure to only append continuation bytes.
                    Ok(_) => unreachable!(
                        "unwritten_byte_buffer had bytes from more than one code point: {:?}",
                        self.unwritten_byte_buffer
                    ),
                }
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }

    fn write_all(&mut self, mut buf: &[u8]) -> io::Result<()> {
        // First, take care of the unwritten_byte_buffer
        if let Some(b) = self.unwritten_byte_buffer.first().cloned() {
            let current_length = self.unwritten_byte_buffer.len();
            let target_length = match b & 0b1111_0000 {
                0b1100_0000 => 2,
                0b1110_0000 => 3,
                0b1111_0000 => 4,
                b => unreachable!(
                    "unwritten_byte_buffer has an invalid leading UTF-8 byte: {:#X}",
                    b
                ),
            };

            // The number of extra bytes to add to the unwritten_byte_buffer.
            // We'll reuse this number later, to re-slice the user's buffer and
            // try to write it with write_all.
            let num_extra_bytes = target_length - current_length;
            self.unwritten_byte_buffer
                .extend(buf.iter().cloned().take(num_extra_bytes));

            match partial_from_utf8(&self.unwritten_byte_buffer) {
                // In this case, the new bytes were invalid. Truncate them and return the error.
                Err(err) => {
                    self.unwritten_byte_buffer.truncate(current_length);
                    return Err(io::Error::new(io::ErrorKind::InvalidData, err));
                }

                // We got some new data, but not enough for a whole code point.
                Ok(("", _suffix)) => return Ok(()),

                // We got a whole code point! Write it, then continue writing
                Ok((str_part, &[])) => self.writer.write_all(str_part)?,

                Ok(_) => unreachable!(
                    "Invalid data was present in unwritten_byte_buffer: ${:#X?}",
                    self.unwritten_byte_buffer
                ),
            }

            self.unwritten_byte_buffer.clear();

            buf = match buf.get(num_extra_bytes..) {
                None | Some(&[]) => return Ok(()),
                Some(tail) => tail,
            };
        }

        match partial_from_utf8(buf) {
            Err(err) => Err(io::Error::new(io::ErrorKind::InvalidData, err)),
            Ok((str_part, suffix)) => {
                self.writer.write_all(str_part)?;
                self.unwritten_byte_buffer.extend(suffix.iter().cloned());
                Ok(())
            }
        }
    }
}
