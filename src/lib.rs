/*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/

use std::fmt;
use std::io;
use std::str::{from_utf8, from_utf8_unchecked, Utf8Error};

use arrayvec::ArrayVec;

/// Convert a (possibly truncated) byte array to a str and a partial code point.
///
/// Attempt to convert a byte slice to a string, in a context where the bytes
/// are valid UTF-8 that was potentially cut off halfway through. If successful,
/// the function will return the longest UTF-8 string possible, as well as the
/// suffix of the bytes which are a partial UTF-8 code point. If any invalid
/// bytes are encountered, return the Utf8Error.
pub fn partial_from_utf8(buf: &[u8]) -> Result<(&str, &[u8]), Utf8Error> {
    match from_utf8(buf) {
        Ok(buf_str) => Ok((buf_str, &[])),
        Err(err) if err.error_len().is_some() => Err(err),
        Err(err) => {
            let valid_utf8_boundary = err.valid_up_to();
            let full_str =
                unsafe { from_utf8_unchecked(buf.get_unchecked(..valid_utf8_boundary)) };
            let partial_code_point = unsafe { buf.get_unchecked(valid_utf8_boundary..) };
            Ok((full_str, partial_code_point))
        }
    }
}

/// Returns true if a byte is a UTF-8 continuation byte
#[inline]
pub fn is_continuation_byte(b: u8) -> bool {
    // TODO: const fn
    // TODO: there's probably a faster way to do this with bitwise operations
    b >= 0b1000_0000 && b <= 0b1011_1111
}

/// This trait is designed to provide a similar interface to io::Write, but
/// is designed to operate on strings. Unlike fmt::Write, it can report partial
/// writes, but it guarantees that a whole number of code points were written
/// with every write. Generally this involves buffering partial code points
/// as necessary.
pub trait StrWrite: fmt::Write {
    /// Write a str to the writer. Like [`io::Write::write`], this function returns
    /// the number of bytes written; it additionally GUARENTEES that the number
    /// of written bytes encompasses a whole number of code points.
    fn write(&mut self, buf: &str) -> io::Result<usize>;
    fn flush(&mut self) -> io::Result<()>;
}

// This struct adapts an io::Write into a StrWrite
#[derive(Debug, Clone)]
pub struct WrapIoWrite<W> {
    writer: W,
    code_point_buffer: ArrayVec<[u8; 3]>,
}

impl<W> WrapIoWrite<W> {
    pub fn wrap(writer: W) -> Self {
        WrapIoWrite {
            writer,
            code_point_buffer: ArrayVec::new(),
        }
    }
}

#[inline]
fn retry_interrupts<T, F: FnMut() -> io::Result<T>>(mut f: F) -> io::Result<T> {
    loop {
        match f() {
            Ok(result) => break Ok(result),
            Err(ref err) if err.kind() == io::ErrorKind::Interrupted => continue,
            Err(err) => break Err(err),
        }
    }
}

impl<W: io::Write> WrapIoWrite<W> {
    fn flush_code_point_buffer(&mut self) -> io::Result<()> {
        while !self.code_point_buffer.is_empty() {
            match self.writer.write(&self.code_point_buffer)? {
                0 => return Err(io::ErrorKind::WriteZero.into()),
                written => self.code_point_buffer.drain(..written),
            };
        }
        Ok(())
    }

    fn flush_code_point_buffer_retrying(&mut self) -> io::Result<()> {
        retry_interrupts(move || self.flush_code_point_buffer())
    }
}

impl<W: io::Write> StrWrite for WrapIoWrite<W> {
    fn write(&mut self, buf: &str) -> io::Result<usize> {
        self.flush_code_point_buffer()?;

        let buf_bytes = buf.as_bytes();
        let mut written = self.writer.write(buf_bytes)?;
        let unwritten = &buf_bytes[written..];

        for &b in unwritten {
            if is_continuation_byte(b) {
                // Because the bytes came from a str, they're definitely not malformed
                // UTF-8, which means that this push can't overflow
                if let Err(_err) = self.code_point_buffer.try_push(b) {
                    // TODO: if we determine it's safe, replace this with unreachable unchecked
                    unreachable!("Got a string with malformed UTF-8");
                }
                // This is effectively a write, since the user shouldn't
                // retry this byte.
                written += 1;
            } else {
                break;
            }
        }
        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        retry_interrupts(|| self.flush_code_point_buffer())?;
        self.writer.flush()
    }
}

impl<W: io::Write> fmt::Write for WrapIoWrite<W> {
    fn write_str(&mut self, buf: &str) -> Result<(), fmt::Error> {
        self.flush_code_point_buffer_retrying()
            .map_err(|_| fmt::Error)?;
        self.writer
            .write_all(buf.as_bytes())
            .map_err(|_| fmt::Error)
    }

    fn write_fmt(&mut self, args: fmt::Arguments) -> Result<(), fmt::Error> {
        self.flush_code_point_buffer_retrying()
            .map_err(|_| fmt::Error)?;
        self.writer.write_fmt(args).map_err(|_| fmt::Error)
    }
}

pub struct WrapStrWrite<W> {
    writer: W,
    unwritten_byte_buffer: ArrayVec<[u8; 4]>,
}

impl<W: StrWrite> io::Write for WrapStrWrite<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.unwritten_byte_buffer.is_empty() {
            // If the unwritten_byte_buffer is empty, then buf should start with a valid
            // UTF-8 string that may have been cut off. We attempt to write the part
            // which can be converted into a str, then store the trailing bytes.
            match partial_from_utf8(buf) {
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
            }
        } else {
            // If there is content in the unwritten_byte_buffer, try to fill out a single
            // complete code point and write that
            let current_length = self.unwritten_byte_buffer.len();

            // Extend is fine to use here because it automatically caps out at
            // the max size of the ArrayVec
            self.unwritten_byte_buffer.extend(
                buf.iter()
                    .cloned()
                    .take_while(move |b| is_continuation_byte(*b)),
            );

            match partial_from_utf8(&self.unwritten_byte_buffer) {
                Err(err) => {
                    // In this case, the new bytes were invalid. Truncate them and return the error.
                    self.unwritten_byte_buffer.truncate(current_length);
                    Err(io::Error::new(io::ErrorKind::InvalidData, err))
                }
                Ok(("", _)) => {
                    // In this case, we didn't have enough new data. Mark the bytes as written
                    // (since we put them in the unwritten_byte_buffer)
                    Ok(self.unwritten_byte_buffer.len() - current_length)
                }
                Ok((code_point, &[])) => {
                    // We have a complete code point. Write it with write_str. If there's an error,
                    // re-truncate and pass it back to the client.
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

                            // This means that it shouldn't be possible that 0 new bytes that we
                            // got from the user were written (because it adds unwritten bytes to
                            // unwritten_continuation_bytes)
                            debug_assert_ne!(written - current_length, 0);

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

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_is_continuation_byte() {
        use crate::is_continuation_byte;

        let content = "a😀b".as_bytes();

        assert!(!is_continuation_byte(content[0]));
        assert!(!is_continuation_byte(content[1]));
        assert!(is_continuation_byte(content[2]));
        assert!(is_continuation_byte(content[3]));
        assert!(is_continuation_byte(content[4]));
        assert!(!is_continuation_byte(content[5]));
    }

    mod test_partial_from_utf8 {
        use crate::partial_from_utf8;

        #[test]
        fn test_empty() {
            assert_eq!(partial_from_utf8(b""), Ok(("", b"" as &[u8])));
        }

        #[test]
        fn test_simple_string() {
            assert_eq!(
                partial_from_utf8(&[0x61, 0xC3, 0xA9]),
                Ok(("aé", b"" as &[u8]))
            );
        }

        #[test]
        fn test_partial_string() {
            // UTF-8 equivelent of "😀😀", minus the last byte
            assert_eq!(
                partial_from_utf8(&[0xF0, 0x9F, 0x98, 0x80, 0xF0, 0x9F, 0x98]),
                Ok(("😀", &[0xF0u8, 0x9Fu8, 0x98u8] as &[u8]))
            );
        }

        #[test]
        fn test_not_unicode() {
            match partial_from_utf8(&[0x61, 0xFF]) {
                Ok(_) => assert!(false),
                Err(err) => {
                    assert_eq!(err.valid_up_to(), 1);
                    assert!(err.error_len().is_some());
                }
            }
        }

        #[test]
        fn test_bad_unicode() {
            match partial_from_utf8(&[0x61, 0xF0, 0x9F, 0xF0]) {
                Ok(_) => assert!(false),
                Err(err) => {
                    assert_eq!(err.valid_up_to(), 1);
                    assert!(err.error_len().is_some());
                }
            }
        }
    }
}
