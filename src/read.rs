/*use std::io;

use arrayvec::ArrayVec;
use crate::{partial_from_utf8_mut};

/// This trait is similar to io::Read, but it operates on UTF-8 strings.
pub trait StrRead {
    // Read bytes into a user-provided buffer, and return the prefix of that buffer
    // that contains a valid UTF-8 string.
    // Note that this function may return Ok("") for any of the reasons of io::Read::read,
    // and may additionally do so if the buffer is too small to fit the next code point.
    fn read<'a>(&mut self, buf: &'a mut [u8]) -> io::Result<(&'a mut str, &'a mut [u8])>;
}

pub trait StrBufRead: StrRead {
    fn fill_buf(&mut self) -> io::Result<&str>;
    fn consume(&mut self, amount: usize);
}

#[derive(Debug, Clone, Default)]
pub struct StrReader<R: io::Read> {
    reader: R,

    // In the event that reads from the underying io::Read return a partial code point,
    // store those bytes here to be later reconstructed into a full code point.
    code_point_buffer: ArrayVec<[u8; 4]>,
}

impl<R: io::Read> StrRead for StrReader<R> {
    fn read_into<'a>(&mut self, buf: &'a mut [u8]) -> io::Result<&'a mut str> {
        if buf.len() <= self.code_point_buffer.len() {
            // Hmm. The user didn't give us enough room to do a read from the
            // underlying reader. Just try to give them some bytes from the
            // code_point_buffer.
            buf.copy_from_slice(&self.code_point_buffer[..buf.len()]);
            match partial_from_utf8_mut(buf) {
                // The data in code_point_buffer was insufficent. Just return
                // an empty string.
                Ok(("", suffix)) => Ok(""),

                // We have something for them! Drain the str_part bytes from
                // code_point_buffer, then return the str.
                Ok((str_part, suffix)) => {
                    self.code_point_buffer.drain(..str_part.len());
                    Ok(str_part);
                }

                Err(err) => unreachable!("Invalid UTF8 bytes were somehow added to code_point_buffer: {:#X?}", self.code_point_buffer)
            }
        } else {
            // Count is the total number of bytes that we copied + read into buf
            let count = if !self.code_point_buffer.is_empty() {
                // We have some data in code_point_buffer from a prior read. Copy those
                // bytes into buf, then do a read from the underlying reader.

                let (lhs, rhs) = buf.split_at_mut(self.code_point_buffer.len());

                let count_from_buffer = self.code_point_buffer.len();
                lhs.copy_from_slice(&self.code_point_buffer);

                let count_from_reader = self.reader.read(rhs)?;

                self.code_point_buffer.clear();

                count_from_buffer + count_from_reader
            } else {
                //
                self.reader.read(buf)?
            };

            match partial_from_utf8_mut(&mut buf[..count]) {
                Ok((str_part, suffix)) => {
                    self.code_point_buffer.extend(suffix.iter().cloned());
                    Ok(str_part)
                },
                Err(err) => {
                    Err(io::Error::new(io::ErrorKind::InvalidData, err))
                }
            }
        }
    }
}
*/
/*
trait Length {
    fn len(&self) -> usize;
    fn truncate(&mut self);
}

impl<T: arrayvec::Array> Length for ArrayVec<T> {
    fn len(&self) -> usize {
        ArrayVec::len(self)
    }

    fn truncate(&mut self) {
        ArrayVec::truncate(self)
    }
}

#[derive(Debug, Clone)]
struct LengthGuard<T: Length> {
    pub value: T,
    pub target_len: usize
}

impl<T: Length> LengthGuard<T> {
    fn new(value: T) -> Self {
        LengthGuard {
            value: value,
            target_len: value.len(),
        }
    }
}

impl<T: Length> Drop for LengthGuard<T> {
    fn drop(&mut self) {
        self.value.truncate(target_len),
    }
}

impl<R: io::Read> StrBufRead for StrReader<R> {
    // Given that the code_point_buffer has a partial code point in it, read
    // until it has a full code point.
    fn fill_code_point_buf(&mut self) -> io::Result<()> {
        // We need to fill our code_point_buffer with more bytes until it
        // has at least one code point, then return it. It is critical that this
        // function not leave code_point_buffer in a bad state.

        let target_len = match self.code_point_buffer.first().cloned() {
            Some(0b110_00000...0b110_11111) => 2,
            Some(0b1110_0000...0b1110_1111) => 3,
            Some(0b11110_000...0b11110_111) => 4,
            Some(b) => unreachable!("Bad bytes in code_point_buffer: {:#X?}", self.code_point_buffer),
            None => Ok(()),
        }

        let original_length = self.code_point_buffer.len();
        let guard = LengthGuard::new(&mut self.code_point_buffer);

        // TODO: when Read::initializer becomes stable, use it instead of pushing.
        while guard.value.len() < target_len {
            unsafe { guard.value.push_unchecked(0) };
        }

        let mut write_buf = &mut guard.value[current_len..]

        while !write_buf.is_empty() {
            match self.reader.read(write_buf) {
                Ok(0) => return Err(io::ErrorKind::UnexpectedEof.into()),
                Ok(n) => {
                    guard.target_len += n;
                    write_buf = &mut guard.value[guard.target_len..];
                }
                Err(ref err) if err.kind() == io::ErrorKind::Interrupted => continue,
                Err(err) => return Err(io::Error),
            }
        }

        // At this point we should have a complete code point. Check it.
    }
}

impl<R: io::BufRead> StrBufRead for StrReader<R> {
    fn fill_buf(&mut self) -> io::Result<&str> {
        // This function is a bit odd, because we have two buffers to work with:
        // our own code_point_buffer, and the underlying BufRead buffer.
        let reader = &mut self.reader;
        let code_point_buffer = &mut self.code_point_buffer;

        match code_point_buffer.first() {
            // If our code_point_buffer is empty, we can work with the underlying
            // fill_buf directly.
            None => match reader.fill_buf() {
                Ok(b"") => Ok(""),
                Err(err) => Err(err),
                Ok(buf) => match partial_from_utf8(buf) {
                    Ok(("", suffix)) => {
                        // This is a problem. Calling self.reader.fill_buf over and
                        // over will simply keep returning the unused suffix. We have
                        // to store it ourselves, then give the user our own code_point_buffer.
                        code_point_buffer.extend(suffix.iter().cloned());
                        reader.consume(suffix.len());

                        // Now what? Need to figure out how to call the other branch of this
                        // match. Probably needs to be a separate function.
                        panic!("We haven't finished this part yet")
                    },
                    Ok((str_part, _suffix)) => Ok(str_part),
                    Err(err) => Err(io::Error::new(io::ErrorKind::InvalidData, err)),
                }
            }
            // If we have data in code_point buffer, we need to try to fill it with a
            // single code point, then present that to the user.
            Some(b) => {
                let current_buf_len = code_point_buffer.len();
                let underlying_buf = reader.fill_buf()?;

                // Push these bytes into the code_point_buffer. ArrayVec::extend
                // automatically stops silently when the ArrayVec is full.
                self.code_point_buffer.extend(underlying_buf.iter().cloned());

                unimplemented!();
            }
        }
        }
    }

    fn consume(&mut self, amt: usize) {
        if self.code_point_buffer.is_empty() {
            // TODO: find a way to assert that amt is at a code point boundary.
            // The problem is that if we call self.reader.fill_buf in here, those
            // no guarentee that it won't result in an io::Error, which there's no
            // way to forward back to the client. It may not matter in practice,
            // since if they consume a bad number of bytes, the subsequent fill_buf
            // will definitely return an error.
            self.reader.consume(amt)
        } else {
        }
    }
}
*/
