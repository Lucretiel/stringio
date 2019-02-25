#![warn(bare_trait_objects)]
#![deny(missing_debug_implementations)]
#![warn(missing_docs)]

pub mod read;
pub mod write;

use std::str::{from_utf8, from_utf8_mut, from_utf8_unchecked, from_utf8_unchecked_mut, Utf8Error};

// TODO: deduplicate with partial_from_utf8_mut

/// Convert a (possibly truncated) UTD-8 byte array to a str and a partial code point.
///
/// Attempt to convert a byte slice to a string, in a context where the bytes
/// are valid UTF-8 that was potentially cut off halfway through. If successful,
/// the function will return the longest UTF-8 string possible, as well as the
/// suffix of the bytes which are a partial UTF-8 code point. If any invalid
/// bytes are encountered, return the Utf8Error.
///
/// Example:
///
/// ```
/// use stringio::partial_from_utf8;
/// // UTF-8 equivelent of "😀😀", minus the last byte
/// assert_eq!(
///     partial_from_utf8(&[0xF0, 0x9F, 0x98, 0x80, 0xF0, 0x9F, 0x98]),
///     Ok(("😀", &[0xF0u8, 0x9Fu8, 0x98u8] as &[u8])),
/// );
/// ```
pub fn partial_from_utf8(buf: &[u8]) -> Result<(&str, &[u8]), Utf8Error> {
    match from_utf8(buf) {
        Ok(buf_str) => Ok((buf_str, &[])),
        Err(err) if err.error_len().is_some() => Err(err),
        Err(err) => {
            let valid_utf8_boundary = err.valid_up_to();
            let full_str = unsafe { from_utf8_unchecked(buf.get_unchecked(..valid_utf8_boundary)) };
            let partial_code_point = unsafe { buf.get_unchecked(valid_utf8_boundary..) };
            Ok((full_str, partial_code_point))
        }
    }
}

/// Converte a (possible trucated) mutable UTD-8 byte array to a str and a partial code point.
///
/// THis function is the same as [`partial_from_utf8`], but for mutable slices; see
/// its documentation for details.
///
/// /// Example:
///
/// ```
/// use stringio::partial_from_utf8_mut;
/// // UTF-8 equivelent of "😀😀", minus the last byte
/// let mut data = [0xF0, 0x9F, 0x98, 0x80, 0xF0, 0x9F, 0x98];
/// let mut tail = [0xF0u8, 0x9F, 0x98];
/// assert_eq!(
///     partial_from_utf8_mut(&mut data),
///     Ok((String::from("😀").as_mut_str(), &mut tail[..])),
/// );
/// ```
pub fn partial_from_utf8_mut(buf: &mut [u8]) -> Result<(&mut str, &mut [u8]), Utf8Error> {
    match from_utf8_mut(buf) {
        Ok(_) => Ok((unsafe { from_utf8_unchecked_mut(buf) }, &mut [])),
        Err(err) if err.error_len().is_some() => Err(err),
        Err(err) => {
            let valid_utf8_boundary = err.valid_up_to();
            let (lhs, rhs) = buf.split_at_mut(valid_utf8_boundary);
            let str_part = unsafe { from_utf8_unchecked_mut(lhs) };
            Ok((str_part, rhs))
        }
    }
}

#[cfg(test)]
mod tests {
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
