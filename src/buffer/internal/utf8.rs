//! Helper functions for working with utf8 encoded text.

// The following helper functions are adapted from nightly APIs in std::core::str
// -> https://doc.rust-lang.org/stable/src/core/str/validations.rs.html

/// Mask of the value bits of a continuation byte.
const CONT_MASK: u8 = 0b0011_1111;

#[inline]
pub(super) fn count_chars(bytes: &[u8]) -> usize {
    if bytes.is_empty() {
        return 0;
    }

    let mut n_chars = 0;
    let mut cur = 0;

    while cur < bytes.len() {
        // SAFETY: we know we are in bounds and that we contain valid utf-8 data
        let ch = unsafe { decode_char_at(cur, bytes) };
        cur += ch.len_utf8();
        n_chars += 1;
    }

    n_chars
}

/// Returns the initial codepoint accumulator for the first byte.
/// The first byte is special, only want bottom 5 bits for width 2, 4 bits
/// for width 3, and 3 bits for width 4.
#[inline]
pub(super) const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
pub(super) const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// Checks whether the byte is a UTF-8 continuation byte (i.e., starts with the
/// bits `10`).
#[inline]
pub(super) const fn utf8_is_cont_byte(byte: u8) -> bool {
    (byte as i8) < -64
}

/// Decode a utf-8 code point from `bytes` starting at `start`.
/// `bytes` must contain valid utf-8 data beginning at `start`
#[inline]
pub(super) unsafe fn decode_char_at(start: usize, bytes: &[u8]) -> char {
    // Decode UTF-8
    // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
    let x = bytes[start];
    if x < 128 {
        // SAFETY: x is in the correct range
        return unsafe { char::from_u32_unchecked(x as u32) };
    }

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    // NOTE: Performance is sensitive to the exact formulation here
    let init = utf8_first_byte(x, 2);
    // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
    let y = bytes[start + 1];
    let mut ch = utf8_acc_cont_byte(init, y);

    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
        let z = bytes[start + 2];
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = (init << 12) | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
            let w = bytes[start + 3];
            ch = ((init & 7) << 18) | utf8_acc_cont_byte(y_z, w);
        }
    }

    // SAFETY: we know that ch is valid at this point
    unsafe { char::from_u32_unchecked(ch) }
}

/// Decode a utf-8 code point from `bytes` ending at `end`.
/// `bytes` must contain valid utf-8 data ending at `end`
#[inline]
pub(super) unsafe fn decode_char_ending_at(end: usize, bytes: &[u8]) -> char {
    // Decode UTF-8
    let w = match bytes[end] {
        // SAFETY: b is in range
        b if b < 128 => return unsafe { char::from_u32_unchecked(b as u32) },
        b => b,
    };

    // Multibyte case follows
    // Decode from a byte combination out of: [x [y [z w]]]
    let mut ch;
    // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
    let z = bytes[end - 1];
    ch = utf8_first_byte(z, 2);
    if utf8_is_cont_byte(z) {
        // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
        let y = bytes[end - 2];
        ch = utf8_first_byte(y, 3);
        if utf8_is_cont_byte(y) {
            // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
            let x = bytes[end - 3];
            ch = utf8_first_byte(x, 4);
            ch = utf8_acc_cont_byte(ch, y);
        }
        ch = utf8_acc_cont_byte(ch, z);
    }
    ch = utf8_acc_cont_byte(ch, w);

    // SAFETY: we know that ch is valid at this point
    unsafe { char::from_u32_unchecked(ch) }
}
