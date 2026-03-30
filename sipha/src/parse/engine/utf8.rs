#[inline]
pub(super) fn decode_utf8(bytes: &[u8], pos: usize) -> Option<(u32, usize)> {
    let b0 = *bytes.get(pos)?;
    if b0 < 0x80 {
        return Some((u32::from(b0), 1));
    }
    if b0 < 0xC2 {
        return None;
    }
    if b0 < 0xE0 {
        let b1 = *bytes.get(pos + 1)?;
        if b1 & 0xC0 != 0x80 {
            return None;
        }
        return Some((((u32::from(b0) & 0x1F) << 6) | (u32::from(b1) & 0x3F), 2));
    }
    if b0 < 0xF0 {
        let b1 = *bytes.get(pos + 1)?;
        let b2 = *bytes.get(pos + 2)?;
        if b1 & 0xC0 != 0x80 || b2 & 0xC0 != 0x80 {
            return None;
        }
        let cp =
            ((u32::from(b0) & 0x0F) << 12) | ((u32::from(b1) & 0x3F) << 6) | (u32::from(b2) & 0x3F);
        if cp < 0x800 || (0xD800..=0xDFFF).contains(&cp) {
            return None;
        }
        return Some((cp, 3));
    }
    if b0 < 0xF5 {
        let b1 = *bytes.get(pos + 1)?;
        let b2 = *bytes.get(pos + 2)?;
        let b3 = *bytes.get(pos + 3)?;
        if b1 & 0xC0 != 0x80 || b2 & 0xC0 != 0x80 || b3 & 0xC0 != 0x80 {
            return None;
        }
        let cp = ((u32::from(b0) & 0x07) << 18)
            | ((u32::from(b1) & 0x3F) << 12)
            | ((u32::from(b2) & 0x3F) << 6)
            | (u32::from(b3) & 0x3F);
        if !(0x10000..=0x10_FFFF).contains(&cp) {
            return None;
        }
        return Some((cp, 4));
    }
    None
}
