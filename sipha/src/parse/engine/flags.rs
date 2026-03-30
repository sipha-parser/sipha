use super::frames::SnapEntry;

#[inline]
pub(super) fn flag_is_set(flags: &[u64], flag_id: u16) -> bool {
    let w = (flag_id >> 6) as usize;
    w < flags.len() && (flags[w] >> (flag_id & 63)) & 1 != 0
}

#[inline]
pub(super) fn restore_snapshot(flags: &mut [u64], snaps: &mut Vec<SnapEntry>, mark: u32) {
    let mark = mark as usize;
    for entry in snaps[mark..].iter().rev() {
        if (entry.word as usize) < flags.len() {
            flags[entry.word as usize] = entry.val;
        }
    }
    snaps.truncate(mark);
}
