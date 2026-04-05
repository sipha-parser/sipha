//! Stage-1 opcode classification for the parse VM.
//!
//! [`STAGE_LOOKUP`] splits bytecode into a **scan-hot** bucket (byte/literal/class matching,
//! unconditional jump, [`Insn::ByteDispatch`], [`Insn::ConsumeWhileClass`]) and everything
//! else. This matches [`crate::parse::insn::opcode::is_hot_scan_opcode`].
//!
//! The main interpreter branches on [`STAGE_LOOKUP`] first, then `match`es
//! [`Insn`](crate::parse::insn::Insn) in a scan-hot vs general arm (with
//! `debug_assert!` + [`core::hint::unreachable_unchecked`] if the table ever disagrees with
//! [`Insn::opcode_u8`]). The same predicate is also useful for macros, codegen, and future
//! JIT / threaded-code experiments.

use crate::parse::insn::opcode;

/// Const lookup table: index by [`Insn::opcode_u8()`] (valid 0..=39; indices 40..255 are general).
/// Value `0` = scan-hot stage, `1` = general stage.
pub const STAGE_LOOKUP: [u8; 256] = stage_lookup_table();

const fn stage_lookup_table() -> [u8; 256] {
    let mut t = [1u8; 256];
    let mut i = 0usize;
    while i < 256 {
        let o = i as u8;
        t[i] = if opcode::is_hot_scan_opcode(o) { 0 } else { 1 };
        i += 1;
    }
    t
}

#[cfg(test)]
mod tests {
    use super::STAGE_LOOKUP;
    use crate::parse::insn::Insn;
    use crate::parse::insn::opcode;

    #[test]
    fn stage_lookup_matches_hot_scan_predicate() {
        let hot = [
            Insn::Byte {
                byte: b'a',
                on_fail: 0,
            },
            Insn::Jump { target: 0 },
        ];
        for insn in hot {
            assert!(opcode::is_hot_scan_opcode(insn.opcode_u8()));
            assert_eq!(STAGE_LOOKUP[insn.opcode_u8() as usize], 0);
        }
        let cold = [Insn::Call { rule: 0 }, Insn::Return];
        for insn in cold {
            assert!(!opcode::is_hot_scan_opcode(insn.opcode_u8()));
            assert_eq!(STAGE_LOOKUP[insn.opcode_u8() as usize], 1);
        }
    }

    #[test]
    fn stage_lookup_byte_and_call() {
        let b = Insn::Byte {
            byte: b'x',
            on_fail: 0,
        };
        assert_eq!(STAGE_LOOKUP[b.opcode_u8() as usize], 0);
        assert_eq!(STAGE_LOOKUP[Insn::Call { rule: 0 }.opcode_u8() as usize], 1);
    }
}
