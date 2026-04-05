//! # SIMD-Accelerated Literal Comparison
//!
//! Provides [`literal_eq`] for table-backed literals and [`literal_small_eq`] for
//! up-to-8-byte inline literals (scalar unaligned word compares, no `memcmp` setup).
//!
//! [`literal_eq`] additionally:
//!
//! - Returns immediately if the input doesn't have enough bytes.
//! - For literals ≥ 32 bytes on AVX2 machines, compares 32 bytes per cycle.
//! - For literals ≥ 16 bytes on SSE2 machines (all `x86_64`), compares 16 bytes
//!   per cycle.
//! - Falls back to a plain slice comparison on other platforms, which the
//!   compiler already lowers to `memcmp` / SIMD via auto-vectorisation.
//!
//! On **x86_64** with the `std` feature, AVX2 availability is probed once and
//! stored in an atomic instead of calling `is_x86_feature_detected!` on every
//! long literal match (grammars can issue thousands of [`literal_eq`](literal_eq)
//! calls per parse).
//!
//! ## Why bother?
//!
//! For short keywords like `"true"` (4 bytes) the compiler generates a single
//! 32-bit integer compare — no room for improvement.  For longer tokens such
//! as UUID strings, HTTP method names, or base64 chunks, the SIMD paths
//! deliver a measurable throughput gain.

use crate::types::Pos;

/// Compare `input[off .. off + n]` with the first `n` bytes of `bytes` (`n ≤ 8`).
///
/// For `n == 0`, returns `true` without reading `input` (nothing to compare).
/// Returns `false` if `off + n` is out of range or `n > 8`.
#[must_use]
#[inline]
pub fn literal_small_eq(input: &[u8], off: usize, n: usize, bytes: &[u8; 8]) -> bool {
    if n > 8 {
        return false;
    }
    if n == 0 {
        return true;
    }
    let Some(end) = off.checked_add(n) else {
        return false;
    };
    if end > input.len() {
        return false;
    }
    let p = input.as_ptr().wrapping_add(off);
    let q = bytes.as_ptr();
    // SAFETY: `end <= input.len()` and `n <= 8`; all pointer offsets stay within the compared regions.
    unsafe {
        match n {
            1 => *p == *q,
            2 => (p as *const u16).read_unaligned() == (q as *const u16).read_unaligned(),
            3 => {
                (p as *const u16).read_unaligned() == (q as *const u16).read_unaligned()
                    && *p.add(2) == *q.add(2)
            }
            4 => (p as *const u32).read_unaligned() == (q as *const u32).read_unaligned(),
            5 => {
                (p as *const u32).read_unaligned() == (q as *const u32).read_unaligned()
                    && *p.add(4) == *q.add(4)
            }
            6 => {
                (p as *const u32).read_unaligned() == (q as *const u32).read_unaligned()
                    && (p.add(4) as *const u16).read_unaligned()
                        == (q.add(4) as *const u16).read_unaligned()
            }
            7 => {
                (p as *const u32).read_unaligned() == (q as *const u32).read_unaligned()
                    && (p.add(4) as *const u16).read_unaligned()
                        == (q.add(4) as *const u16).read_unaligned()
                    && *p.add(6) == *q.add(6)
            }
            8 => (p as *const u64).read_unaligned() == (q as *const u64).read_unaligned(),
            _ => false,
        }
    }
}

/// `2` = unset, `1` = AVX2 available, `0` = not available.
#[cfg(all(target_arch = "x86_64", feature = "std"))]
static X86_AVX2_CACHED: std::sync::atomic::AtomicU8 = std::sync::atomic::AtomicU8::new(2);

#[cfg(all(target_arch = "x86_64", feature = "std"))]
#[inline]
fn x86_cached_has_avx2() -> bool {
    use std::arch::is_x86_feature_detected;
    use std::sync::atomic::Ordering;
    match X86_AVX2_CACHED.load(Ordering::Relaxed) {
        0 => false,
        1 => true,
        _ => {
            let v = is_x86_feature_detected!("avx2");
            X86_AVX2_CACHED.store(if v { 1 } else { 0 }, Ordering::Relaxed);
            v
        }
    }
}

/// Compare `input[pos .. pos + lit.len()]` with `lit` and return whether
/// they are equal.  Never panics (bounds are checked before the comparison).
///
/// # Platform notes
/// * **`x86_64`**: uses AVX2 (32-byte chunks) or SSE2 (16-byte chunks).
/// * **other**: delegates to `<[u8]>::eq` which the compiler may vectorize.
#[must_use]
#[inline]
pub fn literal_eq(input: &[u8], pos: Pos, lit: &[u8]) -> bool {
    let n = lit.len();
    let off = pos as usize;
    let end = off + n;

    // Bounds check first — avoids any unsafe issues below.
    if end > input.len() {
        return false;
    }
    if n == 0 {
        return true;
    }

    let a = input.as_ptr();
    let b = lit.as_ptr();

    // --- x86_64 fast path ---------------------------------------------------
    #[cfg(target_arch = "x86_64")]
    {
        // `is_x86_feature_detected!` consults OS/cpu data each call — cache once per process.
        #[cfg(feature = "std")]
        let use_avx2 = n >= 32 && x86_cached_has_avx2();
        #[cfg(not(feature = "std"))]
        let use_avx2 = false;

        if use_avx2 {
            // SAFETY: bounds checked above; AVX2 verified at init.
            return unsafe { eq_avx2(a.add(off), b, n) };
        }
        if n >= 16 {
            // SSE2 is mandatory for all x86_64 targets (baseline ABI).
            // SAFETY: bounds checked above.
            return unsafe { eq_sse2(a.add(off), b, n) };
        }
    }

    // --- Generic fallback ---------------------------------------------------
    // The compiler lowers this to `memcmp`, which is well-optimised everywhere.
    input[off..end] == *lit
}

// ─── SSE2 (16-byte chunks) ────────────────────────────────────────────────────

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "sse2")]
#[allow(clippy::cast_ptr_alignment)] // _mm_loadu_si128 requires *const __m128i; load is unaligned-safe
#[inline]
unsafe fn load_u128(p: *const u8) -> core::arch::x86_64::__m128i {
    use core::arch::x86_64::_mm_loadu_si128;
    // Rust 2024 requires explicit unsafe blocks inside `unsafe fn`.
    unsafe { _mm_loadu_si128(p.cast()) }
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "sse2")]
unsafe fn eq_sse2(a: *const u8, b: *const u8, n: usize) -> bool {
    use core::arch::x86_64::{_mm_cmpeq_epi8, _mm_movemask_epi8};

    let mut i = 0usize;

    // Compare 16 bytes per iteration.
    while i + 16 <= n {
        let va = unsafe { load_u128(a.add(i)) };
        let vb = unsafe { load_u128(b.add(i)) };
        let eq = _mm_cmpeq_epi8(va, vb);
        if _mm_movemask_epi8(eq) != 0xFFFF {
            return false;
        }
        i += 16;
    }

    // Tail: up to 15 remaining bytes.
    unsafe { byte_eq_tail(a.add(i), b.add(i), n - i) }
}

// ─── AVX2 (32-byte chunks) ───────────────────────────────────────────────────

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
#[allow(clippy::cast_ptr_alignment)] // _mm256_loadu_si256 requires *const __m256i
#[inline]
unsafe fn load_u256(p: *const u8) -> core::arch::x86_64::__m256i {
    use core::arch::x86_64::_mm256_loadu_si256;
    // Rust 2024 requires explicit unsafe blocks inside `unsafe fn`.
    unsafe { _mm256_loadu_si256(p.cast()) }
}

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn eq_avx2(a: *const u8, b: *const u8, n: usize) -> bool {
    use core::arch::x86_64::{_mm256_cmpeq_epi8, _mm256_movemask_epi8};

    let mut i = 0usize;

    // Compare 32 bytes per iteration.
    while i + 32 <= n {
        let va = unsafe { load_u256(a.add(i)) };
        let vb = unsafe { load_u256(b.add(i)) };
        let eq = _mm256_cmpeq_epi8(va, vb);
        // movemask returns i32; all 32 bits set is -1.
        if _mm256_movemask_epi8(eq) != -1i32 {
            return false;
        }
        i += 32;
    }

    // Handle 16-byte residual (AVX2 implies SSE2).
    if i + 16 <= n {
        use core::arch::x86_64::{_mm_cmpeq_epi8, _mm_movemask_epi8};
        let va = unsafe { load_u128(a.add(i)) };
        let vb = unsafe { load_u128(b.add(i)) };
        let eq = _mm_cmpeq_epi8(va, vb);
        if _mm_movemask_epi8(eq) != 0xFFFF {
            return false;
        }
        i += 16;
    }

    // Final tail: up to 15 bytes.
    unsafe { byte_eq_tail(a.add(i), b.add(i), n - i) }
}

// ─── Scalar tail ─────────────────────────────────────────────────────────────

/// Compare `n` bytes starting at `a` and `b` with a simple loop.
/// Used for the tail portion that doesn't fill a full SIMD register.
#[inline]
unsafe fn byte_eq_tail(a: *const u8, b: *const u8, n: usize) -> bool {
    for i in 0..n {
        if unsafe { *a.add(i) } != unsafe { *b.add(i) } {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::literal_small_eq;

    #[test]
    fn literal_small_eq_lengths_and_alignment() {
        let data = b"0123456789abcdefghij";
        for n in 1..=8 {
            for off in 0..12 {
                if off + n > data.len() {
                    continue;
                }
                let mut b = [0u8; 8];
                b[..n].copy_from_slice(&data[off..off + n]);
                assert!(
                    literal_small_eq(data, off, n, &b),
                    "n={n} off={off} should match"
                );
            }
        }
        let mut b = [0u8; 8];
        b[..4].copy_from_slice(b"0123");
        assert!(literal_small_eq(data, 0, 0, &b));
        assert!(!literal_small_eq(data, 0, 9, &b));
        assert!(!literal_small_eq(data, 100, 1, &b));
        b[..4].copy_from_slice(b"xxxx");
        assert!(!literal_small_eq(data, 0, 4, &b));
    }
}
