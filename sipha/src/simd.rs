//! # SIMD-Accelerated Literal Comparison
//!
//! Provides a single entry-point [`literal_eq`] that:
//!
//! - Returns immediately if the input doesn't have enough bytes.
//! - For literals ≥ 32 bytes on AVX2 machines, compares 32 bytes per cycle.
//! - For literals ≥ 16 bytes on SSE2 machines (all `x86_64`), compares 16 bytes
//!   per cycle.
//! - Falls back to a plain slice comparison on other platforms, which the
//!   compiler already lowers to `memcmp` / SIMD via auto-vectorisation.
//!
//! ## Why bother?
//!
//! For short keywords like `"true"` (4 bytes) the compiler generates a single
//! 32-bit integer compare — no room for improvement.  For longer tokens such
//! as UUID strings, HTTP method names, or base64 chunks, the SIMD paths
//! deliver a measurable throughput gain.

use crate::types::Pos;

/// Compare `input[pos .. pos + lit.len()]` with `lit` and return whether
/// they are equal.  Never panics (bounds are checked before the comparison).
///
/// # Platform notes
/// * **`x86_64`**: uses AVX2 (32-byte chunks) or SSE2 (16-byte chunks).
/// * **other**: delegates to `<[u8]>::eq` which the compiler may vectorize.
#[must_use]
#[inline]
pub fn literal_eq(input: &[u8], pos: Pos, lit: &[u8]) -> bool {
    let n   = lit.len();
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
        if n >= 32 && is_x86_feature_detected!("avx2") {
            // SAFETY: bounds checked above; avx2 detected at runtime.
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
#[allow(clippy::cast_ptr_alignment)] // required: _mm_loadu_si128 takes *const __m128i
unsafe fn eq_sse2(a: *const u8, b: *const u8, n: usize) -> bool {
    use std::arch::x86_64::{
        __m128i, _mm_cmpeq_epi8, _mm_loadu_si128, _mm_movemask_epi8,
    };

    let mut i = 0usize;

    // Compare 16 bytes per iteration.
    while i + 16 <= n {
        let va = _mm_loadu_si128(a.add(i).cast::<__m128i>());
        let vb = _mm_loadu_si128(b.add(i).cast::<__m128i>());
        let eq = _mm_cmpeq_epi8(va, vb);
        if _mm_movemask_epi8(eq) != 0xFFFF {
            return false;
        }
        i += 16;
    }

    // Tail: up to 15 remaining bytes.
    byte_eq_tail(a.add(i), b.add(i), n - i)
}

// ─── AVX2 (32-byte chunks) ───────────────────────────────────────────────────

#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
#[allow(clippy::cast_ptr_alignment)] // required: load intrinsics take *const __m256i/__m128i
unsafe fn eq_avx2(a: *const u8, b: *const u8, n: usize) -> bool {
    use std::arch::x86_64::{
        __m256i, _mm256_cmpeq_epi8, _mm256_loadu_si256, _mm256_movemask_epi8,
    };

    let mut i = 0usize;

    // Compare 32 bytes per iteration.
    while i + 32 <= n {
        let va = _mm256_loadu_si256(a.add(i).cast::<__m256i>());
        let vb = _mm256_loadu_si256(b.add(i).cast::<__m256i>());
        let eq = _mm256_cmpeq_epi8(va, vb);
        // movemask returns i32; all 32 bits set is -1.
        if _mm256_movemask_epi8(eq) != -1i32 {
            return false;
        }
        i += 32;
    }

    // Handle 16-byte residual (AVX2 implies SSE2).
    if i + 16 <= n {
        use std::arch::x86_64::{
            __m128i, _mm_cmpeq_epi8, _mm_loadu_si128, _mm_movemask_epi8,
        };
        let va = _mm_loadu_si128(a.add(i).cast::<__m128i>());
        let vb = _mm_loadu_si128(b.add(i).cast::<__m128i>());
        let eq = _mm_cmpeq_epi8(va, vb);
        if _mm_movemask_epi8(eq) != 0xFFFF {
            return false;
        }
        i += 16;
    }

    // Final tail: up to 15 bytes.
    byte_eq_tail(a.add(i), b.add(i), n - i)
}

// ─── Scalar tail ─────────────────────────────────────────────────────────────

/// Compare `n` bytes starting at `a` and `b` with a simple loop.
/// Used for the tail portion that doesn't fill a full SIMD register.
#[inline]
unsafe fn byte_eq_tail(a: *const u8, b: *const u8, n: usize) -> bool {
    for i in 0..n {
        if *a.add(i) != *b.add(i) {
            return false;
        }
    }
    true
}
