// This file is part of Eigen, a lightweight C++ template library
// for linear algebra.
//
// Copyright (C) 2015 Benoit Steiner <benoit.steiner.goog@gmail.com>
//
// This Source Code Form is subject to the terms of the Mozilla
// Public License v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef EIGEN_TYPE_CASTING_AVX_H
#define EIGEN_TYPE_CASTING_AVX_H

// IWYU pragma: private
#include "../../InternalHeaderCheck.h"

namespace Eigen {

namespace internal {

#ifndef EIGEN_VECTORIZE_AVX512
template<> struct type_casting_traits<float, bool> : vectorized_type_casting_traits<float, bool> {};
template<> struct type_casting_traits<bool, float> : vectorized_type_casting_traits<bool, float> {};

template<> struct type_casting_traits<float, int> : vectorized_type_casting_traits<float, int> {};
template<> struct type_casting_traits<int, float> : vectorized_type_casting_traits<int, float> {};

template<> struct type_casting_traits<float, double> : vectorized_type_casting_traits<float, double> {};
template<> struct type_casting_traits<double, float> : vectorized_type_casting_traits<double, float> {};

template<> struct type_casting_traits<double, int> : vectorized_type_casting_traits<double, int> {};
template<> struct type_casting_traits<int, double> : vectorized_type_casting_traits<int, double> {};

template<> struct type_casting_traits<half, float> : vectorized_type_casting_traits<half, float> {};
template<> struct type_casting_traits<float, half> : vectorized_type_casting_traits<float, half> {};

template<> struct type_casting_traits<bfloat16, float> : vectorized_type_casting_traits<bfloat16, float> {};
template<> struct type_casting_traits<float, bfloat16> : vectorized_type_casting_traits<float, bfloat16> {};
#endif

template <>
EIGEN_STRONG_INLINE Packet16b pcast<Packet8f, Packet16b>(const Packet8f& a,
                                                         const Packet8f& b) {
  __m256 nonzero_a = _mm256_cmp_ps(a, pzero(a), _CMP_NEQ_UQ);
  __m256 nonzero_b = _mm256_cmp_ps(b, pzero(b), _CMP_NEQ_UQ);
  constexpr char kFF = '\255';
#ifndef EIGEN_VECTORIZE_AVX2
  __m128i shuffle_mask128_a_lo = _mm_set_epi8(kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, 12, 8, 4, 0);
  __m128i shuffle_mask128_a_hi = _mm_set_epi8(kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, 12, 8, 4, 0, kFF, kFF, kFF, kFF);
  __m128i shuffle_mask128_b_lo = _mm_set_epi8(kFF, kFF, kFF, kFF, 12, 8, 4, 0, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF);
  __m128i shuffle_mask128_b_hi = _mm_set_epi8(12, 8, 4, 0, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF);
  __m128i a_hi = _mm_shuffle_epi8(_mm256_extractf128_si256(_mm256_castps_si256(nonzero_a), 1), shuffle_mask128_a_hi);
  __m128i a_lo = _mm_shuffle_epi8(_mm256_extractf128_si256(_mm256_castps_si256(nonzero_a), 0), shuffle_mask128_a_lo);
  __m128i b_hi = _mm_shuffle_epi8(_mm256_extractf128_si256(_mm256_castps_si256(nonzero_b), 1), shuffle_mask128_b_hi);
  __m128i b_lo = _mm_shuffle_epi8(_mm256_extractf128_si256(_mm256_castps_si256(nonzero_b), 0), shuffle_mask128_b_lo);
  __m128i merged = _mm_or_si128(_mm_or_si128(b_lo, b_hi), _mm_or_si128(a_lo, a_hi));
  return _mm_and_si128(merged, _mm_set1_epi8(1));
 #else
  __m256i a_shuffle_mask = _mm256_set_epi8(kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF,  12,   8,   4,   0, kFF, kFF, kFF, kFF,
                                           kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF,  12,   8,   4,   0);
  __m256i b_shuffle_mask = _mm256_set_epi8( 12,   8,   4,   0, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF,
                                           kFF, kFF, kFF, kFF,  12,   8,   4,   0, kFF, kFF, kFF, kFF, kFF, kFF, kFF, kFF);
  __m256i a_shuff = _mm256_shuffle_epi8(_mm256_castps_si256(nonzero_a), a_shuffle_mask);
  __m256i b_shuff = _mm256_shuffle_epi8(_mm256_castps_si256(nonzero_b), b_shuffle_mask);
  __m256i a_or_b = _mm256_or_si256(a_shuff, b_shuff);
  __m256i merged = _mm256_or_si256(a_or_b, _mm256_castsi128_si256(_mm256_extractf128_si256(a_or_b, 1)));
  return _mm256_castsi256_si128(_mm256_and_si256(merged, _mm256_set1_epi8(1)));
#endif
}

template <>
EIGEN_STRONG_INLINE Packet8f pcast<Packet16b, Packet8f>(const Packet16b& a) {
  const __m256 cst_one = _mm256_set1_ps(1.0f);
  #ifdef EIGEN_VECTORIZE_AVX2
  __m256i a_extended = _mm256_cvtepi8_epi32(a);
  __m256i abcd_efgh = _mm256_cmpeq_epi32(a_extended, _mm256_setzero_si256());
  #else
  __m128i abcd_efhg_ijkl_mnop = _mm_cmpeq_epi8(a, _mm_setzero_si128());
  __m128i aabb_ccdd_eeff_gghh = _mm_unpacklo_epi8(abcd_efhg_ijkl_mnop, abcd_efhg_ijkl_mnop);
  __m128i aaaa_bbbb_cccc_dddd = _mm_unpacklo_epi8(aabb_ccdd_eeff_gghh, aabb_ccdd_eeff_gghh);
  __m128i eeee_ffff_gggg_hhhh = _mm_unpackhi_epi8(aabb_ccdd_eeff_gghh, aabb_ccdd_eeff_gghh);
  __m256i abcd_efgh = _mm256_setr_m128i(aaaa_bbbb_cccc_dddd, eeee_ffff_gggg_hhhh);
  #endif
  __m256 result = _mm256_andnot_ps(_mm256_castsi256_ps(abcd_efgh), cst_one);
  return result;
}

template<> EIGEN_STRONG_INLINE Packet8i pcast<Packet8f, Packet8i>(const Packet8f& a) {
  return _mm256_cvttps_epi32(a);
}

template<> EIGEN_STRONG_INLINE Packet8i pcast<Packet4d, Packet8i>(const Packet4d& a, const Packet4d& b) {
  return _mm256_set_m128i(_mm256_cvttpd_epi32(b), _mm256_cvttpd_epi32(a));
}

template <> EIGEN_STRONG_INLINE Packet4i pcast<Packet4d, Packet4i>(const Packet4d& a) {
  return _mm256_cvttpd_epi32(a);
}

template<> EIGEN_STRONG_INLINE Packet8f pcast<Packet8i, Packet8f>(const Packet8i& a) {
  return _mm256_cvtepi32_ps(a);
}

template<> EIGEN_STRONG_INLINE Packet8f pcast<Packet4d, Packet8f>(const Packet4d& a, const Packet4d& b) {
  return _mm256_set_m128(_mm256_cvtpd_ps(b), _mm256_cvtpd_ps(a));
}

template <> EIGEN_STRONG_INLINE Packet4f pcast<Packet4d, Packet4f>(const Packet4d& a) {
  return _mm256_cvtpd_ps(a);
}

template <> EIGEN_STRONG_INLINE Packet4d pcast<Packet8i, Packet4d>(const Packet8i& a) {
  return _mm256_cvtepi32_pd(_mm256_castsi256_si128(a));
}

template <> EIGEN_STRONG_INLINE Packet4d pcast<Packet4i, Packet4d>(const Packet4i& a) {
  return _mm256_cvtepi32_pd(a);
}

template <> EIGEN_STRONG_INLINE Packet4d pcast<Packet8f, Packet4d>(const Packet8f& a) {
  return _mm256_cvtps_pd(_mm256_castps256_ps128(a));
}

template <> EIGEN_STRONG_INLINE Packet4d pcast<Packet4f, Packet4d>(const Packet4f& a) {
  return _mm256_cvtps_pd(a);
}

template<> EIGEN_STRONG_INLINE Packet8i preinterpret<Packet8i,Packet8f>(const Packet8f& a) {
  return _mm256_castps_si256(a);
}

template<> EIGEN_STRONG_INLINE Packet8f preinterpret<Packet8f,Packet8i>(const Packet8i& a) {
  return _mm256_castsi256_ps(a);
}

template<> EIGEN_STRONG_INLINE Packet8ui preinterpret<Packet8ui, Packet8i>(const Packet8i& a) {
  return Packet8ui(a);
}

template<> EIGEN_STRONG_INLINE Packet8i preinterpret<Packet8i, Packet8ui>(const Packet8ui& a) {
  return Packet8i(a);
}

// truncation operations

template<> EIGEN_STRONG_INLINE Packet4f preinterpret<Packet4f, Packet8f>(const Packet8f& a) {
  return _mm256_castps256_ps128(a);
}

template<> EIGEN_STRONG_INLINE Packet2d preinterpret<Packet2d, Packet4d>(const Packet4d& a) {
  return _mm256_castpd256_pd128(a);
}

template<> EIGEN_STRONG_INLINE Packet4i preinterpret<Packet4i, Packet8i>(const Packet8i& a) {
  return _mm256_castsi256_si128(a);
}

template<> EIGEN_STRONG_INLINE Packet4ui preinterpret<Packet4ui, Packet8ui>(const Packet8ui& a) {
  return _mm256_castsi256_si128(a);
}


#ifdef EIGEN_VECTORIZE_AVX2
template<> EIGEN_STRONG_INLINE Packet4ul preinterpret<Packet4ul, Packet4l>(const Packet4l& a) {
  return Packet4ul(a);
}

template<> EIGEN_STRONG_INLINE Packet4l preinterpret<Packet4l, Packet4ul>(const Packet4ul& a) {
  return Packet4l(a);
}

#endif

template<> EIGEN_STRONG_INLINE Packet8f pcast<Packet8h, Packet8f>(const Packet8h& a) {
  return half2float(a);
}

template<> EIGEN_STRONG_INLINE Packet8f pcast<Packet8bf, Packet8f>(const Packet8bf& a) {
  return Bf16ToF32(a);
}

template<> EIGEN_STRONG_INLINE Packet8h pcast<Packet8f, Packet8h>(const Packet8f& a) {
  return float2half(a);
}

template<> EIGEN_STRONG_INLINE Packet8bf pcast<Packet8f, Packet8bf>(const Packet8f& a) {
  return F32ToBf16(a);
}

} // end namespace internal

} // end namespace Eigen

#endif // EIGEN_TYPE_CASTING_AVX_H
