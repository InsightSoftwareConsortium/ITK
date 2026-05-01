// This file is part of Eigen, a lightweight C template library
// for linear algebra.
//
// Copyright (C) 2024 Kseniya Zaytseva <kseniya.zaytseva@syntacore.com>
// Copyright (C) 2025 Chip Kerchner <ckerchner@tenstorrent.com>
//
// This Source Code Form is subject to the terms of the Mozilla
// Public License v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef EIGEN_RVV10_GENERAL_BLOCK_KERNEL_H
#define EIGEN_RVV10_GENERAL_BLOCK_KERNEL_H

// IWYU pragma: private
#include "../../InternalHeaderCheck.h"

namespace Eigen {
namespace internal {

/********************************* real ************************************/

template <>
struct gebp_traits<float, float, false, false, Architecture::RVV10, GEBPPacketFull>
    : gebp_traits<float, float, false, false, Architecture::Generic, GEBPPacketFull> {
  typedef float RhsPacket;
  typedef QuadPacket<float> RhsPacketx4;
  EIGEN_STRONG_INLINE void loadRhs(const RhsScalar* b, RhsPacket& dest) const { dest = pset1<RhsPacket>(*b); }
  EIGEN_STRONG_INLINE void loadRhs(const RhsScalar* b, RhsPacketx4& dest) const {
    pbroadcast4(b, dest.B_0, dest.B1, dest.B2, dest.B3);
  }

  EIGEN_STRONG_INLINE void updateRhs(const RhsScalar* b, RhsPacket& dest) const { loadRhs(b, dest); }

  EIGEN_STRONG_INLINE void updateRhs(const RhsScalar*, RhsPacketx4&) const {}

  EIGEN_STRONG_INLINE void loadRhsQuad(const RhsScalar* b, RhsPacket& dest) const { dest = ploadquad<RhsPacket>(b); }

  EIGEN_STRONG_INLINE void madd(const LhsPacket& a, const RhsPacket& b, AccPacket& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
#if EIGEN_RISCV64_DEFAULT_LMUL == 1
    c = __riscv_vfmadd_vf_f32m1(a, b, c, unpacket_traits<AccPacket>::size);
#elif EIGEN_RISCV64_DEFAULT_LMUL == 2
    c = __riscv_vfmadd_vf_f32m2(a, b, c, unpacket_traits<AccPacket>::size);
#elif EIGEN_RISCV64_DEFAULT_LMUL == 4
    c = __riscv_vfmadd_vf_f32m4(a, b, c, unpacket_traits<AccPacket>::size);
#endif
  }

#if EIGEN_RISCV64_DEFAULT_LMUL >= 2
  EIGEN_STRONG_INLINE void madd(const Packet1Xf& a, const RhsPacket& b, Packet1Xf& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
    c = __riscv_vfmadd_vf_f32m1(a, b, c, unpacket_traits<Packet1Xf>::size);
  }
#endif
#if EIGEN_RISCV64_DEFAULT_LMUL == 4
  EIGEN_STRONG_INLINE void madd(const Packet2Xf& a, const RhsPacket& b, Packet2Xf& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
    c = __riscv_vfmadd_vf_f32m2(a, b, c, unpacket_traits<Packet2Xf>::size);
  }
#endif

  template <typename LaneIdType>
  EIGEN_STRONG_INLINE void madd(const LhsPacket& a, const RhsPacketx4& b, AccPacket& c, RhsPacket& /*tmp*/,
                                const LaneIdType& lane) const {
#if EIGEN_RISCV64_DEFAULT_LMUL == 1
    c = __riscv_vfmadd_vf_f32m1(a, b.get(lane), c, unpacket_traits<AccPacket>::size);
#elif EIGEN_RISCV64_DEFAULT_LMUL == 2
    c = __riscv_vfmadd_vf_f32m2(a, b.get(lane), c, unpacket_traits<AccPacket>::size);
#elif EIGEN_RISCV64_DEFAULT_LMUL == 4
    c = __riscv_vfmadd_vf_f32m4(a, b.get(lane), c, unpacket_traits<AccPacket>::size);
#endif
  }
};

template <>
struct gebp_traits<double, double, false, false, Architecture::RVV10, GEBPPacketFull>
    : gebp_traits<double, double, false, false, Architecture::Generic, GEBPPacketFull> {
  typedef double RhsPacket;
  typedef QuadPacket<double> RhsPacketx4;
  EIGEN_STRONG_INLINE void loadRhs(const RhsScalar* b, RhsPacket& dest) const { dest = pset1<RhsPacket>(*b); }
  EIGEN_STRONG_INLINE void loadRhs(const RhsScalar* b, RhsPacketx4& dest) const {
    pbroadcast4(b, dest.B_0, dest.B1, dest.B2, dest.B3);
  }

  EIGEN_STRONG_INLINE void updateRhs(const RhsScalar* b, RhsPacket& dest) const { loadRhs(b, dest); }

  EIGEN_STRONG_INLINE void updateRhs(const RhsScalar*, RhsPacketx4&) const {}

  EIGEN_STRONG_INLINE void loadRhsQuad(const RhsScalar* b, RhsPacket& dest) const { dest = ploadquad<RhsPacket>(b); }

  EIGEN_STRONG_INLINE void madd(const LhsPacket& a, const RhsPacket& b, AccPacket& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
#if EIGEN_RISCV64_DEFAULT_LMUL == 1
    c = __riscv_vfmadd_vf_f64m1(a, b, c, unpacket_traits<AccPacket>::size);
#elif EIGEN_RISCV64_DEFAULT_LMUL == 2
    c = __riscv_vfmadd_vf_f64m2(a, b, c, unpacket_traits<AccPacket>::size);
#elif EIGEN_RISCV64_DEFAULT_LMUL == 4
    c = __riscv_vfmadd_vf_f64m4(a, b, c, unpacket_traits<AccPacket>::size);
#endif
  }

#if EIGEN_RISCV64_DEFAULT_LMUL >= 2
  EIGEN_STRONG_INLINE void madd(const Packet1Xd& a, const RhsPacket& b, Packet1Xd& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
    c = __riscv_vfmadd_vf_f64m1(a, b, c, unpacket_traits<Packet1Xd>::size);
  }
#endif
#if EIGEN_RISCV64_DEFAULT_LMUL == 4
  EIGEN_STRONG_INLINE void madd(const Packet2Xd& a, const RhsPacket& b, Packet2Xd& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
    c = __riscv_vfmadd_vf_f64m2(a, b, c, unpacket_traits<Packet2Xd>::size);
  }
#endif

  template <typename LaneIdType>
  EIGEN_STRONG_INLINE void madd(const LhsPacket& a, const RhsPacketx4& b, AccPacket& c, RhsPacket& /*tmp*/,
                                const LaneIdType& lane) const {
#if EIGEN_RISCV64_DEFAULT_LMUL == 1
    c = __riscv_vfmadd_vf_f64m1(a, b.get(lane), c, unpacket_traits<AccPacket>::size);
#elif EIGEN_RISCV64_DEFAULT_LMUL == 2
    c = __riscv_vfmadd_vf_f64m2(a, b.get(lane), c, unpacket_traits<AccPacket>::size);
#elif EIGEN_RISCV64_DEFAULT_LMUL == 4
    c = __riscv_vfmadd_vf_f64m4(a, b.get(lane), c, unpacket_traits<AccPacket>::size);
#endif
  }
};

#if defined(EIGEN_VECTORIZE_RVV10FP16)

template <>
struct gebp_traits<half, half, false, false, Architecture::RVV10>
    : gebp_traits<half, half, false, false, Architecture::Generic> {
  typedef half RhsPacket;
  typedef PacketXh LhsPacket;
  typedef PacketXh AccPacket;
  typedef QuadPacket<half> RhsPacketx4;

  EIGEN_STRONG_INLINE void loadRhs(const RhsScalar* b, RhsPacket& dest) const { dest = pset1<RhsPacket>(*b); }
  EIGEN_STRONG_INLINE void loadRhs(const RhsScalar* b, RhsPacketx4& dest) const {
    pbroadcast4(b, dest.B_0, dest.B1, dest.B2, dest.B3);
  }

  EIGEN_STRONG_INLINE void updateRhs(const RhsScalar* b, RhsPacket& dest) const { loadRhs(b, dest); }

  EIGEN_STRONG_INLINE void updateRhs(const RhsScalar*, RhsPacketx4&) const {}

  EIGEN_STRONG_INLINE void loadRhsQuad(const RhsScalar* b, RhsPacket& dest) const { dest = pload<RhsPacket>(b); }

  EIGEN_STRONG_INLINE void madd(const LhsPacket& a, const RhsPacket& b, AccPacket& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
#if EIGEN_RISCV64_DEFAULT_LMUL == 1
    c = __riscv_vfmadd_vf_f16m1(a, numext::bit_cast<_Float16>(b), c, unpacket_traits<AccPacket>::size);
#else
    c = __riscv_vfmadd_vf_f16m2(a, numext::bit_cast<_Float16>(b), c, unpacket_traits<AccPacket>::size);
#endif
  }

#if EIGEN_RISCV64_DEFAULT_LMUL >= 2
  EIGEN_STRONG_INLINE void madd(const Packet1Xh& a, const RhsPacket& b, Packet1Xh& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
    c = __riscv_vfmadd_vf_f16m1(a, numext::bit_cast<_Float16>(b), c, unpacket_traits<Packet1Xh>::size);
  }
#endif

  template <typename LaneIdType>
  EIGEN_STRONG_INLINE void madd(const LhsPacket& a, const RhsPacketx4& b, AccPacket& c, RhsPacket& /*tmp*/,
                                const LaneIdType& lane) const {
#if EIGEN_RISCV64_DEFAULT_LMUL == 1
    c = __riscv_vfmadd_vf_f16m1(a, numext::bit_cast<_Float16>(b.get(lane)), c, unpacket_traits<AccPacket>::size);
#else
    c = __riscv_vfmadd_vf_f16m2(a, numext::bit_cast<_Float16>(b.get(lane)), c, unpacket_traits<AccPacket>::size);
#endif
  }
};

#endif

#if defined(EIGEN_VECTORIZE_RVV10BF16)

template <>
struct gebp_traits<bfloat16, bfloat16, false, false, Architecture::RVV10>
    : gebp_traits<bfloat16, bfloat16, false, false, Architecture::Generic> {
  typedef bfloat16 RhsPacket;
  typedef PacketXbf LhsPacket;
  typedef PacketXbf AccPacket;
  typedef QuadPacket<bfloat16> RhsPacketx4;

  EIGEN_STRONG_INLINE void loadRhs(const RhsScalar* b, RhsPacket& dest) const { dest = pset1<RhsPacket>(*b); }
  EIGEN_STRONG_INLINE void loadRhs(const RhsScalar* b, RhsPacketx4& dest) const {
    pbroadcast4(b, dest.B_0, dest.B1, dest.B2, dest.B3);
  }

  EIGEN_STRONG_INLINE void updateRhs(const RhsScalar* b, RhsPacket& dest) const { loadRhs(b, dest); }

  EIGEN_STRONG_INLINE void updateRhs(const RhsScalar*, RhsPacketx4&) const {}

  EIGEN_STRONG_INLINE void loadRhsQuad(const RhsScalar* b, RhsPacket& dest) const { dest = pload<RhsPacket>(b); }

  EIGEN_STRONG_INLINE void madd(const LhsPacket& a, const RhsPacket& b, AccPacket& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
#if EIGEN_RISCV64_DEFAULT_LMUL == 1
    c = F32ToBf16(
        __riscv_vfwmaccbf16_vf_f32m2(Bf16ToF32(c), numext::bit_cast<__bf16>(b), a, unpacket_traits<AccPacket>::size));
#else
    c = F32ToBf16(
        __riscv_vfwmaccbf16_vf_f32m4(Bf16ToF32(c), numext::bit_cast<__bf16>(b), a, unpacket_traits<AccPacket>::size));
#endif
  }

#if EIGEN_RISCV64_DEFAULT_LMUL >= 2
  EIGEN_STRONG_INLINE void madd(const Packet1Xbf& a, const RhsPacket& b, Packet1Xbf& c, RhsPacket& /*tmp*/,
                                const FixedInt<0>&) const {
    c = F32ToBf16(
        __riscv_vfwmaccbf16_vf_f32m2(Bf16ToF32(c), numext::bit_cast<__bf16>(b), a, unpacket_traits<Packet1Xbf>::size));
  }
#endif

  template <typename LaneIdType>
  EIGEN_STRONG_INLINE void madd(const LhsPacket& a, const RhsPacketx4& b, AccPacket& c, RhsPacket& /*tmp*/,
                                const LaneIdType& lane) const {
#if EIGEN_RISCV64_DEFAULT_LMUL == 1
    c = F32ToBf16(__riscv_vfwmaccbf16_vf_f32m2(Bf16ToF32(c), numext::bit_cast<__bf16>(b.get(lane)), a,
                                               unpacket_traits<AccPacket>::size));
#else
    c = F32ToBf16(__riscv_vfwmaccbf16_vf_f32m4(Bf16ToF32(c), numext::bit_cast<__bf16>(b.get(lane)), a,
                                               unpacket_traits<AccPacket>::size));
#endif
  }
};

#endif

}  // namespace internal
}  // namespace Eigen

#endif  // EIGEN_RVV10_GENERAL_BLOCK_KERNEL_H
