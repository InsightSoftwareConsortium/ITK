
#ifndef EIGEN_HVX_PACKET_MATH_H
#define EIGEN_HVX_PACKET_MATH_H

// Only support 128B HVX now.
// Floating-point operations are supported only since V68.
#if defined __HVX__ && (__HVX_LENGTH__ == 128) && __HVX_ARCH__ >= 68

// All the floating-point operations do not support IEEE standard.
// From HVX document:
//   There is no concept of infinity or NaN. QFloat saturates to maximum
//   exponent with maximum positive or minimum negative significand.

#ifndef EIGEN_ARCH_DEFAULT_NUMBER_OF_REGISTERS
#define EIGEN_ARCH_DEFAULT_NUMBER_OF_REGISTERS 32
#endif

namespace Eigen {
namespace internal {

EIGEN_STRONG_INLINE HVX_Vector HVX_load(const void* mem) {
  return *((HVX_Vector*)mem);
}

EIGEN_STRONG_INLINE HVX_Vector HVX_loadu(const void* mem) {
  return *((HVX_UVector*)mem);
}

EIGEN_STRONG_INLINE void HVX_store(void* mem, HVX_Vector v) {
  *((HVX_Vector*)mem) = v;
}

EIGEN_STRONG_INLINE void HVX_storeu(void* mem, HVX_Vector v) {
  *((HVX_UVector*)mem) = v;
}

// Hexagon compiler uses same HVX_Vector to represent all HVX vector types.
// Wrap different vector type (float32, int32, etc) to different class with
// explicit constructor and casting back-and-force to HVX_Vector.
template <int unique_id>
class HVXPacket {
 public:
  HVXPacket() = default;
  static HVXPacket Create(HVX_Vector v) { return HVXPacket(v); }
  HVX_Vector Get() const { return m_val; }

 private:
  explicit HVXPacket(HVX_Vector v) : m_val(v) {}
  HVX_Vector m_val = Q6_V_vzero();
};

typedef HVXPacket<0> Packet32f;   // float32
typedef HVXPacket<1> Packet32qf;  // qfloat32

template <>
struct packet_traits<float> : default_packet_traits {
  typedef Packet32f type;
  typedef Packet32f half;
  enum {
    Vectorizable = 1,
    AlignedOnScalar = 1,
    size = 32,
  };
};

template <>
struct unpacket_traits<Packet32f> {
  typedef float type;
  typedef Packet32f half;
  enum {
    size = 32,
    alignment = Aligned128,
    vectorizable = true,
    masked_load_available = false,
    masked_store_available = false
  };
};

// float32 operations.
template <>
EIGEN_STRONG_INLINE Packet32f pset1<Packet32f>(const float& from) {
  union {
    float f;
    int32_t i;
  } u;
  u.f = from;
  return Packet32f::Create(Q6_V_vsplat_R(u.i));
}

template <>
EIGEN_STRONG_INLINE Packet32f pload<Packet32f>(const float* from) {
  return Packet32f::Create(HVX_load(from));
}
template <>
EIGEN_STRONG_INLINE Packet32f ploadu<Packet32f>(const float* from) {
  return Packet32f::Create(HVX_loadu(from));
}

template <>
EIGEN_STRONG_INLINE void pstore<float>(float* to, const Packet32f& from) {
  HVX_store(to, from.Get());
}
template <>
EIGEN_STRONG_INLINE void pstoreu<float>(float* to, const Packet32f& from) {
  HVX_storeu(to, from.Get());
}

template <>
EIGEN_STRONG_INLINE Packet32f pmul<Packet32f>(const Packet32f& a,
                                              const Packet32f& b) {
  return Packet32f::Create(
      Q6_Vsf_equals_Vqf32(Q6_Vqf32_vmpy_VsfVsf(a.Get(), b.Get())));
}

template <>
EIGEN_STRONG_INLINE Packet32f padd<Packet32f>(const Packet32f& a,
                                              const Packet32f& b) {
  return Packet32f::Create(
      Q6_Vsf_equals_Vqf32(Q6_Vqf32_vadd_VsfVsf(a.Get(), b.Get())));
}

template <>
EIGEN_STRONG_INLINE Packet32f psub<Packet32f>(const Packet32f& a,
                                              const Packet32f& b) {
  return Packet32f::Create(
      Q6_Vsf_equals_Vqf32(Q6_Vqf32_vsub_VsfVsf(a.Get(), b.Get())));
}

template <>
EIGEN_STRONG_INLINE Packet32f pnegate(const Packet32f& a) {
  return psub(Packet32f::Create(Q6_V_vzero()), a);
}

template <>
EIGEN_STRONG_INLINE Packet32f pcmp_le(const Packet32f& a, const Packet32f& b) {
  HVX_Vector v_true = Q6_Vb_vsplat_R(0xff);
  HVX_VectorPred pred = Q6_Q_vcmp_gt_VsfVsf(a.Get(), b.Get());
  return Packet32f::Create(Q6_V_vmux_QVV(pred, Q6_V_vzero(), v_true));
}

template <>
EIGEN_STRONG_INLINE Packet32f pcmp_eq(const Packet32f& a, const Packet32f& b) {
  HVX_Vector v_true = Q6_Vb_vsplat_R(0xff);
  HVX_VectorPred pred = Q6_Q_vcmp_eq_VwVw(a.Get(), b.Get());
  return Packet32f::Create(Q6_V_vmux_QVV(pred, v_true, Q6_V_vzero()));
}

template <>
EIGEN_STRONG_INLINE Packet32f pcmp_lt(const Packet32f& a, const Packet32f& b) {
  HVX_Vector v_true = Q6_Vb_vsplat_R(0xff);
  HVX_VectorPred pred = Q6_Q_vcmp_gt_VsfVsf(b.Get(), a.Get());
  return Packet32f::Create(Q6_V_vmux_QVV(pred, v_true, Q6_V_vzero()));
}

template <>
EIGEN_STRONG_INLINE Packet32f pcmp_lt_or_nan(const Packet32f& a,
                                             const Packet32f& b) {
  HVX_Vector v_true = Q6_Vb_vsplat_R(0xff);
  HVX_VectorPred pred = Q6_Q_vcmp_gt_VsfVsf(b.Get(), a.Get());
  return Packet32f::Create(Q6_V_vmux_QVV(pred, v_true, Q6_V_vzero()));
}

template <>
EIGEN_STRONG_INLINE Packet32f pabs(const Packet32f& a) {
  HVX_VectorPred pred = Q6_Q_vcmp_gt_VsfVsf(a.Get(), Q6_V_vzero());
  return Packet32f::Create(Q6_V_vmux_QVV(pred, a.Get(), pnegate(a).Get()));
}

template <>
EIGEN_STRONG_INLINE float pfirst(const Packet32f& a) {
  float vsf[32] __attribute__((aligned(128)));
  pstore(vsf, a);
  return vsf[0];
}

EIGEN_STRONG_INLINE void ptranspose(PacketBlock<Packet32f, 4>& kernel) {
  // Shuffle the 32-bit lanes.
  HVX_VectorPair v_0_1_0 =
      Q6_W_vshuff_VVR(kernel.packet[1].Get(), kernel.packet[0].Get(), -4);
  HVX_VectorPair v_0_3_2 =
      Q6_W_vshuff_VVR(kernel.packet[3].Get(), kernel.packet[2].Get(), -4);

  // Shuffle the 64-bit lanes.
  HVX_VectorPair v_1_1_0 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_3_2),
                                           HEXAGON_HVX_GET_V0(v_0_1_0), -8);
  HVX_VectorPair v_1_3_2 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_3_2),
                                           HEXAGON_HVX_GET_V1(v_0_1_0), -8);

  kernel.packet[0] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_1_1_0));
  kernel.packet[1] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_1_1_0));
  kernel.packet[2] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_1_3_2));
  kernel.packet[3] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_1_3_2));
}

EIGEN_STRONG_INLINE void ptranspose(PacketBlock<Packet32f, 32>& kernel) {
  // Shuffle the 32-bit lanes.
  HVX_VectorPair v_0_1_0 =
      Q6_W_vshuff_VVR(kernel.packet[1].Get(), kernel.packet[0].Get(), -4);
  HVX_VectorPair v_0_3_2 =
      Q6_W_vshuff_VVR(kernel.packet[3].Get(), kernel.packet[2].Get(), -4);
  HVX_VectorPair v_0_5_4 =
      Q6_W_vshuff_VVR(kernel.packet[5].Get(), kernel.packet[4].Get(), -4);
  HVX_VectorPair v_0_7_6 =
      Q6_W_vshuff_VVR(kernel.packet[7].Get(), kernel.packet[6].Get(), -4);
  HVX_VectorPair v_0_9_8 =
      Q6_W_vshuff_VVR(kernel.packet[9].Get(), kernel.packet[8].Get(), -4);
  HVX_VectorPair v_0_11_10 =
      Q6_W_vshuff_VVR(kernel.packet[11].Get(), kernel.packet[10].Get(), -4);
  HVX_VectorPair v_0_13_12 =
      Q6_W_vshuff_VVR(kernel.packet[13].Get(), kernel.packet[12].Get(), -4);
  HVX_VectorPair v_0_15_14 =
      Q6_W_vshuff_VVR(kernel.packet[15].Get(), kernel.packet[14].Get(), -4);
  HVX_VectorPair v_0_17_16 =
      Q6_W_vshuff_VVR(kernel.packet[17].Get(), kernel.packet[16].Get(), -4);
  HVX_VectorPair v_0_19_18 =
      Q6_W_vshuff_VVR(kernel.packet[19].Get(), kernel.packet[18].Get(), -4);
  HVX_VectorPair v_0_21_20 =
      Q6_W_vshuff_VVR(kernel.packet[21].Get(), kernel.packet[20].Get(), -4);
  HVX_VectorPair v_0_23_22 =
      Q6_W_vshuff_VVR(kernel.packet[23].Get(), kernel.packet[22].Get(), -4);
  HVX_VectorPair v_0_25_24 =
      Q6_W_vshuff_VVR(kernel.packet[25].Get(), kernel.packet[24].Get(), -4);
  HVX_VectorPair v_0_27_26 =
      Q6_W_vshuff_VVR(kernel.packet[27].Get(), kernel.packet[26].Get(), -4);
  HVX_VectorPair v_0_29_28 =
      Q6_W_vshuff_VVR(kernel.packet[29].Get(), kernel.packet[28].Get(), -4);
  HVX_VectorPair v_0_31_30 =
      Q6_W_vshuff_VVR(kernel.packet[31].Get(), kernel.packet[30].Get(), -4);

  // Shuffle the 64-bit lanes.
  HVX_VectorPair v_1_1_0 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_3_2),
                                           HEXAGON_HVX_GET_V0(v_0_1_0), -8);
  HVX_VectorPair v_1_3_2 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_3_2),
                                           HEXAGON_HVX_GET_V1(v_0_1_0), -8);
  HVX_VectorPair v_1_5_4 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_7_6),
                                           HEXAGON_HVX_GET_V0(v_0_5_4), -8);
  HVX_VectorPair v_1_7_6 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_7_6),
                                           HEXAGON_HVX_GET_V1(v_0_5_4), -8);
  HVX_VectorPair v_1_9_8 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_11_10),
                                           HEXAGON_HVX_GET_V0(v_0_9_8), -8);
  HVX_VectorPair v_1_11_10 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_11_10),
                                             HEXAGON_HVX_GET_V1(v_0_9_8), -8);
  HVX_VectorPair v_1_13_12 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_15_14),
                                             HEXAGON_HVX_GET_V0(v_0_13_12), -8);
  HVX_VectorPair v_1_15_14 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_15_14),
                                             HEXAGON_HVX_GET_V1(v_0_13_12), -8);
  HVX_VectorPair v_1_17_16 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_19_18),
                                             HEXAGON_HVX_GET_V0(v_0_17_16), -8);
  HVX_VectorPair v_1_19_18 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_19_18),
                                             HEXAGON_HVX_GET_V1(v_0_17_16), -8);
  HVX_VectorPair v_1_21_20 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_23_22),
                                             HEXAGON_HVX_GET_V0(v_0_21_20), -8);
  HVX_VectorPair v_1_23_22 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_23_22),
                                             HEXAGON_HVX_GET_V1(v_0_21_20), -8);
  HVX_VectorPair v_1_25_24 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_27_26),
                                             HEXAGON_HVX_GET_V0(v_0_25_24), -8);
  HVX_VectorPair v_1_27_26 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_27_26),
                                             HEXAGON_HVX_GET_V1(v_0_25_24), -8);
  HVX_VectorPair v_1_29_28 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_31_30),
                                             HEXAGON_HVX_GET_V0(v_0_29_28), -8);
  HVX_VectorPair v_1_31_30 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_31_30),
                                             HEXAGON_HVX_GET_V1(v_0_29_28), -8);

  // Shuffle the 128-bit lanes.
  v_0_1_0 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_5_4),
                            HEXAGON_HVX_GET_V0(v_1_1_0), -16);
  v_0_3_2 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_5_4),
                            HEXAGON_HVX_GET_V1(v_1_1_0), -16);
  v_0_5_4 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_7_6),
                            HEXAGON_HVX_GET_V0(v_1_3_2), -16);
  v_0_7_6 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_7_6),
                            HEXAGON_HVX_GET_V1(v_1_3_2), -16);
  v_0_9_8 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_13_12),
                            HEXAGON_HVX_GET_V0(v_1_9_8), -16);
  v_0_11_10 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_13_12),
                              HEXAGON_HVX_GET_V1(v_1_9_8), -16);
  v_0_13_12 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_15_14),
                              HEXAGON_HVX_GET_V0(v_1_11_10), -16);
  v_0_15_14 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_15_14),
                              HEXAGON_HVX_GET_V1(v_1_11_10), -16);
  v_0_17_16 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_21_20),
                              HEXAGON_HVX_GET_V0(v_1_17_16), -16);
  v_0_19_18 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_21_20),
                              HEXAGON_HVX_GET_V1(v_1_17_16), -16);
  v_0_21_20 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_23_22),
                              HEXAGON_HVX_GET_V0(v_1_19_18), -16);
  v_0_23_22 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_23_22),
                              HEXAGON_HVX_GET_V1(v_1_19_18), -16);
  v_0_25_24 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_29_28),
                              HEXAGON_HVX_GET_V0(v_1_25_24), -16);
  v_0_27_26 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_29_28),
                              HEXAGON_HVX_GET_V1(v_1_25_24), -16);
  v_0_29_28 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_31_30),
                              HEXAGON_HVX_GET_V0(v_1_27_26), -16);
  v_0_31_30 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_31_30),
                              HEXAGON_HVX_GET_V1(v_1_27_26), -16);

  // Shuffle the 256-bit lanes.
  v_1_1_0 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_9_8),
                            HEXAGON_HVX_GET_V0(v_0_1_0), -32);
  v_1_3_2 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_9_8),
                            HEXAGON_HVX_GET_V1(v_0_1_0), -32);
  v_1_5_4 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_11_10),
                            HEXAGON_HVX_GET_V0(v_0_3_2), -32);
  v_1_7_6 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_11_10),
                            HEXAGON_HVX_GET_V1(v_0_3_2), -32);
  v_1_9_8 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_13_12),
                            HEXAGON_HVX_GET_V0(v_0_5_4), -32);
  v_1_11_10 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_13_12),
                              HEXAGON_HVX_GET_V1(v_0_5_4), -32);
  v_1_13_12 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_15_14),
                              HEXAGON_HVX_GET_V0(v_0_7_6), -32);
  v_1_15_14 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_15_14),
                              HEXAGON_HVX_GET_V1(v_0_7_6), -32);
  v_1_17_16 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_25_24),
                              HEXAGON_HVX_GET_V0(v_0_17_16), -32);
  v_1_19_18 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_25_24),
                              HEXAGON_HVX_GET_V1(v_0_17_16), -32);
  v_1_21_20 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_27_26),
                              HEXAGON_HVX_GET_V0(v_0_19_18), -32);
  v_1_23_22 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_27_26),
                              HEXAGON_HVX_GET_V1(v_0_19_18), -32);
  v_1_25_24 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_29_28),
                              HEXAGON_HVX_GET_V0(v_0_21_20), -32);
  v_1_27_26 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_29_28),
                              HEXAGON_HVX_GET_V1(v_0_21_20), -32);
  v_1_29_28 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_0_31_30),
                              HEXAGON_HVX_GET_V0(v_0_23_22), -32);
  v_1_31_30 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_0_31_30),
                              HEXAGON_HVX_GET_V1(v_0_23_22), -32);

  // Shuffle the 512-bit lanes.
  v_0_1_0 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_17_16),
                            HEXAGON_HVX_GET_V0(v_1_1_0), -64);
  v_0_3_2 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_17_16),
                            HEXAGON_HVX_GET_V1(v_1_1_0), -64);
  v_0_5_4 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_19_18),
                            HEXAGON_HVX_GET_V0(v_1_3_2), -64);
  v_0_7_6 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_19_18),
                            HEXAGON_HVX_GET_V1(v_1_3_2), -64);
  v_0_9_8 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_21_20),
                            HEXAGON_HVX_GET_V0(v_1_5_4), -64);
  v_0_11_10 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_21_20),
                              HEXAGON_HVX_GET_V1(v_1_5_4), -64);
  v_0_13_12 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_23_22),
                              HEXAGON_HVX_GET_V0(v_1_7_6), -64);
  v_0_15_14 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_23_22),
                              HEXAGON_HVX_GET_V1(v_1_7_6), -64);
  v_0_17_16 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_25_24),
                              HEXAGON_HVX_GET_V0(v_1_9_8), -64);
  v_0_19_18 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_25_24),
                              HEXAGON_HVX_GET_V1(v_1_9_8), -64);
  v_0_21_20 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_27_26),
                              HEXAGON_HVX_GET_V0(v_1_11_10), -64);
  v_0_23_22 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_27_26),
                              HEXAGON_HVX_GET_V1(v_1_11_10), -64);
  v_0_25_24 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_29_28),
                              HEXAGON_HVX_GET_V0(v_1_13_12), -64);
  v_0_27_26 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_29_28),
                              HEXAGON_HVX_GET_V1(v_1_13_12), -64);
  v_0_29_28 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(v_1_31_30),
                              HEXAGON_HVX_GET_V0(v_1_15_14), -64);
  v_0_31_30 = Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V1(v_1_31_30),
                              HEXAGON_HVX_GET_V1(v_1_15_14), -64);

  kernel.packet[0] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_1_0));
  kernel.packet[1] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_1_0));
  kernel.packet[2] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_3_2));
  kernel.packet[3] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_3_2));
  kernel.packet[4] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_5_4));
  kernel.packet[5] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_5_4));
  kernel.packet[6] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_7_6));
  kernel.packet[7] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_7_6));
  kernel.packet[8] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_9_8));
  kernel.packet[9] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_9_8));
  kernel.packet[10] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_11_10));
  kernel.packet[11] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_11_10));
  kernel.packet[12] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_13_12));
  kernel.packet[13] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_13_12));
  kernel.packet[14] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_15_14));
  kernel.packet[15] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_15_14));
  kernel.packet[16] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_17_16));
  kernel.packet[17] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_17_16));
  kernel.packet[18] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_19_18));
  kernel.packet[19] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_19_18));
  kernel.packet[20] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_21_20));
  kernel.packet[21] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_21_20));
  kernel.packet[22] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_23_22));
  kernel.packet[23] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_23_22));
  kernel.packet[24] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_25_24));
  kernel.packet[25] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_25_24));
  kernel.packet[26] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_27_26));
  kernel.packet[27] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_27_26));
  kernel.packet[28] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_29_28));
  kernel.packet[29] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_29_28));
  kernel.packet[30] = Packet32f::Create(HEXAGON_HVX_GET_V0(v_0_31_30));
  kernel.packet[31] = Packet32f::Create(HEXAGON_HVX_GET_V1(v_0_31_30));
}

template <>
EIGEN_STRONG_INLINE float predux<Packet32f>(const Packet32f& a) {
  HVX_Vector vsum_4 = Q6_Vqf32_vadd_VsfVsf(Q6_V_vror_VR(a.Get(), 4), a.Get());
  HVX_Vector vsum_8 = Q6_Vqf32_vadd_Vqf32Vqf32(Q6_V_vror_VR(vsum_4, 8), vsum_4);
  HVX_Vector vsum_16 =
      Q6_Vqf32_vadd_Vqf32Vqf32(Q6_V_vror_VR(vsum_8, 16), vsum_8);
  HVX_Vector vsum_32 =
      Q6_Vqf32_vadd_Vqf32Vqf32(Q6_V_vror_VR(vsum_16, 32), vsum_16);
  HVX_Vector vsum_64 =
      Q6_Vqf32_vadd_Vqf32Vqf32(Q6_V_vror_VR(vsum_32, 64), vsum_32);
  return pfirst(Packet32f::Create(Q6_Vsf_equals_Vqf32(vsum_64)));
}

template <>
EIGEN_STRONG_INLINE Packet32f ploaddup(const float* from) {
  HVX_Vector load = HVX_loadu(from);
  HVX_VectorPair dup = Q6_W_vshuff_VVR(load, load, -4);
  return Packet32f::Create(HEXAGON_HVX_GET_V0(dup));
}

template <>
EIGEN_STRONG_INLINE Packet32f ploadquad(const float* from) {
  HVX_Vector load = HVX_loadu(from);
  HVX_VectorPair dup = Q6_W_vshuff_VVR(load, load, -4);
  HVX_VectorPair quad =
      Q6_W_vshuff_VVR(HEXAGON_HVX_GET_V0(dup), HEXAGON_HVX_GET_V0(dup), -8);
  return Packet32f::Create(HEXAGON_HVX_GET_V0(quad));
}

template <>
EIGEN_STRONG_INLINE Packet32f preverse(const Packet32f& a) {
  HVX_Vector delta = Q6_Vb_vsplat_R(0x7c);
  return Packet32f::Create(Q6_V_vdelta_VV(a.Get(), delta));
}

template <>
EIGEN_STRONG_INLINE Packet32f pmin(const Packet32f& a, const Packet32f& b) {
  return Packet32f::Create(Q6_Vsf_vmin_VsfVsf(a.Get(), b.Get()));
}

template <>
EIGEN_STRONG_INLINE Packet32f pmax(const Packet32f& a, const Packet32f& b) {
  return Packet32f::Create(Q6_Vsf_vmax_VsfVsf(a.Get(), b.Get()));
}

template <>
EIGEN_STRONG_INLINE Packet32f pand(const Packet32f& a, const Packet32f& b) {
  return Packet32f::Create(a.Get() & b.Get());
}

template <>
EIGEN_STRONG_INLINE Packet32f por(const Packet32f& a, const Packet32f& b) {
  return Packet32f::Create(a.Get() | b.Get());
}

template <>
EIGEN_STRONG_INLINE Packet32f pxor(const Packet32f& a, const Packet32f& b) {
  return Packet32f::Create(a.Get() ^ b.Get());
}

template <>
EIGEN_STRONG_INLINE Packet32f pnot(const Packet32f& a) {
  return Packet32f::Create(~a.Get());
}

template <>
EIGEN_STRONG_INLINE Packet32f pselect(const Packet32f& mask, const Packet32f& a,
                                      const Packet32f& b) {
  HVX_VectorPred pred = Q6_Q_vcmp_eq_VwVw(mask.Get(), Q6_V_vzero());
  return Packet32f::Create(Q6_V_vmux_QVV(pred, b.Get(), a.Get()));
}

template <typename Op>
EIGEN_STRONG_INLINE float predux_generic(const Packet32f& a, Op op) {
  Packet32f vredux_4 = op(Packet32f::Create(Q6_V_vror_VR(a.Get(), 4)), a);
  Packet32f vredux_8 =
      op(Packet32f::Create(Q6_V_vror_VR(vredux_4.Get(), 8)), vredux_4);
  Packet32f vredux_16 =
      op(Packet32f::Create(Q6_V_vror_VR(vredux_8.Get(), 16)), vredux_8);
  Packet32f vredux_32 =
      op(Packet32f::Create(Q6_V_vror_VR(vredux_16.Get(), 32)), vredux_16);
  Packet32f vredux_64 =
      op(Packet32f::Create(Q6_V_vror_VR(vredux_32.Get(), 64)), vredux_32);
  return pfirst(vredux_64);
}

template <>
EIGEN_STRONG_INLINE float predux_max(const Packet32f& a) {
  return predux_generic(a, pmax<Packet32f>);
}

template <>
EIGEN_STRONG_INLINE float predux_min(const Packet32f& a) {
  return predux_generic(a, pmin<Packet32f>);
}

template <>
EIGEN_STRONG_INLINE bool predux_any(const Packet32f& a) {
  return predux_generic(a, por<Packet32f>) != 0.0f;
}

static const float index_vsf[32] __attribute__((aligned(128))) = {
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};

template <>
EIGEN_STRONG_INLINE Packet32f plset(const float& a) {
  return padd(pload<Packet32f>(index_vsf), pset1<Packet32f>(a));
}

// qfloat32 operations.
template <>
EIGEN_STRONG_INLINE Packet32qf pzero<Packet32qf>(const Packet32qf&) {
  return Packet32qf::Create(Q6_V_vzero());
}

template <>
EIGEN_STRONG_INLINE Packet32qf pmul<Packet32qf>(const Packet32qf& a,
                                                const Packet32qf& b) {
  return Packet32qf::Create(Q6_Vqf32_vmpy_Vqf32Vqf32(a.Get(), b.Get()));
}

template <>
EIGEN_STRONG_INLINE Packet32qf padd<Packet32qf>(const Packet32qf& a,
                                                const Packet32qf& b) {
  return Packet32qf::Create(Q6_Vqf32_vadd_Vqf32Vqf32(a.Get(), b.Get()));
}

// Mixed float32 and qfloat32 operations.
EIGEN_STRONG_INLINE Packet32qf pmadd_f32_to_qf32(const Packet32f& a,
                                                 const Packet32f& b,
                                                 const Packet32qf& c) {
  return Packet32qf::Create(Q6_Vqf32_vadd_Vqf32Vqf32(
      Q6_Vqf32_vmpy_VsfVsf(a.Get(), b.Get()), c.Get()));
}

EIGEN_STRONG_INLINE Packet32f pmadd_qf32_to_f32(const Packet32qf& a,
                                                const Packet32f& b,
                                                const Packet32f& c) {
  return Packet32f::Create(Q6_Vsf_equals_Vqf32(Q6_Vqf32_vadd_Vqf32Vsf(
      Q6_Vqf32_vmpy_VsfVsf(Q6_Vsf_equals_Vqf32(a.Get()), b.Get()), c.Get())));
}

}  // end namespace internal
}  // end namespace Eigen

#endif  // __HVX__ && (__HVX_LENGTH__ == 128) && __HVX_ARCH__ >= 68

#endif  // EIGEN_HVX_PACKET_MATH_H
