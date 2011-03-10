#include <vnl/vnl_inverse.h>
#include <vnl/vnl_double_2x2.h>
#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_double_4x4.h>
#include <vnl/vnl_random.h>

#include <testlib/testlib_test.h>

static void test_inverse()
{
  double eps = 1e-11;
  vnl_random rng(9667566ul);
  vnl_double_2x2 residue2, id2; id2.set_identity();
  vnl_double_3x3 residue3, id3; id3.set_identity();
  vnl_double_4x4 residue4, id4; id4.set_identity();
  vnl_matrix<double> M, Mi;

  // 2x2 inverse of a specific matrix
  vnl_double_2x2 id2i = vnl_inverse(id2);
  TEST("vnl_inverse of 2x2 Id", id2i, id2);
  id2i = vnl_inverse_transpose(id2);
  TEST("vnl_inverse_transpose of 2x2 Id", id2i, id2);

  double M2[4] = {
    0.60684258354179,   0.89129896614890,
    0.48598246870930,   0.76209683302739
  };
  vnl_double_2x2 m2(M2);
  vnl_double_2x2 m2i = vnl_inverse(m2);
  residue2 = m2*m2i - id2;
  TEST_NEAR("2x2 vnl_inverse", residue2.array_inf_norm(), 0.0, eps);
  residue2 = m2i*m2 - id2;
  TEST_NEAR("2x2 vnl_inverse", residue2.array_inf_norm(), 0.0, eps);

  m2i = vnl_inverse_transpose(m2);
  residue2 = m2.transpose()*m2i - id2;
  TEST_NEAR("2x2 vnl_inverse_transpose", residue2.array_inf_norm(), 0.0, eps);
  residue2 = m2i.transpose()*m2 - id2;
  TEST_NEAR("2x2 vnl_inverse_transpose", residue2.array_inf_norm(), 0.0, eps);

  M = vnl_matrix<double>(m2.data_block(), 2, 2);
  Mi = vnl_inverse(M);
  residue2 = m2*Mi - id2;
  TEST_NEAR("2x2 vnl_inverse", residue2.array_inf_norm(), 0.0, eps);

  Mi = vnl_inverse_transpose(M).transpose();
  residue2 = m2*Mi - id2;
  TEST_NEAR("2x2 vnl_inverse_transpose", residue2.array_inf_norm(), 0.0, eps);

  // 2x2 inverse of random matrix
  for (int i=0; i<4; ++i) M2[i] = rng.drand32(-1.0,1.0);
  vnl_double_2x2 m2r(M2);
  m2i = vnl_inverse(m2r);
  residue2 = m2r*m2i - id2;
  TEST_NEAR("rand 2x2 vnl_inverse", residue2.array_inf_norm(), 0.0, eps);
  residue2 = m2i*m2r - id2;
  TEST_NEAR("rand 2x2 vnl_inverse", residue2.array_inf_norm(), 0.0, eps);

  m2i = vnl_inverse_transpose(m2r);
  residue2 = m2r.transpose()*m2i - id2;
  TEST_NEAR("rand 2x2 vnl_inverse_transpose", residue2.array_inf_norm(), 0.0, eps);
  residue2 = m2i.transpose()*m2r - id2;
  TEST_NEAR("rand 2x2 vnl_inverse_transpose", residue2.array_inf_norm(), 0.0, eps);

  // 3x3 inverse of a specific matrix
  vnl_double_3x3 id3i = vnl_inverse(id3);
  TEST("vnl_inverse of 3x3 Id", id3i, id3);
  id3i = vnl_inverse_transpose(id3);
  TEST("vnl_inverse_transpose of 3x3 Id", id3i, id3);

  double M3[9] = {
    0.45646766516834,   0.44470336435319,   0.92181297074480,
    0.01850364324822,   0.61543234810009,   0.73820724581067,
    0.82140716429525,   0.79193703742704,   0.17626614449462
  };
  vnl_double_3x3 m3(M3);
  vnl_double_3x3 m3i = vnl_inverse(m3);
  residue3 = m3*m3i - id3;
  TEST_NEAR("3x3 vnl_inverse", residue3.array_inf_norm(), 0.0, eps);
  residue3 = m3i*m3 - id3;
  TEST_NEAR("3x3 vnl_inverse", residue3.array_inf_norm(), 0.0, eps);

  m3i = vnl_inverse_transpose(m3);
  residue3 = m3.transpose()*m3i - id3;
  TEST_NEAR("3x3 vnl_inverse_transpose", residue3.array_inf_norm(), 0.0, eps);
  residue3 = m3i.transpose()*m3 - id3;
  TEST_NEAR("3x3 vnl_inverse_transpose", residue3.array_inf_norm(), 0.0, eps);

  M = vnl_matrix<double>(m3.data_block(), 3, 3);
  Mi = vnl_inverse(M);
  residue3 = m3*Mi - id3;
  TEST_NEAR("3x3 vnl_inverse", residue3.array_inf_norm(), 0.0, eps);

  Mi = vnl_inverse_transpose(M).transpose();
  residue3 = m3*Mi - id3;
  TEST_NEAR("3x3 vnl_inverse_transpose", residue3.array_inf_norm(), 0.0, eps);

  // 3x3 inverse of random matrix
  for (int i=0; i<9; ++i) M3[i] = rng.drand32(-1.0,1.0);
  vnl_double_3x3 m3r(M3);
  m3i = vnl_inverse(m3r);
  residue3 = m3r*m3i - id3;
  TEST_NEAR("3x3 vnl_inverse", residue3.array_inf_norm(), 0.0, eps);
  residue3 = m3i*m3r - id3;
  TEST_NEAR("3x3 vnl_inverse", residue3.array_inf_norm(), 0.0, eps);

  m3i = vnl_inverse_transpose(m3r);
  residue3 = m3r.transpose()*m3i - id3;
  TEST_NEAR("rand 3x3 vnl_inverse_transpose", residue3.array_inf_norm(), 0.0, eps);
  residue3 = m3i.transpose()*m3r - id3;
  TEST_NEAR("rand 3x3 vnl_inverse_transpose", residue3.array_inf_norm(), 0.0, eps);

  // 4x4 inverse of a specific matrix
  vnl_double_4x4 id4i = vnl_inverse(id4);
  TEST("vnl_inverse of 4x4 Id", id4i, id4);
  id4i = vnl_inverse_transpose(id4);
  TEST("vnl_inverse_transpose of 4x4 Id", id4i, id4);

  double M4[16] = {
    0.40570621306210,   0.89364953091353,   0.00986130066092,   0.60379247919382,
    0.93546969910761,   0.05789130478427,   0.13889088195695,   0.27218792496996,
    0.91690443991341,   0.35286813221700,   0.20276521856027,   0.19881426776106,
    0.41027020699095,   0.81316649730376,   0.19872174266149,   0.01527392702904
  };
  vnl_double_4x4 m4(M4);
  vnl_double_4x4 m4i = vnl_inverse(m4);
  residue4 = m4*m4i - id4;
  TEST_NEAR("4x4 vnl_inverse", residue4.array_inf_norm(), 0.0, eps);
  residue4 = m4i*m4 - id4;
  TEST_NEAR("4x4 vnl_inverse", residue4.array_inf_norm(), 0.0, eps);

  m4i = vnl_inverse_transpose(m4);
  residue4 = m4.transpose()*m4i - id4;
  TEST_NEAR("4x4 vnl_inverse_transpose", residue4.array_inf_norm(), 0.0, eps);
  residue4 = m4i.transpose()*m4 - id4;
  TEST_NEAR("4x4 vnl_inverse_transpose", residue4.array_inf_norm(), 0.0, eps);

  M = vnl_matrix<double>(m4.data_block(), 4, 4);
  Mi = vnl_inverse(M);
  residue4 = m4*Mi - id4;
  TEST_NEAR("4x4 vnl_inverse", residue4.array_inf_norm(), 0.0, eps);

  Mi = vnl_inverse_transpose(M).transpose();
  residue4 = m4*Mi - id4;
  TEST_NEAR("4x4 vnl_inverse_transpose", residue4.array_inf_norm(), 0.0, eps);

  // 4x4 inverse of random matrix
  for (int i=0; i<16; ++i) M4[i] = rng.drand32(-1.0,1.0);
  vnl_double_4x4 m4r(M4);
  m4i = vnl_inverse(m4r);
  residue4 = m4r*m4i - id4;
  TEST_NEAR("4x4 vnl_inverse", residue4.array_inf_norm(), 0.0, eps);
  residue4 = m4i*m4r - id4;
  TEST_NEAR("4x4 vnl_inverse", residue4.array_inf_norm(), 0.0, eps);

  m4i = vnl_inverse_transpose(m4r);
  residue4 = m4r.transpose()*m4i - id4;
  TEST_NEAR("rand 4x4 vnl_inverse_transpose", residue4.array_inf_norm(), 0.0, eps);
  residue4 = m4i.transpose()*m4r - id4;
  TEST_NEAR("rand 4x4 vnl_inverse_transpose", residue4.array_inf_norm(), 0.0, eps);

  // 4x4 inverse of a specific sparse matrix
  double M4s[16] = {
    0.9998, 0.0, 0.02,   0.059,
    0.0,    1.0, 0.0,    0.0,
   -0.02,   0.0, 0.9998, 0.0,
    0.0,    0.0, 0.0,    1.0
  };
  vnl_double_4x4 m4s(M4s);
  m4i = vnl_inverse(m4s);
  residue4 = m4s*m4i - id4;
  TEST_NEAR("4x4 vnl_inverse", residue4.array_inf_norm(), 0.0, eps);
  residue4 = m4i*m4s - id4;
  TEST_NEAR("4x4 vnl_inverse", residue4.array_inf_norm(), 0.0, eps);
  m4i = vnl_inverse_transpose(m4s);
  residue4 = m4s.transpose()*m4i - id4;
  TEST_NEAR("4x4 vnl_inverse_transpose", residue4.array_inf_norm(), 0.0, eps);
  residue4 = m4i.transpose()*m4s - id4;
  TEST_NEAR("4x4 vnl_inverse_transpose", residue4.array_inf_norm(), 0.0, eps);
}

TESTMAIN(test_inverse);
