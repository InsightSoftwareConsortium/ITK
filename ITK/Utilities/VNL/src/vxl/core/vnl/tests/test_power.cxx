#include <vnl/vnl_power.h>
#include <vnl/vnl_double_2x2.h>
#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_double_4x4.h>

#include <testlib/testlib_test.h>

static void test_power()
{
  double eps = 1e-11;
  vnl_double_2x2 residue2, id2; id2.set_identity(); vnl_matrix<double> ID2 = id2.as_ref();
  vnl_double_3x3 residue3, id3; id3.set_identity(); vnl_matrix<double> ID3 = id3.as_ref();
  vnl_double_4x4 residue4, id4; id4.set_identity(); vnl_matrix<double> ID4 = id4.as_ref();
  vnl_matrix<double> M, Mi;

  // 11th power of the identity 2x2 matrix
  vnl_double_2x2 id2i = vnl_power(id2,11);
  TEST("11th power of 2x2 Id", id2i, id2);
  // 11th power of the identity 2x2 matrix
  id2i = vnl_power(id2,-11);
  TEST("-11th power of 2x2 Id", id2i, id2);

  // power of a specific 2x2 matrix
  double M2[4] = {
    0.60684258354179,   0.89129896614890,
    0.48598246870930,   0.76209683302739
  };
  vnl_double_2x2 m2(M2);
  vnl_double_2x2 m2i = vnl_power(m2, 0);
  TEST("0th power of fixed 2x2 matrix", m2i, id2);
  m2i = vnl_power(m2, 1);
  TEST("1st power of fixed 2x2 matrix", m2i, m2i);
  m2i = vnl_power(m2, -1);
  residue2 = m2*m2i - id2;
  residue2 += m2i*m2 - id2;
  TEST_NEAR("-1st power of a fixed 2x2 matrix", residue2.array_inf_norm(), 0.0, eps);

  M = m2.as_ref();
  Mi = vnl_power(M,0);
  TEST("0th power of 2x2 matrix", Mi, ID2);
  Mi = vnl_power(M,1);
  TEST("1st power of 2x2 matrix", Mi, M);
  Mi = vnl_power(M,2);
  TEST("2nd power of 2x2 matrix", Mi, M*M);
  Mi = vnl_power(M,3);
  TEST("3rd power of 2x2 matrix", Mi, M*M*M);
  Mi = vnl_power(M,4);
  TEST_NEAR("4th power of 2x2 matrix", (Mi-M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,5);
  TEST_NEAR("5th power of 2x2 matrix", (Mi-M*M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,6);
  TEST_NEAR("6th power of 2x2 matrix", (Mi-M*M*M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,-1);
  residue2 = m2*Mi - id2;
  TEST_NEAR("-1st power of a 2x2 matrix", residue2.array_inf_norm(), 0.0, eps);

  // power of the identity 3x3 matrix
  vnl_double_3x3 id3i = vnl_power(id3,11);
  TEST("11th power of 3x3 Id", id3i, id3);
  id3i = vnl_power(id3,-11);
  TEST("-11th power of 3x3 Id", id3i, id3);

  // power of a specific 3x3 matrix
  double M3[9] = {
    0.45646766516834,   0.44470336435319,   0.92181297074480,
    0.01850364324822,   0.61543234810009,   0.73820724581067,
    0.82140716429525,   0.79193703742704,   0.17626614449462
  };
  vnl_double_3x3 m3(M3);
  vnl_double_3x3 m3i = vnl_power(m3,0);
  TEST("0th power of fixed 3x3 matrix", m3i, id3);
  m3i = vnl_power(m3,1);
  TEST("1st power of fixed 3x3 matrix", m3i, m3i);
  m3i = vnl_power(m3,-1);
  residue3 = m3*m3i - id3;
  residue3 += m3i*m3 - id3;
  TEST_NEAR("-1st power of a fixed 3x3 matrix", residue3.array_inf_norm(), 0.0, eps);

  M = m3.as_ref();
  Mi = vnl_power(M,0);
  TEST("0th power of 3x3 matrix", Mi, ID3);
  Mi = vnl_power(M,1);
  TEST("1st power of 3x3 matrix", Mi, M);
  Mi = vnl_power(M,2);
  TEST("2nd power of 3x3 matrix", Mi, M*M);
  Mi = vnl_power(M,3);
  TEST("3rd power of 3x3 matrix", Mi, M*M*M);
  Mi = vnl_power(M,4);
  TEST_NEAR("4th power of 3x3 matrix", (Mi-M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,5);
  TEST_NEAR("5th power of 3x3 matrix", (Mi-M*M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,6);
  TEST_NEAR("6th power of 3x3 matrix", (Mi-M*M*M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,-1);
  residue3 = m3*Mi - id3;
  TEST_NEAR("-1st power of a 3x3 matrix", residue3.array_inf_norm(), 0.0, eps);

  // power of the identity 4x4 matrix
  vnl_double_4x4 id4i = vnl_power(id4,11);
  TEST("11th power of 4x4 Id", id4i, id4);
  id4i = vnl_power(id4,-11);
  TEST("-11th power of 4x4 Id", id4i, id4);

  // power of a specific 4x4 matrix
  double M4[16] = {
    0.40570621306210,   0.89364953091353,   0.00986130066092,   0.60379247919382,
    0.93546969910761,   0.05789130478427,   0.13889088195695,   0.27218792496996,
    0.91690443991341,   0.35286813221700,   0.20276521856027,   0.19881426776106,
    0.41027020699095,   0.81316649730376,   0.19872174266149,   0.01527392702904
  };
  vnl_double_4x4 m4(M4);
  vnl_double_4x4 m4i = vnl_power(m4,0);
  TEST("0th power of fixed 4x4 matrix", m4i, id4);
  m4i = vnl_power(m4,1);
  TEST("1st power of fixed 4x4 matrix", m4i, m4i);
  m4i = vnl_power(m4,-1);
  residue4 = m4*m4i - id4;
  residue4 += m4i*m4 - id4;
  TEST_NEAR("-1st power of a fixed 4x4 matrix", residue4.array_inf_norm(), 0.0, eps);

  M = m4.as_ref();
  Mi = vnl_power(M,0);
  TEST("0th power of 4x4 matrix", Mi, ID4);
  Mi = vnl_power(M,1);
  TEST("1st power of 4x4 matrix", Mi, M);
  Mi = vnl_power(M,2);
  TEST("2nd power of 4x4 matrix", Mi, M*M);
  Mi = vnl_power(M,3);
  TEST("3rd power of 4x4 matrix", Mi, M*M*M);
  Mi = vnl_power(M,4);
  TEST_NEAR("4th power of 4x4 matrix", (Mi-M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,5);
  TEST_NEAR("5th power of 4x4 matrix", (Mi-M*M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,6);
  TEST_NEAR("6th power of 4x4 matrix", (Mi-M*M*M*M*M*M).array_inf_norm(), 0.0, eps);
  Mi = vnl_power(M,-1);
  residue4 = m4*Mi - id4;
  TEST_NEAR("4x4 vnl_power", residue4.array_inf_norm(), 0.0, eps);
}

TESTMAIN(test_power);
