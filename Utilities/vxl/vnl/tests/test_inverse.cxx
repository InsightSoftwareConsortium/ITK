#include <vnl/vnl_inverse.h>
#include <vnl/vnl_double_2x2.h>
#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_double_4x4.h>
#include <vnl/vnl_sample.h>

#include <testlib/testlib_test.h>

void test_inverse() {
  double eps = 1e-11;

  {
    vnl_double_2x2 id2; id2.set_identity();
    double M2[4] = {
      0.60684258354179,   0.89129896614890,
      0.48598246870930,   0.76209683302739
    };
    vnl_double_2x2 m2(M2);
    vnl_double_2x2 m2i = vnl_inverse(m2);
    vnl_double_2x2 residue = m2*m2i - id2;
    TEST_NEAR("2x2 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
    residue = m2i*m2 - id2;
    TEST_NEAR("2x2 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
  }

  {
    vnl_double_2x2 id2; id2.set_identity();
    double M2[4];
    for (int i=0; i<4; ++i) M2[i] = vnl_sample_uniform(-1.0,1.0);
    vnl_double_2x2 m2(M2);
    vnl_double_2x2 m2i = vnl_inverse(m2);
    vnl_double_2x2 residue = m2*m2i - id2;
    TEST_NEAR("2x2 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
    residue = m2i*m2 - id2;
    TEST_NEAR("2x2 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
  }

  {
    vnl_double_3x3 id3; id3.set_identity();
    double M3[9] = {
      0.45646766516834,   0.44470336435319,   0.92181297074480,
      0.01850364324822,   0.61543234810009,   0.73820724581067,
      0.82140716429525,   0.79193703742704,   0.17626614449462
    };
    vnl_double_3x3 m3(M3);
    vnl_double_3x3 m3i = vnl_inverse(m3);
    vnl_double_3x3 residue = m3*m3i - id3;
    TEST_NEAR("3x3 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
    residue = m3i*m3 - id3;
    TEST_NEAR("3x3 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
  }

  {
    vnl_double_3x3 id3; id3.set_identity();
    double M3[9];
    for (int i=0; i<9; ++i) M3[i] = vnl_sample_uniform(-1.0,1.0);
    vnl_double_3x3 m3(M3);
    vnl_double_3x3 m3i = vnl_inverse(m3);
    vnl_double_3x3 residue = m3*m3i - id3;
    TEST_NEAR("3x3 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
    residue = m3i*m3 - id3;
    TEST_NEAR("3x3 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
  }

  {
    vnl_double_4x4 id4; id4.set_identity();
    double M4[16] = {
      0.40570621306210,   0.89364953091353,   0.00986130066092,   0.60379247919382,
      0.93546969910761,   0.05789130478427,   0.13889088195695,   0.27218792496996,
      0.91690443991341,   0.35286813221700,   0.20276521856027,   0.19881426776106,
      0.41027020699095,   0.81316649730376,   0.19872174266149,   0.01527392702904
    };
    vnl_double_4x4 m4(M4);
    vnl_double_4x4 m4i = vnl_inverse(m4);
    vnl_double_4x4 residue = m4*m4i - id4;
    TEST_NEAR("4x4 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
    residue = m4i*m4 - id4;
    TEST_NEAR("4x4 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
  }

  {
    vnl_double_4x4 id4; id4.set_identity();
    double M4[16];
    for (int i=0; i<16; ++i) M4[i] = vnl_sample_uniform(-1.0,1.0);
    vnl_double_4x4 m4(M4);
    vnl_double_4x4 m4i = vnl_inverse(m4);
    vnl_double_4x4 residue = m4*m4i - id4;
    TEST_NEAR("4x4 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
    residue = m4i*m4 - id4;
    TEST_NEAR("4x4 vnl_inverse", residue.array_inf_norm(), 0.0, eps);
  }
}

TESTMAIN(test_inverse);
