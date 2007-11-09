#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/vnl_rank.h>

#include <testlib/testlib_test.h>

inline int svd_rank(vnl_matrix<double> const& M) { return vnl_svd<double>(M, 1e-8).rank(); }

void test_rank()
{
  // 1x1 double
  vnl_matrix<double> m11(1,1, 0.0); // all zero matrix
  TEST("zero_1x1.vnl_svd().rank()", svd_rank(m11), 0);
  TEST("vnl_rank(zero_1x1)", vnl_rank(m11), 0);
  m11[0][0] = -3.0;
  TEST("-3_1x1.vnl_svd().rank()", svd_rank(m11), 1);
  TEST("vnl_rank(-3_1x1)", vnl_rank(m11), 1);
  TEST("vnl_rank(-3_1x1, vnl_rank_row)", vnl_rank(m11, vnl_rank_row), 1);
  TEST("vnl_rank(-3_1x1, vnl_rank_column)", vnl_rank(m11, vnl_rank_column), 1);

  // 1x1 int
  vnl_matrix<int> i11(1,1, 0); // all zero matrix
  TEST("vnl_rank(int_zero_1x1)", vnl_rank(i11), 0);
  i11[0][0] = -3;
  TEST("vnl_rank(int_-3_1x1)", vnl_rank(i11), 1);
  TEST("vnl_rank(int_-3_1x1, vnl_rank_row)", vnl_rank(i11, vnl_rank_row), 1);
  TEST("vnl_rank(int_-3_1x1, vnl_rank_column)", vnl_rank(i11, vnl_rank_column), 1);

  // 1x2 double
  vnl_matrix<double> m12(1,2, 0.0); // all zero matrix
  TEST("zero_1x2.vnl_svd().rank()", svd_rank(m12), 0);
  TEST("vnl_rank(zero_1x2)", vnl_rank(m12), 0);
  TEST("vnl_rank(zero_1x2, vnl_rank_row)", vnl_rank(m12, vnl_rank_row), 0);
  TEST("vnl_rank(zero_1x2, vnl_rank_column)", vnl_rank(m12, vnl_rank_column), 0);
  m12[0][1] = -2.0;
  TEST("0-2_1x2.vnl_svd().rank()", svd_rank(m12), 1);
  TEST("vnl_rank(0-2_1x2)", vnl_rank(m12), 1);
  TEST("vnl_rank(0-2_1x2, vnl_rank_row)", vnl_rank(m12, vnl_rank_row), 1);
  TEST("vnl_rank(0-2_1x2, vnl_rank_column)", vnl_rank(m12, vnl_rank_column), 1);
  m12[0][0] = 1.0;
  TEST("1-2_1x2.vnl_svd().rank()", svd_rank(m12), 1);
  TEST("vnl_rank(1-2_1x2)", vnl_rank(m12), 1);
  TEST("vnl_rank(1-2_1x2, vnl_rank_row)", vnl_rank(m12, vnl_rank_row), 1);
  TEST("vnl_rank(1-2_1x2, vnl_rank_column)", vnl_rank(m12, vnl_rank_column), 1);

  // 1x2 int
  vnl_matrix<int> i12(1,2, 0); // all zero matrix
  TEST("vnl_rank(int_zero_1x2)", vnl_rank(i12), 0);
  TEST("vnl_rank(int_zero_1x2, vnl_rank_row)", vnl_rank(i12, vnl_rank_row), 0);
  TEST("vnl_rank(int_zero_1x2, vnl_rank_column)", vnl_rank(i12, vnl_rank_column), 0);
  i12[0][1] = -2;
  TEST("vnl_rank(int_0-2_1x2)", vnl_rank(i12), 1);
  TEST("vnl_rank(int_0-2_1x2, vnl_rank_row)", vnl_rank(i12, vnl_rank_row), 1);
  TEST("vnl_rank(int_0-2_1x2, vnl_rank_column)", vnl_rank(i12, vnl_rank_column), 1);
  i12[0][0] = 1;
  TEST("vnl_rank(int_1-2_1x2)", vnl_rank(i12), 1);
  TEST("vnl_rank(int_1-2_1x2, vnl_rank_row)", vnl_rank(i12, vnl_rank_row), 1);
  TEST("vnl_rank(int_1-2_1x2, vnl_rank_column)", vnl_rank(i12, vnl_rank_column), 1);

  // 2x2 double
  vnl_matrix<double> m22(2,2, 0.0); // all zero matrix
  TEST("zero_2x2.vnl_svd().rank()", svd_rank(m22), 0);
  TEST("vnl_rank(zero_2x2)", vnl_rank(m22), 0);
  TEST("vnl_rank(zero_2x2, vnl_rank_row)", vnl_rank(m22, vnl_rank_row), 0);
  TEST("vnl_rank(zero_2x2, vnl_rank_column)", vnl_rank(m22, vnl_rank_column), 0);
  m22[0][1] = 6.0;
  TEST("0300_2x2.vnl_svd().rank()", svd_rank(m22), 1);
  TEST("vnl_rank(0600_2x2)", vnl_rank(m22), 1);
  TEST("vnl_rank(0600_2x2, vnl_rank_row)", vnl_rank(m22, vnl_rank_row), 1);
  TEST("vnl_rank(0600_2x2, vnl_rank_column)", vnl_rank(m22, vnl_rank_column), 1);
  m22[1][0] = -1.0;
  TEST("06-10_2x2.vnl_svd().rank()", svd_rank(m22), 2);
  TEST("vnl_rank(06-10_2x2)", vnl_rank(m22), 2);
  TEST("vnl_rank(06-10_2x2, vnl_rank_row)", vnl_rank(m22, vnl_rank_row), 2);
  TEST("vnl_rank(06-10_2x2, vnl_rank_column)", vnl_rank(m22, vnl_rank_column), 2);
  m22[0][0] = -2.0; m22[1][1] = 3.0;
  TEST("-26-13_2x2.vnl_svd().rank()", svd_rank(m22), 1);
  TEST("vnl_rank(-26-13_2x2)", vnl_rank(m22), 1);
  TEST("vnl_rank(-26-13_2x2, vnl_rank_row)", vnl_rank(m22, vnl_rank_row), 1);
  TEST("vnl_rank(-26-13_2x2, vnl_rank_column)", vnl_rank(m22, vnl_rank_column), 1);
  m22[1][0] = -3.0; m22[1][1] = 9.0;
  TEST("-26-39_2x2.vnl_svd().rank()", svd_rank(m22), 1);
  TEST("vnl_rank(-26-39_2x2)", vnl_rank(m22), 1);
  TEST("vnl_rank(-26-39_2x2, vnl_rank_row)", vnl_rank(m22, vnl_rank_row), 1);
  TEST("vnl_rank(-26-39_2x2, vnl_rank_column)", vnl_rank(m22, vnl_rank_column), 1);
  m22 *= 2.0; // now the pivot element will never be 1
  TEST("-4_12_-6_18_2x2.vnl_svd().rank()", svd_rank(m22), 1);
  TEST("vnl_rank(-4_12_-6_18_2x2)", vnl_rank(m22), 1);
  TEST("vnl_rank(-4_12_-6_18_2x2, vnl_rank_row)", vnl_rank(m22, vnl_rank_row), 1);
  TEST("vnl_rank(-4_12_-6_18_2x2, vnl_rank_column)", vnl_rank(m22, vnl_rank_column), 1);

  // 2x2 int
  vnl_matrix<int> i22(2,2, 0); // all zero matrix
  TEST("vnl_rank(int_zero_2x2)", vnl_rank(i22), 0);
  TEST("vnl_rank(int_zero_2x2, vnl_rank_row)", vnl_rank(i22, vnl_rank_row), 0);
  TEST("vnl_rank(int_zero_2x2, vnl_rank_column)", vnl_rank(i22, vnl_rank_column), 0);
  i22[0][1] = 6;
  TEST("vnl_rank(int_0600_2x2)", vnl_rank(i22), 1);
  TEST("vnl_rank(int_0600_2x2, vnl_rank_row)", vnl_rank(i22, vnl_rank_row), 1);
  TEST("vnl_rank(int_0600_2x2, vnl_rank_column)", vnl_rank(i22, vnl_rank_column), 1);
  i22[1][0] = -1;
  TEST("vnl_rank(int_06-10_2x2)", vnl_rank(i22), 2);
  TEST("vnl_rank(int_06-10_2x2, vnl_rank_row)", vnl_rank(i22, vnl_rank_row), 2);
  TEST("vnl_rank(int_06-10_2x2, vnl_rank_column)", vnl_rank(i22, vnl_rank_column), 2);
  i22[0][0] = -2; i22[1][1] = 3;
  TEST("vnl_rank(int_-26-13_2x2)", vnl_rank(i22), 1);
  TEST("vnl_rank(int_-26-13_2x2, vnl_rank_row)", vnl_rank(i22, vnl_rank_row), 1);
  TEST("vnl_rank(int_-26-13_2x2, vnl_rank_column)", vnl_rank(i22, vnl_rank_column), 1);
  i22[1][0] = -3; i22[1][1] = 9;
  TEST("vnl_rank(int_-26-39_2x2)", vnl_rank(i22), 1);
  TEST("vnl_rank(int_-26-39_2x2, vnl_rank_row)", vnl_rank(i22, vnl_rank_row), 1);
  TEST("vnl_rank(int_-26-39_2x2, vnl_rank_column)", vnl_rank(i22, vnl_rank_column), 1);
  i22 *= 2; // now the pivot element will never be 1
  TEST("vnl_rank(-4_12_-6_18_2x2)", vnl_rank(i22), 1);
  TEST("vnl_rank(-4_12_-6_18_2x2, vnl_rank_row)", vnl_rank(i22, vnl_rank_row), 1);
  TEST("vnl_rank(-4_12_-6_18_2x2, vnl_rank_column)", vnl_rank(i22, vnl_rank_column), 1);

  // 3x2 double
  vnl_matrix<double> m32(3,2, 0.0); // all zero matrix
  TEST("zero_3x2.vnl_svd().rank()", svd_rank(m32), 0);
  TEST("vnl_rank(zero_3x2)", vnl_rank(m32), 0);
  TEST("vnl_rank(zero_3x2, vnl_rank_row)", vnl_rank(m32, vnl_rank_row), 0);
  TEST("vnl_rank(zero_3x2, vnl_rank_column)", vnl_rank(m32, vnl_rank_column), 0);
  m32[0][1] = 6.0;
  TEST("3x2.vnl_svd().rank()", svd_rank(m32), 1);
  TEST("vnl_rank(3x2)", vnl_rank(m32), 1);
  TEST("vnl_rank(3x2, vnl_rank_row)", vnl_rank(m32, vnl_rank_row), 1);
  TEST("vnl_rank(3x2, vnl_rank_column)", vnl_rank(m32, vnl_rank_column), 1);
  m32[2][0] = -1.0;
  TEST("3x2.vnl_svd().rank()", svd_rank(m32), 2);
  TEST("vnl_rank(3x2)", vnl_rank(m32), 2);
  TEST("vnl_rank(3x2, vnl_rank_row)", vnl_rank(m32, vnl_rank_row), 2);
  TEST("vnl_rank(3x2, vnl_rank_column)", vnl_rank(m32, vnl_rank_column), 2);
  m32[1][0] = 3.0; m32[2][1] = 1.0;
  TEST("3x2.vnl_svd().rank()", svd_rank(m32), 2);
  TEST("vnl_rank(3x2)", vnl_rank(m32), 2);
  TEST("vnl_rank(3x2, vnl_rank_row)", vnl_rank(m32, vnl_rank_row), 2);
  TEST("vnl_rank(3x2, vnl_rank_column)", vnl_rank(m32, vnl_rank_column), 2);
  m32[0][0] = -6.0; m32[1][1] = -3.0;
  TEST("3x2.vnl_svd().rank()", svd_rank(m32), 1);
  TEST("vnl_rank(3x2)", vnl_rank(m32), 1);
  TEST("vnl_rank(3x2, vnl_rank_row)", vnl_rank(m32, vnl_rank_row), 1);
  TEST("vnl_rank(3x2, vnl_rank_column)", vnl_rank(m32, vnl_rank_column), 1);
  m32 *= 2.0;
  TEST("3x2.vnl_svd().rank()", svd_rank(m32), 1);
  TEST("vnl_rank(3x2)", vnl_rank(m32), 1);

  // 3x2 int
  vnl_matrix<int> i32(3,2, 0); // all zero matrix
  TEST("vnl_rank(zero_3x2)", vnl_rank(i32), 0);
  TEST("vnl_rank(zero_3x2, vnl_rank_row)", vnl_rank(i32, vnl_rank_row), 0);
  TEST("vnl_rank(zero_3x2, vnl_rank_column)", vnl_rank(i32, vnl_rank_column), 0);
  i32[0][1] = 6;
  TEST("vnl_rank(3x2)", vnl_rank(i32), 1);
  TEST("vnl_rank(3x2, vnl_rank_row)", vnl_rank(i32, vnl_rank_row), 1);
  TEST("vnl_rank(3x2, vnl_rank_column)", vnl_rank(i32, vnl_rank_column), 1);
  i32[2][0] = -1;
  TEST("vnl_rank(3x2)", vnl_rank(i32), 2);
  TEST("vnl_rank(3x2, vnl_rank_row)", vnl_rank(i32, vnl_rank_row), 2);
  TEST("vnl_rank(3x2, vnl_rank_column)", vnl_rank(i32, vnl_rank_column), 2);
  i32[1][0] = 3; i32[2][1] = 1;
  TEST("vnl_rank(3x2)", vnl_rank(i32), 2);
  TEST("vnl_rank(3x2, vnl_rank_row)", vnl_rank(i32, vnl_rank_row), 2);
  TEST("vnl_rank(3x2, vnl_rank_column)", vnl_rank(i32, vnl_rank_column), 2);
  i32[0][0] = -6; i32[1][1] = -3;
  TEST("vnl_rank(3x2)", vnl_rank(i32), 1);
  TEST("vnl_rank(3x2, vnl_rank_row)", vnl_rank(i32, vnl_rank_row), 1);
  TEST("vnl_rank(3x2, vnl_rank_column)", vnl_rank(i32, vnl_rank_column), 1);
  i32 *= 2;
  TEST("vnl_rank(3x2)", vnl_rank(i32), 1);

  // 3x3 double
  vnl_matrix<double> m33(3,3, 0.0); // all zero matrix
  TEST("zero_3x3.vnl_svd().rank()", svd_rank(m33), 0);
  TEST("vnl_rank(zero_3x3)", vnl_rank(m33), 0);
  TEST("vnl_rank(zero_3x3, vnl_rank_row)", vnl_rank(m33, vnl_rank_row), 0);
  TEST("vnl_rank(zero_3x3, vnl_rank_column)", vnl_rank(m33, vnl_rank_column), 0);
  m33[0][1] = 6.0; m33[0][2] = -2.0;
  TEST("3x3.vnl_svd().rank()", svd_rank(m33), 1);
  TEST("vnl_rank(3x3)", vnl_rank(m33), 1);
  TEST("vnl_rank(3x3, vnl_rank_row)", vnl_rank(m33, vnl_rank_row), 1);
  TEST("vnl_rank(3x3, vnl_rank_column)", vnl_rank(m33, vnl_rank_column), 1);
  m33[1][2] = -1.0; m33[1][0] = 7.0;
  TEST("3x3.vnl_svd().rank()", svd_rank(m33), 2);
  TEST("vnl_rank(3x3)", vnl_rank(m33), 2);
  TEST("vnl_rank(3x3, vnl_rank_row)", vnl_rank(m33, vnl_rank_row), 2);
  TEST("vnl_rank(3x3, vnl_rank_column)", vnl_rank(m33, vnl_rank_column), 2);
  m33[2][0] = 7.0;
  TEST("3x3.vnl_svd().rank()", svd_rank(m33), 3);
  TEST("vnl_rank(3x3)", vnl_rank(m33), 3);
  TEST("vnl_rank(3x3, vnl_rank_row)", vnl_rank(m33, vnl_rank_row), 3);
  TEST("vnl_rank(3x3, vnl_rank_column)", vnl_rank(m33, vnl_rank_column), 3);
  m33[2][1] = 6.0; m33[2][2] = -3.0;
  TEST("3x3.vnl_svd().rank()", svd_rank(m33), 2);
  TEST("vnl_rank(3x3)", vnl_rank(m33), 2);
  m33 *= 2.0;
  TEST("3x3.vnl_svd().rank()", svd_rank(m33), 2);
  TEST("vnl_rank(3x3)", vnl_rank(m33), 2);

  // 3x3 int
  vnl_matrix<int> i33(3,3, 0); // all zero matrix
  TEST("vnl_rank(int_3x3)", vnl_rank(i33), 0);
  TEST("vnl_rank(int_3x3, vnl_rank_row)", vnl_rank(i33, vnl_rank_row), 0);
  TEST("vnl_rank(int_3x3, vnl_rank_column)", vnl_rank(i33, vnl_rank_column), 0);
  i33[0][1] = 6; i33[0][2] = -2;
  TEST("vnl_rank(int_3x3)", vnl_rank(i33), 1);
  TEST("vnl_rank(int_3x3, vnl_rank_row)", vnl_rank(i33, vnl_rank_row), 1);
  TEST("vnl_rank(int_3x3, vnl_rank_column)", vnl_rank(i33, vnl_rank_column), 1);
  i33[1][2] = -1; i33[1][0] = 7;
  TEST("vnl_rank(int_3x3)", vnl_rank(i33), 2);
  TEST("vnl_rank(int_3x3, vnl_rank_row)", vnl_rank(i33, vnl_rank_row), 2);
  TEST("vnl_rank(int_3x3, vnl_rank_column)", vnl_rank(i33, vnl_rank_column), 2);
  i33[2][0] = 7;
  TEST("vnl_rank(int_3x3)", vnl_rank(i33), 3);
  TEST("vnl_rank(int_3x3, vnl_rank_row)", vnl_rank(i33, vnl_rank_row), 3);
  TEST("vnl_rank(int_3x3, vnl_rank_column)", vnl_rank(i33, vnl_rank_column), 3);
  i33[2][1] = 6; i33[2][2] = -3;
  TEST("vnl_rank(int_3x3)", vnl_rank(i33), 2);
  TEST("vnl_rank(int_3x3, vnl_rank_row)", vnl_rank(i33, vnl_rank_row), 2);
  TEST("vnl_rank(int_3x3, vnl_rank_column)", vnl_rank(i33, vnl_rank_column), 2);
  i33 *= 2;
  TEST("vnl_rank(int_3x3)", vnl_rank(i33), 2);
  TEST("vnl_rank(int_3x3, vnl_rank_row)", vnl_rank(i33, vnl_rank_row), 2);
  TEST("vnl_rank(int_3x3, vnl_rank_column)", vnl_rank(i33, vnl_rank_column), 2);
}

TESTMAIN(test_rank);
