#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_qr.h>
#include <vnl/algo/vnl_determinant.h>
#include <vnl/vnl_det.h>

#include <testlib/testlib_test.h>

double qr_det(vnl_matrix<double> const &M)
{ return vnl_qr<double>(M).determinant(); }

void test_determinant()
{
  double eps = 1e-14;

#if 0 // commented out
  {
    double M1[1][1] = {
      { 0.95012928514718 }
    };
    vnl_matrix<double> m1(&M1[0][0],1,1);
    double d1 = M1[0][0];
    TEST_NEAR("1x1 vnl_determinant(vnl_matrix<double>)", vnl_determinant(m1), d1, eps);
    TEST_NEAR("1x1 qr_det(vnl_matrix<double>)", qr_det(m1), d1, eps);
    TEST_NEAR("1x1 vnl_determinant(double, ...)", vnl_determinant(M1[0]), d1, eps);
    vnl_matrix_fixed<double,1,1> m_1 = m1;
    TEST_NEAR("vnl_det(vnl_matrix_fixed<double,1,1>)", vnl_det(m_1), d1, eps);
  }

  {
    double N1[1][1] = {
      { -0.95012928514718 }
    };
    vnl_matrix<double> n1(&N1[0][0],1,1);
    double d1 = N1[0][0];
    TEST_NEAR("1x1 vnl_determinant(vnl_matix<double>)", vnl_determinant(n1), d1, eps);
    TEST_NEAR("1x1 qr_det(vnl_matrix<double>)", qr_det(n1), d1, eps);
    TEST_NEAR("1x1 vnl_determinant(double, ...)", vnl_determinant(N1[0]), d1, eps);
    vnl_matrix_fixed<double,1,1> n_1 = n1;
    TEST_NEAR("vnl_det(vnl_matrix_fixed<double,1,1>)", vnl_det(n_1), d1, eps);
  }
#endif
  {
    double M2[2][2] = {
      { 0.60684258354179,   0.89129896614890 },
      { 0.48598246870930,   0.76209683302739 }
    };
    vnl_matrix<double> m2(&M2[0][0],2,2);
    double d2 = qr_det(m2);
    TEST_NEAR("2x2 vnl_determinant(vnl_matix<double>)", vnl_determinant(m2), d2, eps);
    TEST_NEAR("2x2 vnl_determinant(double, ...)", vnl_determinant(M2[0], M2[1]), d2, eps);
    vnl_matrix_fixed<double,2,2> m_2 = m2;
    TEST_NEAR("vnl_det(vnl_matrix_fixed<double,2,2>)", vnl_det(m_2), d2, eps);
  }

  {
    double N2[2][2] = {
      { 0.60684258354179,   0.89129896614890 },
      { 0.48598246870930,  -0.76209683302739 }
    };
    vnl_matrix<double> n2(&N2[0][0],2,2);
    double d2 = qr_det(n2);
    TEST_NEAR("2x2 vnl_determinant(vnl_matix<double>)", vnl_determinant(n2), d2, eps);
    TEST_NEAR("2x2 vnl_determinant(double, ...)", vnl_determinant(N2[0], N2[1]), d2, eps);
    vnl_matrix_fixed<double,2,2> n_2 = n2;
    TEST_NEAR("vnl_det(vnl_matrix_fixed<double,2,2>)", vnl_det(n_2), d2, eps);
  }

  {
    double M3[3][3] = {
      { 0.45646766516834,   0.44470336435319,   0.92181297074480 },
      { 0.01850364324822,   0.61543234810009,   0.73820724581067 },
      { 0.82140716429525,   0.79193703742704,   0.17626614449462 }
    };
    vnl_matrix<double> m3(&M3[0][0],3,3);
    double d3 = qr_det(m3);
    TEST_NEAR("3x3 vnl_determinant(vnl_matix<double>)", vnl_determinant(m3), d3, eps);
    TEST_NEAR("3x3 vnl_determinant(double, ...)", vnl_determinant(M3[0], M3[1], M3[2]), d3, eps);
    vnl_matrix_fixed<double,3,3> m_3 = m3;
    TEST_NEAR("vnl_det(vnl_matrix_fixed<double,3,3>)", vnl_det(m_3), d3, eps);
  }

  {
    double N3[3][3] = {
      { 0.45646766516834,   0.44470336435319,  -0.92181297074480 },
      { 0.01850364324822,   0.61543234810009,  -0.73820724581067 },
      { 0.82140716429525,   0.79193703742704,   0.17626614449462 }
    };
    vnl_matrix<double> n3(&N3[0][0],3,3);
    double d3 = qr_det(n3);
    TEST_NEAR("3x3 vnl_determinant(vnl_matix<double>)", vnl_determinant(n3), d3, eps);
    TEST_NEAR("3x3 vnl_determinant(double, ...)", vnl_determinant(N3[0], N3[1], N3[2]), d3, eps);
    vnl_matrix_fixed<double,3,3> n_3 = n3;
    TEST_NEAR("vnl_det(vnl_matrix_fixed<double,3,3>)", vnl_det(n_3), d3, eps);
  }

  {
    double M4[4][4] = {
      { 0.40570621306210,   0.89364953091353,   0.00986130066092,   0.60379247919382 },
      { 0.93546969910761,   0.05789130478427,   0.13889088195695,   0.27218792496996 },
      { 0.91690443991341,   0.35286813221700,   0.20276521856027,   0.19881426776106 },
      { 0.41027020699095,   0.81316649730376,   0.19872174266149,   0.01527392702904 }
    };
    vnl_matrix<double> m4(&M4[0][0],4,4);
    double d4 = qr_det(m4);
    TEST_NEAR("4x4 vnl_determinant(vnl_matix<double>)", vnl_determinant(m4), d4, eps);
    TEST_NEAR("4x4 vnl_determinant(double, ...)", vnl_determinant(M4[0],M4[1],M4[2],M4[3]), d4, eps);
    vnl_matrix_fixed<double,4,4> m_4 = m4;
    TEST_NEAR("vnl_det(vnl_matrix_fixed<double,4,4>)", vnl_det(m_4), d4, eps);
  }

  {
    double N4[4][4] = {
      { 0.40570621306210,  -0.89364953091353,   0.00986130066092,  -0.60379247919382 },
      { 0.93546969910761,   0.05789130478427,   0.13889088195695,   0.27218792496996 },
      { 0.91690443991341,  -0.35286813221700,   0.20276521856027,  -0.19881426776106 },
      { 0.41027020699095,   0.81316649730376,   0.19872174266149,   0.01527392702904 }
    };
    vnl_matrix<double> n4(&N4[0][0],4,4);
    double d4 = qr_det(n4);
    TEST_NEAR("4x4 vnl_determinant(vnl_matix<double>)", vnl_determinant(n4), d4, eps);
    TEST_NEAR("4x4 vnl_determinant(double, ...)", vnl_determinant(N4[0],N4[1],N4[2],N4[3]), d4, eps);
    vnl_matrix_fixed<double,4,4> n_4 = n4;
    TEST_NEAR("vnl_det(vnl_matrix_fixed<double,4,4>)", vnl_det(n_4), d4, eps);

    double N5[5][5] = {
      { 1, 0,        0,         0,        0       },
      { 0, N4[0][0], N4[0][1], N4[0][2], N4[0][3] },
      { 0, N4[1][0], N4[1][1], N4[1][2], N4[1][3] },
      { 0, N4[2][0], N4[2][1], N4[2][2], N4[2][3] },
      { 0, N4[3][0], N4[3][1], N4[3][2], N4[3][3] }
    };
    vnl_matrix<double> n5(&N5[0][0],5,5);
    double d5 = qr_det(n5);
    TEST_NEAR("5x5 qr_det equals 4x4 one", d5, d4, eps);
    TEST_NEAR("5x5 vnl_determinant(vnl_matix<double>)", vnl_determinant(n5), d5, eps);
    vnl_matrix_fixed<double,5,5> n_5 = n5;
    TEST_NEAR("vnl_determinant(vnl_matrix_fixed<double,5,5>)", vnl_determinant(n_5), d5, eps);
  }

  {
    int M6[3][3] = {
      { 2, 0, 0 },
      { 0, 1, 0 },
      { 0, 0, 5 } };
    vnl_matrix<int> m6(&M6[0][0],3,3);
    TEST_NEAR("3x3 vnl_determinant(vnl_matix<int>)", vnl_determinant(m6), 10, eps);
  }

  {
    int M7[6][6] = {
      { 2, 0, 0, 0, 0, 0 },
      { 0, 1, 0, 0, 0, 0 },
      { 0, 0, 5, 0, 0, 0 },
      { 0, 0, 0, 4, 0, 0 },
      { 0, 0, 0, 0, 2, 0 },
      { 0, 0, 0, 0, 0, 6 } };
    vnl_matrix<int> m7(&M7[0][0],6,6);
    TEST_NEAR("6x6 vnl_determinant(vnl_matix<int>)", vnl_determinant(m7), 2*1*5*4*2*6, eps);
  }
}

TESTMAIN(test_determinant);
