#include <vcl_iostream.h>

#include <vnl/vnl_test.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/algo/vnl_qr.h>
#include <vnl/algo/vnl_determinant.h>

double qr_det(vnl_matrix<double> const &M) {
  return vnl_qr<double>(M).determinant();
}

void test_determinant() {
  double eps = 1e-8;

  { 
    double M1[1*1] = {
      0.95012928514718
    };
    double const * const M1_[]={M1};
    AssertNear("M1", vnl_determinant(M1_, 1), qr_det(vnl_matrix<double>(M1,1,1)), eps);
  }

  {
    double M2[2*2] = {
      0.60684258354179,   0.89129896614890,
      0.48598246870930,   0.76209683302739
    };
    double const * const M2_[]={M2, M2+2};
    AssertNear("M2", vnl_determinant(M2_, 2), qr_det(vnl_matrix<double>(M2,2,2)), eps);
  }

  {
    double M3[3*3] = {
      0.45646766516834,   0.44470336435319,   0.92181297074480,
      0.01850364324822,   0.61543234810009,   0.73820724581067,
      0.82140716429525,   0.79193703742704,   0.17626614449462
    };
    double const * const M3_[]={M3, M3+3, M3+6};
    AssertNear("M3", vnl_determinant(M3_, 3), qr_det(vnl_matrix<double>(M3,3,3)), eps);
  }

  {
    double M4[4*4] = {
      0.40570621306210,   0.89364953091353,   0.00986130066092,   0.60379247919382,
      0.93546969910761,   0.05789130478427,   0.13889088195695,   0.27218792496996,
      0.91690443991341,   0.35286813221700,   0.20276521856027,   0.19881426776106,
      0.41027020699095,   0.81316649730376,   0.19872174266149,   0.01527392702904
    };
    double const * const M4_[]={M4, M4+4, M4+8, M4+12};
    AssertNear("M4", vnl_determinant(M4_, 4), qr_det(vnl_matrix<double>(M4,4,4)), eps);
  }
}

TESTMAIN(test_determinant);
