/*
  fsm@robots.ox.ac.uk
*/
#include <vcl_iostream.h>

#include <vnl/vnl_test.h>
#include <vnl/vnl_file_matrix.h>
#include <vnl/vnl_matlab_print.h>

void test_file_matrix()
{
  vnl_file_matrix<double> H("data_3x3_matrix");

  vnl_matlab_print(vcl_cout, H, "H");
  TEST("file_matrix 3x3", H.rows(), 3);
  TEST("file_matrix 3x3", H.cols(), 3);

  H /= H[0][0];

  vnl_matlab_print(vcl_cout, H, "H");
  TEST("file_matrix 3x3", H(0,0), 1.0);
}

TESTMAIN(test_file_matrix);
