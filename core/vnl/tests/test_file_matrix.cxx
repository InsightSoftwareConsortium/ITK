/*
  fsm
*/
#include <iostream>
#include <vcl_compiler.h>
#include <testlib/testlib_root_dir.h>

#include <testlib/testlib_test.h>
#include <vnl/vnl_file_matrix.h>
#include <vnl/vnl_matlab_print.h>

void test_file_matrix()
{
  vnl_file_matrix<double> H((testlib_root_dir()+
    "/core/vnl/tests/data_3x3_matrix").c_str());

  vnl_matlab_print(std::cout, H, "H");
  TEST("file_matrix 3x3", H.rows(), 3);
  TEST("file_matrix 3x3", H.cols(), 3);

  TEST_NEAR("data(1,2)", H(1,2), 0.0185, 1e-12);

  H /= H[0][0];

  vnl_matlab_print(std::cout, H, "H");
  TEST_NEAR("file_matrix 3x3", H(0,0), 1.0, 1e-12);
}

TESTMAIN(test_file_matrix);
