// @author fsm

#include <iostream>
#include <cmath>
#include <vcl_compiler.h>

#include <vnl/vnl_double_3.h>
#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_matrix_exp.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_rotation_matrix.h>
#include <vnl/vnl_cross_product_matrix.h>
#include <testlib/testlib_test.h>

void test_matrix_exp()
{
  vnl_double_3 v(1.233,-0.572,0.777);

  vnl_cross_product_matrix X(v);
  vnl_matlab_print(std::cout, X, "[v]");

  vnl_double_3x3 expX = vnl_matrix_exp(static_cast<vnl_double_3x3>(X));
  vnl_matlab_print(std::cout, expX, "matrix exp([v])");
  TEST_NEAR("expX(0,0)", expX(0,0), 0.6221833130, 1e-10);
  TEST_NEAR("expX(0,1)", expX(0,1),-0.7825192869, 1e-10);
  TEST_NEAR("expX(1,1)", expX(1,1), 0.1379544126, 1e-10);
  TEST_NEAR("expX(2,2)", expX(2,2), 0.2501918781, 1e-10);

  vnl_double_3x3 rotv = vnl_rotation_matrix(v);
  vnl_matlab_print(std::cout, rotv, "rotate exp([v])");
  TEST_NEAR("rotv == expX", (rotv-expX).fro_norm(), 0.0, 1e-10);
}

TESTMAIN(test_matrix_exp);
