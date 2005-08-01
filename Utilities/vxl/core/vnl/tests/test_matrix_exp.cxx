// @author fsm

#include <vcl_iostream.h>
#include <vcl_cmath.h> // for vcl_abs()

#include <vnl/vnl_double_3.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_exp.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/vnl_rotation_matrix.h>
#include <vnl/vnl_cross_product_matrix.h>
#include <testlib/testlib_test.h>

void test_matrix_exp()
{
  vnl_double_3 v(1.233,-0.572,0.777);

  vnl_matrix<double> X = vnl_cross_product_matrix(v).as_ref();
  vnl_matlab_print(vcl_cout, X, "[v]");

  vnl_matrix<double> expX = vnl_matrix_exp(X);
  vnl_matlab_print(vcl_cout, expX, "matrix exp([v])");
  testlib_test_assert("expX(0,0)", vcl_abs(expX(0,0)-0.6221833130) < 1e-10);
  testlib_test_assert("expX(0,1)", vcl_abs(expX(0,1)+0.7825192869) < 1e-10);
  testlib_test_assert("expX(1,1)", vcl_abs(expX(1,1)-0.1379544126) < 1e-10);
  testlib_test_assert("expX(2,2)", vcl_abs(expX(2,2)-0.2501918781) < 1e-10);

  vnl_matrix<double> rotv = vnl_rotation_matrix(v);
  vnl_matlab_print(vcl_cout, rotv, "rotate exp([v])");
  testlib_test_assert("rotv == expX", (rotv-expX).fro_norm() < 1e-10);
}

TESTMAIN(test_matrix_exp);
