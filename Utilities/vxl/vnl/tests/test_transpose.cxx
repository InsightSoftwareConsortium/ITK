#include <vcl_iostream.h>

#include <testlib/testlib_test.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matlab_print.h>

void test_transpose()
{
  vnl_matrix<double> X(10, 2);
  for (unsigned int i=0; i<X.rows(); ++i)
    for (unsigned int j=0; j<X.cols(); ++j)
      X[i][j] = (i+1)*3 + (j+1)*(j+i);

  vnl_matrix<double> old_X(X);

  vnl_matlab_print(vcl_cout, X, "X");

  X.inplace_transpose();

  vnl_matlab_print(vcl_cout, X, "X");

  TEST ("X == old_X.transpose()", X == old_X.transpose(), true);

  X.inplace_transpose();

  vnl_matlab_print(vcl_cout, X, "X");

  TEST ("X == old_X", X == old_X, true);
}

TESTMAIN(test_transpose);
