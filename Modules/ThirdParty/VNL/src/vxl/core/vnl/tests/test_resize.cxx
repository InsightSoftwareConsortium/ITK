// This is core/vnl/tests/test_resize.cxx
#include <iostream>
#include <testlib/testlib_test.h>
// \author fsm
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

static void test_size()
{
  vnl_vector<double> X(3);

  X.fill(2);
  TEST("fill 2", X(0)+X(1)+X(2), 6.0);
  std::cout << "X = " << X << std::endl;

  X.set_size(5);
  TEST("size", X.size(), 5);
}

static void test_rows_cols()
{
  vnl_matrix<double> M(3, 4);

  M.fill(2);
  TEST("fill 2", M(0,0)+M(1,1)+M(2,2)+M(2,3), 8.0);
  std::cout << "M =\n" << M << std::endl;

  M.set_size(5,7);
  TEST("size: rows", M.rows(), 5);
  TEST("size: cols", M.cols(), 7);
}

static void test_resize()
{
  test_size();
  test_rows_cols();
}

TESTMAIN(test_resize);
