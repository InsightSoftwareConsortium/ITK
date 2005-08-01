// This is core/vnl/tests/test_resize.cxx
#include <testlib/testlib_test.h>
// \author fsm
#include <vcl_iostream.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

static void test_size()
{
  vnl_vector<double> X(3);

  X.fill(2);
  TEST("fill 2", X(0)+X(1)+X(2), 6.0);
  vcl_cout << "X = " << X << vcl_endl;

  X.set_size(5);
  TEST("size", X.size(), 5);
#if 0
  // After resize, old data is lost, so the following test must fail:
  TEST("resize", X(0)+X(1)+X(2)+X(3)+X(4), 6.0);
#endif
}

static void test_rows_cols()
{
  vnl_matrix<double> M(3, 4);

  M.fill(2);
  TEST("fill 2", M(0,0)+M(1,1)+M(2,2)+M(2,3), 8.0);
  vcl_cout << "M =\n" << M << vcl_endl;

  M.set_size(5,7);
  TEST("size: rows", M.rows(), 5);
  TEST("size: cols", M.cols(), 7);
#if 0
  // After resize, old data is lost, so the following test must fail:
  TEST("resize", M(0,0)+M(1,1)+M(2,2)+M(3,3)+M(4,4), 6.0);
#endif
}

static void test_resize()
{
  test_size();
  test_rows_cols();
}

TESTMAIN(test_resize);
