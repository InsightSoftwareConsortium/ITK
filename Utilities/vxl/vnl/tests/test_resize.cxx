// This is vxl/vnl/tests/test_resize.cxx
/*
  fsm
*/
#include <vcl_iostream.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <testlib/testlib_test.h>
#if 0
# include <vnl/vnl_resize.h>
# define vnl_resize_v vnl_resize
# define vnl_resize_m vnl_resize
#else
# define vnl_resize_v(v, n) (v).resize(n)
# define vnl_resize_m(A, m, n) (A).resize(m, n)
#endif

void test_resize()
{
  {
    vnl_vector<double> X(3);

    X.fill(2);
    TEST("fill 2", X(0)+X(1)+X(2), 6.0);
    vcl_cout << "X = " << X << vcl_endl;

    vnl_resize_v(X, 5);
    TEST("size", X.size(), 5);
    // After resize, old data is lost, so the following test must fail:
//  TEST("resize", X(0)+X(1)+X(2)+X(3)+X(4), 6.0);
    // avoid UMR from output
    X(0) = X(1) = X(2) = X(3) = X(4) = 0.0;
    vcl_cout << "X = " << X << vcl_endl;
  }

  {
    vnl_matrix<double> M(3, 4);

    M.fill(2);
    TEST("fill 2", M(0,0)+M(1,1)+M(2,2)+M(2,3), 8.0);
    vcl_cout << "M = " << vcl_endl << M << vcl_endl;

    vnl_resize_m(M, 5, 7);
    TEST("size", M.rows(), 5);
    TEST("size", M.cols(), 7);
    // After resize, old data is lost, so the following test must fail:
//  TEST("resize", M(0,0)+M(1,1)+M(2,2)+M(3,3)+M(4,4), 6.0);
    vcl_cout << "M = " << vcl_endl << M << vcl_endl;
  }
}

TESTMAIN(test_resize);
