/*
  fsm@robots.ox.ac.uk
*/
#include <vcl_iostream.h>
#include <vcl_cmath.h>

#include <vnl/vnl_test.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#if 1
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
    vcl_cout << "X = " << X << vcl_endl;
    
    vnl_resize_v(X, 5);
    vcl_cout << "X = " << X << vcl_endl;
  }
  
  {
    vnl_matrix<double> M(3, 4);
    
    M.fill(2);
    vcl_cout << "M = " << vcl_endl << M << vcl_endl;
    
    vnl_resize_m(M, 5, 7);
    vcl_cout << "M = " << vcl_endl << M << vcl_endl;
  }
}

TESTMAIN(test_resize);
