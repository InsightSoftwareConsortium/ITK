// This is core/vnl/algo/tests/test_matrix_update.cxx
#include <iostream>
#include "test_util.h"
// not used? #include <vcl_compiler.h>
#include <testlib/testlib_test.h>
#include <vnl/algo/vnl_matrix_update.h>

//--------------------------------------------------------------------------------

extern "C" void test_matrix_update()
{
  unsigned nr=5,nc=7;
  vnl_matrix<double> M(nr,nc), true_M(nr,nc);
  vnl_matrix<double> Ma(nr,1),Mb(1,nc);
  vnl_vector<double> a(nr),b(nc);

  for (unsigned i=0;i<nr;++i) { a[i]=1+i; Ma(i,0)=a[i]; }
  for (unsigned i=0;i<nc;++i) { b[i]=i*i-1; Mb(0,i)=b[i]; }

  M.fill(0.0);
  vnl_matrix_update(M,a,b);
  true_M = Ma*Mb;

  TEST_NEAR("M = a*b'", (M-true_M).absolute_value_max(),0.0,1e-6);

  vnl_matrix_update(M,a,b);
  TEST_NEAR("M2 = 2a*b'", (M-(true_M*2)).absolute_value_max(),0.0,1e-6);
}

TESTMAIN(test_matrix_update);

