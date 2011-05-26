// This is core/vnl/algo/tests/test_solve_qp.cxx
#include "test_util.h"
#include <vcl_iostream.h>
#include <testlib/testlib_test.h>
#include <vnl/algo/vnl_solve_qp.h>

//--------------------------------------------------------------------------------

void test_solve_qp_with_non_neg_constraints1()
{
  // Minimise |x|^2 subject to 1.x=1 and x(i)>=0
  // x(i)>=0 not relevant for this solution

  unsigned n=4;
  vnl_matrix<double> H(n,n,0.0);
  for (unsigned i=0;i<n;++i) H(i,i)=1.0;
  vnl_matrix<double> A(1,n,1.0);
  vnl_vector<double> g(n,0.0), b(1,1.0),x(n,0.0);

  // Initialise to satisfy Ax=b
  x[0]=1.0;

  vnl_solve_qp_with_non_neg_constraints(H,g,A,b,x);

  vnl_vector<double> sol(n,1.0/n);

  vcl_cout<<"Solution: "<<x<<vcl_endl;
  TEST_NEAR("|x-x_true|^2", vnl_vector_ssd(x,sol), 0, 1e-5);
}

void test_solve_qp_with_non_neg_constraints2()
{
  // Minimise 0.5|x|^2 -x.(1 -1 1 1) subject to 1.x=1 and x(i)>=0
  // x(i)>=0 not relevant for this solution

  unsigned n=4;
  vnl_matrix<double> H(n,n,0.0);
  for (unsigned i=0;i<n;++i) H(i,i)=1.0;
  vnl_matrix<double> A(1,n,1.0);
  vnl_vector<double> g(n,-1.0), b(1,1.0),x(n,0.0);
  g[1]=1.0;

  // Initialise to satisfy Ax=b
  x[0]=1.0;

  vnl_solve_qp_with_non_neg_constraints(H,g,A,b,x);

  vnl_vector<double> sol(n,1.0/3);
  sol[1]=0.0;

  vcl_cout<<"Solution: "<<x<<vcl_endl;
  TEST_NEAR("|x-x_true|^2", vnl_vector_ssd(x,sol), 0, 1e-5);
}

void test_solve_qp_non_neg_sum_one1()
{
  // Minimise |x|^2 subject to 1.x=1 and x(i)>=0
  // x(i)>=0 not relevant for this solution

  unsigned n=4;
  vnl_matrix<double> H(n,n,0.0);
  for (unsigned i=0;i<n;++i) H(i,i)=1.0;
  vnl_vector<double> g(n,0.0), x(n,0.0);

  // Initialise to satisfy sum(x)=1
  x[0]=1.0;

  vnl_solve_qp_non_neg_sum_one(H,g,x);

  vnl_vector<double> sol(n,1.0/n);

  vcl_cout<<"Solution: "<<x<<vcl_endl;
  TEST_NEAR("|x-x_true|^2", vnl_vector_ssd(x,sol), 0, 1e-5);
}

void test_solve_qp_non_neg_sum_one2()
{
  // Minimise 0.5|x|^2 -x.(1 -1 1 1) subject to 1.x=1 and x(i)>=0
  // x(i)>=0 not relevant for this solution

  unsigned n=4;
  vnl_matrix<double> H(n,n,0.0);
  for (unsigned i=0;i<n;++i) H(i,i)=1.0;
  vnl_vector<double> g(n,-1.0), x(n,0.0);
  g[1]=1.0;

  // Initialise to satisfy sum(x)=1
  x[0]=1.0;

  vnl_solve_qp_non_neg_sum_one(H,g,x);

  vnl_vector<double> sol(n,1.0/3);
  sol[1]=0.0;

  vcl_cout<<"Solution: "<<x<<vcl_endl;
  TEST_NEAR("|x-x_true|^2", vnl_vector_ssd(x,sol), 0, 1e-5);
}

extern "C" void test_solve_qp()
{
  test_solve_qp_with_non_neg_constraints1();
  test_solve_qp_with_non_neg_constraints2();
  test_solve_qp_non_neg_sum_one1();
  test_solve_qp_non_neg_sum_one2();
}

TESTMAIN(test_solve_qp);

