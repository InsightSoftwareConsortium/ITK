// This is core/vnl/algo/tests/test_algo.cxx
#include <testlib/testlib_test.h>
//:
// \file
// \brief test miscellaneous classes and functions in vnl/algo.
// This file contains short tests for several algorithms in vnl/algo
// that are not tested more extensively in separate test files.
// Currently, the following classes or functions are tested here:
// - vnl_adjugate
// - vnl_svd_economy
// - vnl_matrix_inverse
// - vnl_fft_1d
// - vnl_fft_2d
// - vnl_orthogonal_complement
// - vnl_conjugate_gradient
// - vnl_lbfgs
// - vnl_powell
// - vnl_lsqr
// - vnl_discrete_diff_fwd
// - vnl_discrete_diff_sym
// - vnl_generalized_schur
//
// \author Peter Vanroose, KULeuven/ESAT.
// \date 20 September 2003

#include <vcl_complex.h>

#include <vnl/algo/vnl_adjugate.h>
#include <vnl/algo/vnl_conjugate_gradient.h>
#include <vnl/algo/vnl_discrete_diff.h>
#include <vnl/algo/vnl_fft_1d.h>
#include <vnl/algo/vnl_fft_2d.h>
#include <vnl/algo/vnl_generalized_schur.h>
#include <vnl/algo/vnl_lbfgs.h>
#include <vnl/algo/vnl_lbfgsb.h>
#include <vnl/algo/vnl_lsqr.h>
#include <vnl/algo/vnl_matrix_inverse.h>
#include <vnl/algo/vnl_orthogonal_complement.h>
#include <vnl/algo/vnl_powell.h>
#include <vnl/algo/vnl_svd_economy.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/vnl_sparse_matrix_linear_system.h>
#include <vnl/vnl_least_squares_function.h>

static void test_adjugate()
{
  int data[] = { 1, -1, 1, -1,  1, 1, -1, -1,  1, 1, 1, 1,  1, -1, -1, 1 };
  vnl_matrix<int> m(data,4,4);
  vnl_matrix<int> m_adj = vnl_adjugate(m);
  vnl_matrix<int> identity(4,4); identity.set_identity();
  TEST("vnl_adjugate", (m*m_adj-16*identity).array_inf_norm(), 0);
}

static void test_matrix_inverse()
{
  double data[] = {1.,-1.,1.,-1., 1.,1.,-1.,-1., 1.,1.,1.,1., 1.,-1.,-1.,1.};
  vnl_matrix<double> m(data,4,4);
  vnl_svd_economy<double> svde(m); vnl_matrix<double> V = svde.V();
  vnl_svd<double> svd(m); vnl_matrix<double> V0 = svd.V();
  TEST_NEAR("vnl_svd_economy", V[0][1], V0[0][1], 1e-6);

  vnl_matrix<double> inv = vnl_matrix_inverse<double>(m);
  vnl_matrix<double> identity(4,4); identity.set_identity();
  TEST_NEAR("vnl_matrix_inverse", (m*inv-identity).array_inf_norm(), 0, 1e-6);
}

static void test_fft()
{
  vcl_vector<vcl_complex<double> > v(256); for (int i=0; i<256; ++i) v[i]=0.5+i;
  vnl_fft_1d<double> fft1d(256); fft1d.fwd_transform(v); fft1d.bwd_transform(v);
  TEST_NEAR("vnl_fft_1d", v[10], 256*10.5, 1e-6);
  vnl_matrix<vcl_complex<double> > m(10,9);
  for (int i=0; i<10; ++i) for (int j=0; j<9; ++j) m[i][j]=0.5+i+j;
  vnl_fft_2d<double> fft2d(10,9); fft2d.fwd_transform(m); fft2d.bwd_transform(m);
  TEST_NEAR("vnl_fft_2d", m[5][5], 10*9*10.5, 1e-6);
}

static void test_orthogonal_complement()
{
  vnl_vector<double> v(20); for (int i=0; i<20; ++i) v[i]=0.5+i;
  vnl_matrix<double> oc = vnl_orthogonal_complement(v);
  TEST("vnl_orthogonal_complement", oc[0][0]<0 && oc[0][1]==0 && oc[1][0]>0, true);
}

class F_test_powell : public vnl_cost_function
{
 public:
  // Local min near (0,0) is at (1,1) and has value 1.
  F_test_powell() : vnl_cost_function(2) {}
  double f(vnl_vector<double> const& x)
    { double u=x[0]-x[1]*x[1], v=x[1]-1; return u*u+v*v+1; }
  void gradf(vnl_vector<double> const& x, vnl_vector<double>& g)
    { g[0]=2*x[0]-2*x[1]*x[1]; g[1]=4*x[1]*x[1]*x[1]-4*x[0]*x[1]+2*x[1]-2; }
};

static void test_powell()
{
  F_test_powell f; // local minimum is 1 in (1,1).
  vnl_vector<double> x(2); x[0]=x[1]=0.0;
  vnl_conjugate_gradient cg(f); cg.minimize(x);
  TEST_NEAR("vnl_conjugate_gradient", x[0], 1.0, 1e-5);

  vnl_lbfgs lbfgs(f); x[0]=x[1]=0.0; lbfgs.minimize(x);
  TEST_NEAR("vnl_lbfgs", x[1], 1.0, 1e-6);

  {
  // Local min near (0,0) with (x,y) bounded to [-0.5,+0.5] is
  //   at (0.25, 0.5) with value 1.25.
  vnl_lbfgsb lbfgsb(f); x[0]=x[1]=0.0;
  vnl_vector<double> l(2); l[0] = -0.5; l[1] = -0.5;
  vnl_vector<double> u(2); u[0] = +0.5; u[1] = +0.5;
  vnl_vector<long> nbd(2); nbd[0] = 3; nbd[1] = 3;
  lbfgsb.set_lower_bound(l);
  lbfgsb.set_upper_bound(u);
  lbfgsb.set_bound_selection(nbd);
  lbfgsb.minimize(x);
  TEST_NEAR("vnl_lbfgsb", x[0], 0.25, 1e-6);
  }

  vnl_powell powell(&f); x[0]=x[1]=0.0; powell.minimize(x);
  TEST_NEAR("vnl_powell", f.f(x), 1.0, 1e-6);
}

static void test_lsqr()
{
  vnl_sparse_matrix<double> A(2,2); vnl_vector<double> b(2);
  A(0,0)=2; A(0,1)=3; A(1,0)=4; A(1,1)=5; b[0]=5; b[1]=9;
  vnl_sparse_matrix_linear_system<double> ls(A,b);
  vnl_vector<double> x(2); x[0]=x[1]=0.0;
  vnl_lsqr lsqr(ls); lsqr.minimize(x);
  TEST_NEAR("vnl_lsqr", x[1], 1.0, 1e-6);
}

class F_test_discrete_diff : public vnl_least_squares_function
{
 public:
  F_test_discrete_diff(): vnl_least_squares_function(2, 2, no_gradient) {}
  void f(vnl_vector<double> const& x, vnl_vector<double>& fx) { fx[0]=x[0]-x[1]*x[1]; fx[1]=x[1]-1; }
};

static void test_discrete_diff()
{
  F_test_discrete_diff f;
  double h = 0.1;
  vnl_vector<double> x(2); x[0]=5.0; x[1]=9.0;
  vnl_matrix<double> J(2,2);
  vnl_discrete_diff_fwd(&f, h, x, J);
  TEST_NEAR("vnl_discrete_diff_fwd", J(0,1), -18.1, 1e-6);
  vnl_discrete_diff_sym(&f, h, x, J);
  TEST_NEAR("vnl_discrete_diff_sym", J(0,1), -18, 1e-6);
}

static void test_generalized_schur()
{
  vnl_matrix<float> A(4,4,0.0f), B(4,4,0.0f), L(4,4,1.0f), R(4,4,1.0f);
  vnl_vector<float> ar(4,0.0f), ai(4,0.0f), b(4,0.0f);
  vnl_generalized_schur(&A, &B, &ar, &ai, &b, &L, &R);
  TEST("vnl_generalized_schur", true, true);
}

void test_algo()
{
  test_adjugate();
  test_matrix_inverse();
  test_fft();
  test_orthogonal_complement();
  test_powell();
  test_lsqr();
  test_discrete_diff();
  test_generalized_schur();
}

TESTMAIN(test_algo);
