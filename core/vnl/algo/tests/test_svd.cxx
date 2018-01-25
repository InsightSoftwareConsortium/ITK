// This is core/vnl/algo/tests/test_svd.cxx
#include <iostream>
#include <complex>
#include "test_util.h"
//:
// \file
#include <testlib/testlib_test.h>
#include <vcl_compiler.h>
#include <vcl_cfloat.h>

#include <vnl/vnl_matrix.h>
#include <vnl/vnl_double_3.h>
#include <vnl/vnl_random.h>
#include <vnl/algo/vnl_svd.h>


template <class T, class S> static
void test_hilbert(T /*dummy*/, char const* type, S residual)
{
  std::cout << "----- Testing svd<"<<type<<">(Hilbert_3x3) -----" << std::endl;
  typedef typename vnl_numeric_traits<T>::abs_t abs_t;
  // Test inversion and recomposition of 5x5 hilbert matrix
  vnl_matrix<T> H(5,5);
  for (int i = 0; i < 5; ++i)
    for (int j = 0; j < 5; ++j)
      H(i,j) = T(1) / T(abs_t(i+j+1)); // sic, because i,j are zero based

  std::cout << "H = <"<<type<<">[ " << H << "]\n";

  vnl_svd<T> svd(H);

  std::cout << "rcond(H) = " << svd.well_condition() << std::endl;

  vnl_matrix<T> Hinv = svd.inverse();

  vnl_matrix<T> X = Hinv * H;

  std::cout << "H*inv(H) = " << X << std::endl;

  vnl_matrix<T> I(5,5);
  I = 0.0;
  I.fill_diagonal(1.0);

  vnl_matrix<T> res = X - I;
  TEST_NEAR("Hilbert recomposition residual", res.fro_norm(), 0, residual);
}

//: Test recovery of parameters of least-squares parabola fit.
static void test_ls()
{
  std::cout << "----- Testing svd on a Least Squares problem -----" << std::endl;
  double a = 0.15;
  double b = 1.2;
  double c = 3.1;

  // Generate parabola design matrix
  vnl_matrix<double> D(100, 3);
  for (int n = 0; n < 100; ++n)
  {
    double x = n;
    D(n, 0) = x*x;
    D(n, 1) = x;
    D(n, 2) = 1.0;
  }

  // Generate Y vector
  vnl_vector<double> y(100);
  for (int n = 0; n < 100; ++n)
  {
    double x = n;
    double fx = a * x * x + b * x + c;
    // Add sawtooth "noise"
    y(n) = fx + (n%4 - 2) / 10.0;
  }
  std::cout << "y = [" << y << "]\n";

  // Extract vnl_svd<double>
  vnl_svd<double> svd(D);

  // Solve for parameters
  vnl_double_3 A = svd.solve(y);
  std::cout << "A = " << A << '\n';

  vnl_double_3 T(a,b,c);
  TEST_NEAR("Least squares residual", (A - T).squared_magnitude(), 0, 0.005);
}

//: Test nullspace extraction of rank=2 3x4 matrix.
static void test_pmatrix()
{
  double pdata[] = {
    2, 0, 0, 0,
    3, 10, 5, 5,
    5, 12, 6, 6,
  };
  vnl_matrix<double> P(pdata, 3,4);
  vnl_svd<double> svd(P, 1e-8);

  vnl_matrix<double> res = svd.recompose() - P;
  TEST_NEAR("PMatrix recomposition residual", res.fro_norm(), 0, 1e-12);
  std::cout << " Inv = " << svd.inverse() << std::endl;

  TEST("singularities = 2", svd.singularities(), 2);
  TEST("rank = 2", svd.rank(), 2);

  vnl_matrix<double> N = svd.nullspace();
  TEST("nullspace dimension", N.columns(), 2);
  std::cout << "null(P) =\n" << N << std::endl;

  vnl_matrix<double> PN = P*N;
  std::cout << "P * null(P) =\n" << PN << std::endl;
  TEST_NEAR("P nullspace residual", PN.fro_norm(), 0, 1e-12);

  vnl_vector<double> n = svd.nullvector();
  TEST_NEAR("P nullvector residual", (P*n).magnitude(), 0, 1e-12);

  vnl_vector<double> l = svd.left_nullvector();
  std::cout << "left_nullvector(P) = " << l << std::endl;
  TEST_NEAR("P left nullvector residual", (l*P).magnitude(), 0, 1e-12);
}

static void test_I()
{
  double Idata[] = {
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
  };
  vnl_matrix<double> P(3, 4, 12, Idata);
  vnl_svd<double> svd(P);
  std::cout << svd;

  vnl_vector_fixed<double, 4> w_expected(1, 1, 1, 0);
  TEST_NEAR ("Singular values", vnl_vector_ssd(w_expected, svd.W().diagonal().as_ref()), 0, 1e-16);
}

template <class T>
void test_svd_recomposition(char const *type, double maxres, T* /* tag */, vnl_random &rng)
{
  // Test inversion of 5x5 matrix of T :
  std::cout << "----- Testing vnl_svd<" << type << "> recomposition -----\n";

  vnl_matrix<T> A(5,5);
  test_util_fill_random(A.begin(), A.end(), rng);

  std::cout << "A = [\n" << A << "]\n";
  vnl_svd<T> svd(A);

  vnl_matrix<T> B=svd.recompose();
  std::cout << "B = [\n" << B << "]\n";

  double residual=(A - B).fro_norm();
  TEST_NEAR("vnl_svd<float> recomposition residual", residual, 0, maxres);
}

#include <vnl/vnl_matlab_print.h>

template <class T> static
void test_nullvector(char const *type, double max_err, T *, vnl_random &rng)
{
  int n = 5;
  vnl_matrix<T> A(n, n+1);
  test_util_fill_random(A.begin(), A.end(), rng);
  vnl_svd<T> svd(A);
  vnl_vector<T>  x = svd.nullvector();
  vnl_vector<T> Ax = A*x;
  std::cout << __FILE__ ": type = " << type << std::endl;
  vnl_matlab_print(std::cout, A, "A", vnl_matlab_print_format_long);
  std::cout << __FILE__ ": || x|| = " <<  x.two_norm() << std::endl
           << __FILE__ ": ||Ax|| = " << Ax.two_norm() << std::endl;
  TEST_NEAR("||Ax||", Ax.two_norm(), 0.0, max_err);
}

// Driver
void test_svd()
{
  vnl_random rng(9667566);
  test_hilbert(double(), "double", 1.1e-10);
  test_hilbert(float(), "float", float(0.025));
  test_hilbert(std::complex<double>(), "std::complex<double>", double(4.4e-10));
  test_hilbert(std::complex<float>(), "std::complex<float>", float(0.04));
  test_ls();
  test_pmatrix();
  test_I();
  test_svd_recomposition("float",              1e-5 , (float*)VXL_NULLPTR, rng);
  test_svd_recomposition("double",             1e-10, (double*)VXL_NULLPTR, rng);
  test_svd_recomposition("std::complex<float>",  1e-5 , (std::complex<float>*)VXL_NULLPTR, rng);
  test_svd_recomposition("std::complex<double>", 1e-10, (std::complex<double>*)VXL_NULLPTR, rng);

  test_nullvector("float",               5e-7,  (float*)VXL_NULLPTR, rng);
  test_nullvector("double",              5e-15, (double*)VXL_NULLPTR, rng);
  test_nullvector("std::complex<float>",  5e-7,  (std::complex<float>*)VXL_NULLPTR, rng);
  test_nullvector("std::complex<double>", 5e-15, (std::complex<double>*)VXL_NULLPTR, rng);
}

TESTMAIN(test_svd);
