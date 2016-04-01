// This is core/vnl/algo/tests/test_svd_fixed.cxx
#include <iostream>
#include <complex>
#include <ctime>
#include "test_util.h"
//:
// \file
#include <testlib/testlib_test.h>

#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_inverse.h>
#include <vnl/algo/vnl_svd_fixed.h>
#include <vnl/algo/vnl_svd.h>

#include <vcl_compiler.h>


template <class T, class S> static
void test_hilbert(T /*dummy*/, char const* type, S residual)
{
  std::cout << "----- Testing svd_fixed<"<<type<<">(Hilbert_3x3) -----" << std::endl;
  typedef typename vnl_numeric_traits<T>::abs_t abs_t;
  // Test inversion and recomposition of 3x3 hilbert matrix
  vnl_matrix_fixed<T,3,3> H;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      H(i,j) = T(1) / T(abs_t(i+j+1)); // sic, because i,j are zero based

  std::cout << "H = <"<<type<<">[ " << H << "]\n";

  vnl_svd_fixed<T,3,3> svd(H);

  std::cout << "rcond(H) = " << svd.well_condition() << std::endl;

  vnl_matrix_fixed<T,3,3> Hinv = svd.inverse();

  vnl_matrix_fixed<T,3,3> X = Hinv * H;

  std::cout << "H*inv(H) = " << X << std::endl;

  vnl_matrix_fixed<T,3,3> I;
  I = 0.0;
  I.fill_diagonal(1.0);

  vnl_matrix_fixed<T,3,3> res = X - I;
  TEST_NEAR("Hilbert recomposition residual", res.fro_norm(), 0, residual);
}


//: Test nullspace extraction of rank=2 3x4 matrix.
static void test_pmatrix()
{
  std::cout << "----- Testing nullspace extraction of rank=2 3x4 matrix -----" << std::endl;

  double pdata[] = {
    2, 0, 0, 0,
    3, 10, 5, 5,
    5, 12, 6, 6,
  };
  vnl_matrix_fixed<double, 3, 4> P(pdata);
  vnl_svd_fixed<double, 3, 4> svd(P, 1e-8);

  vnl_matrix_fixed<double, 3, 4> res = svd.recompose() - P;
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

  vnl_vector_fixed<double, 4> n = svd.nullvector();
  TEST_NEAR("P nullvector residual", (P*n).magnitude(), 0, 1e-12);

  vnl_vector_fixed<double, 3> l = svd.left_nullvector();
  std::cout << "left_nullvector(P) = " << l << std::endl;
  TEST_NEAR("P left nullvector residual", (l*P).magnitude(), 0, 1e-12);
}

static void test_I()
{
  std::cout << "----- testing vnl_svd(I) -----\n";
  double Idata[] = {
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
  };
  vnl_matrix_fixed<double, 3, 4> P(Idata);
  vnl_svd_fixed<double, 3, 4> svd(P);
  std::cout << svd;

  vnl_vector_fixed<double, 4> w_expected(1, 1, 1, 0);
  TEST_NEAR ("Singular values", vnl_vector_ssd(w_expected, svd.W().diagonal().as_ref()), 0, 1e-16);
}

template <class T>
void test_svd_recomposition(char const *type, double maxres, T* /* tag */, vnl_random &rng)
{
  const unsigned int n = 3;
  // Test inversion of 3x3 matrix of T :
  std::cout << "----- Testing vnl_svd_fixed<" << type << "> recomposition -----" << std::endl;

  vnl_matrix_fixed<T,n,n> A;
  test_util_fill_random(A.begin(), A.end(), rng);

  std::cout << "A = [\n" << A << "]\n";
  vnl_svd_fixed<T,n,n> svd(A);

  vnl_matrix_fixed<T,n,n> B=svd.recompose();
  std::cout << "B = [\n" << B << "]\n";

  double residual=(A - B).fro_norm();
  TEST_NEAR("vnl_svd<float> recomposition residual", residual, 0, maxres);
}


#include <vnl/vnl_matlab_print.h>
template <class T> static
void test_nullvector(char const *type, double max_err, T *, vnl_random &rng)
{
  std::cout << "----- Testing vnl_svd_fixed<" << type << "> null vector -----" << std::endl;
  const int n = 3;
  vnl_matrix_fixed<T,n,n+1> A;
  test_util_fill_random(A.begin(), A.end(), rng);
  vnl_svd_fixed<T,n,n+1> svd(A);
  vnl_vector_fixed<T,n+1>  x = svd.nullvector();
  vnl_vector_fixed<T,n> Ax = A*x;
  vnl_matlab_print(std::cout, A, "A", vnl_matlab_print_format_long);
  std::cout << "|| x|| = " <<  x.two_norm() << std::endl
           << "||Ax|| = " << Ax.two_norm() << std::endl;
  TEST_NEAR("||Ax||", Ax.two_norm(), 0.0, max_err);
}




static void test_speed(vnl_random& rng)
{
  int ms_heap;
  {
    double sum=0;
    const std::clock_t timer_01 = std::clock();
    vnl_matrix<double> A(3,3);
    for (unsigned count=0; count<10000; ++count)
    {
      test_util_fill_random(A.begin(), A.end(), rng);
      vnl_svd<double> svd(A);
      sum += svd.inverse().fro_norm();
    }
    const std::clock_t timer_02 = std::clock();
    ms_heap =  ( ( timer_02 - timer_01)*1000 ) /CLOCKS_PER_SEC;
    std::cout << "vnl_svd time for 10000 3x3 inversions: " << ms_heap << "ms." << std::endl;
  }
  int ms_stack;
  {
    double sum=0;
    const std::clock_t timer_03 = std::clock();
    vnl_matrix_fixed<double,3,3> A;
    for (unsigned count=0; count<10000; ++count)
    {
      test_util_fill_random(A.begin(), A.end(), rng);
      vnl_svd_fixed<double,3,3> svd(A);
      sum += svd.inverse().fro_norm();
    }
    const std::clock_t timer_04 = std::clock();
    ms_stack = ( ( timer_04 - timer_03)*1000 ) /CLOCKS_PER_SEC;
    std::cout << "vnl_svd_fixed time for 10000 3x3 inversions: " << ms_stack << "ms." << std::endl;
  }
  int ms_nosvd;
  {
    double sum=0;
    const std::clock_t timer_05 = std::clock();
    vnl_matrix_fixed<double,3,3> A;
    for (unsigned count=0; count<10000; ++count)
    {
      test_util_fill_random(A.begin(), A.end(), rng);
      sum += vnl_inverse(A).fro_norm();
    }
    const std::clock_t timer_06 = std::clock();
    ms_nosvd = ( ( timer_06 - timer_05)*1000 ) /CLOCKS_PER_SEC;
    std::cout << "(c.f. vnl_inverse no-SVD time for 10000 3x3 inversions: " << ms_nosvd << "ms.)" << std::endl;
  }
  std::cout << "Stack Memory Time: " << ms_stack << " vs. Heap Memory Time: " << ms_heap << std::endl;
  // This test should be allowed to fail.  The timing test above is not very good.  It
  // confounds the random number generation and compares computations of different matricies.
  // TEST("Stack memory SVD is faster than heap memory SVD", ms_stack <= ms_heap, true);
}

// Driver
void test_svd_fixed()
{
  vnl_random rng(9667566);
//  test_hilbert(float(), "float", float(0.025));
  test_hilbert(double(), "double", 1.1e-10);
//  test_hilbert(std::complex<double>(), "std::complex<double>", double(4.4e-10));
//  test_hilbert(std::complex<float>(), "std::complex<float>", float(0.04));
  test_pmatrix();
  test_I();

//  test_svd_recomposition("float",              1e-5 , (float*)0, rng);
  test_svd_recomposition("double",             1e-10, (double*)VXL_NULLPTR, rng);
//  test_svd_recomposition("std::complex<float>",  1e-5 , (std::complex<float>*)0, rng);
//  test_svd_recomposition("std::complex<double>", 1e-10, (std::complex<double>*)0, rng);

//  test_nullvector("float",               5e-7,  (float*)0, rng);
  test_nullvector("double",              5e-15, (double*)VXL_NULLPTR, rng);
//  test_nullvector("std::complex<float>",  5e-7,  (std::complex<float>*)0, rng);
//  test_nullvector("std::complex<double>", 5e-15, (std::complex<double>*)0, rng);

  test_speed(rng);
}

TESTMAIN(test_svd_fixed);
