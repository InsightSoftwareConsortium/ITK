// This is core/vnl/algo/tests/test_svd_fixed.cxx
#include "test_util.h"
//:
// \file
#include <testlib/testlib_test.h>
#include <vcl_iostream.h>
#include <vcl_complex.h>

#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_inverse.h>
#include <vnl/algo/vnl_svd_fixed.h>
#include <vnl/algo/vnl_svd.h>

#include <vul/vul_timer.h>


template <class T, class S> static
void test_hilbert(T /*dummy*/, char const* type, S residual)
{
  vcl_cout << "----- Testing svd_fixed<"<<type<<">(Hilbert_3x3) -----" << vcl_endl;
  typedef typename vnl_numeric_traits<T>::abs_t abs_t;
  // Test inversion and recomposition of 3x3 hilbert matrix
  vnl_matrix_fixed<T,3,3> H;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      H(i,j) = T(1) / T(abs_t(i+j+1)); // sic, because i,j are zero based

  vcl_cout << "H = <"<<type<<">[ " << H << "]\n";

  vnl_svd_fixed<T,3,3> svd(H);

  vcl_cout << "rcond(H) = " << svd.well_condition() << vcl_endl;

  vnl_matrix_fixed<T,3,3> Hinv = svd.inverse();

  vnl_matrix_fixed<T,3,3> X = Hinv * H;

  vcl_cout << "H*inv(H) = " << X << vcl_endl;

  vnl_matrix_fixed<T,3,3> I;
  I = 0.0;
  I.fill_diagonal(1.0);

  vnl_matrix_fixed<T,3,3> res = X - I;
  TEST_NEAR("Hilbert recomposition residual", res.fro_norm(), 0, residual);
}


//: Test nullspace extraction of rank=2 3x4 matrix.
static void test_pmatrix()
{
  vcl_cout << "----- Testing nullspace extraction of rank=2 3x4 matrix -----" << vcl_endl;

  double pdata[] = {
    2, 0, 0, 0,
    3, 10, 5, 5,
    5, 12, 6, 6,
  };
  vnl_matrix_fixed<double, 3, 4> P(pdata);
  vnl_svd_fixed<double, 3, 4> svd(P, 1e-8);

  vnl_matrix_fixed<double, 3, 4> res = svd.recompose() - P;
  TEST_NEAR("PMatrix recomposition residual", res.fro_norm(), 0, 1e-12);
  vcl_cout << " Inv = " << svd.inverse() << vcl_endl;

  TEST("singularities = 2", svd.singularities(), 2);
  TEST("rank = 2", svd.rank(), 2);

  vnl_matrix<double> N = svd.nullspace();
  TEST("nullspace dimension", N.columns(), 2);
  vcl_cout << "null(P) =\n" << N << vcl_endl;

  vnl_matrix<double> PN = P*N;
  vcl_cout << "P * null(P) =\n" << PN << vcl_endl;
  TEST_NEAR("P nullspace residual", PN.fro_norm(), 0, 1e-12);

  vnl_vector_fixed<double, 4> n = svd.nullvector();
  TEST_NEAR("P nullvector residual", (P*n).magnitude(), 0, 1e-12);

  vnl_vector_fixed<double, 3> l = svd.left_nullvector();
  vcl_cout << "left_nullvector(P) = " << l << vcl_endl;
  TEST_NEAR("P left nullvector residual", (l*P).magnitude(), 0, 1e-12);
}

static void test_I()
{
  vcl_cout << "----- testing vnl_svd(I) -----\n";
  double Idata[] = {
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
  };
  vnl_matrix_fixed<double, 3, 4> P(Idata);
  vnl_svd_fixed<double, 3, 4> svd(P);
  vcl_cout << svd;

  vnl_vector_fixed<double, 4> w_expected(1, 1, 1, 0);
  TEST_NEAR ("Singular values", vnl_vector_ssd(w_expected, svd.W().diagonal().as_ref()), 0, 1e-16);
}

template <class T>
void test_svd_recomposition(char const *type, double maxres, T* /* tag */, vnl_random &rng)
{
  const unsigned int n = 3;
  // Test inversion of 3x3 matrix of T :
  vcl_cout << "----- Testing vnl_svd_fixed<" << type << "> recomposition -----" << vcl_endl;

  vnl_matrix_fixed<T,n,n> A;
  test_util_fill_random(A.begin(), A.end(), rng);

  vcl_cout << "A = [\n" << A << "]\n";
  vnl_svd_fixed<T,n,n> svd(A);

  vnl_matrix_fixed<T,n,n> B=svd.recompose();
  vcl_cout << "B = [\n" << B << "]\n";

  double residual=(A - B).fro_norm();
  TEST_NEAR("vnl_svd<float> recomposition residual", residual, 0, maxres);
}


#include <vnl/vnl_matlab_print.h>
template <class T> static
void test_nullvector(char const *type, double max_err, T *, vnl_random &rng)
{
  vcl_cout << "----- Testing vnl_svd_fixed<" << type << "> null vector -----" << vcl_endl;
  const int n = 3;
  vnl_matrix_fixed<T,n,n+1> A;
  test_util_fill_random(A.begin(), A.end(), rng);
  vnl_svd_fixed<T,n,n+1> svd(A);
  vnl_vector_fixed<T,n+1>  x = svd.nullvector();
  vnl_vector_fixed<T,n> Ax = A*x;
  vnl_matlab_print(vcl_cout, A, "A", vnl_matlab_print_format_long);
  vcl_cout << "|| x|| = " <<  x.two_norm() << vcl_endl
           << "||Ax|| = " << Ax.two_norm() << vcl_endl;
  TEST_NEAR("||Ax||", Ax.two_norm(), 0.0, max_err);
}




static void test_speed(vnl_random& rng)
{
  vul_timer timer;
  int ms_heap, ms_stack, ms_nosvd;
  {
    double sum=0;
    timer.mark();
    vnl_matrix<double> A(3,3);
    for (unsigned count=0; count<10000; ++count)
    {
      test_util_fill_random(A.begin(), A.end(), rng);
      vnl_svd<double> svd(A);
      sum += svd.inverse().fro_norm();
    }
    ms_heap = timer.user();
    vcl_cout << "vnl_svd time for 10000 3x3 inversions: " << ms_heap << "ms." << vcl_endl;
  }
  {
    double sum=0;
    timer.mark();
    vnl_matrix_fixed<double,3,3> A;
    for (unsigned count=0; count<10000; ++count)
    {
      test_util_fill_random(A.begin(), A.end(), rng);
      vnl_svd_fixed<double,3,3> svd(A);
      sum += svd.inverse().fro_norm();
    }
    ms_stack = timer.user();
    vcl_cout << "vnl_svd_fixed time for 10000 3x3 inversions: " << ms_stack << "ms." << vcl_endl;
  }
  {
    double sum=0;
    timer.mark();
    vnl_matrix_fixed<double,3,3> A;
    for (unsigned count=0; count<10000; ++count)
    {
      test_util_fill_random(A.begin(), A.end(), rng);
      sum += vnl_inverse(A).fro_norm();
    }
    ms_nosvd = timer.user();
    vcl_cout << "(c.f. vnl_inverse no-SVD time for 10000 3x3 inversions: " << ms_nosvd << "ms.)" << vcl_endl;
  }
  TEST("Stack memory SVD is faster than heap memory SVD", ms_stack < ms_heap, true);
}

// Driver
void test_svd_fixed()
{
  vnl_random rng(9667566);
//  test_hilbert(float(), "float", float(0.025));
  test_hilbert(double(), "double", 1.1e-10);
//  test_hilbert(vcl_complex<double>(), "vcl_complex<double>", double(4.4e-10));
//  test_hilbert(vcl_complex<float>(), "vcl_complex<float>", float(0.04));
  test_pmatrix();
  test_I();

//  test_svd_recomposition("float",              1e-5 , (float*)0, rng);
  test_svd_recomposition("double",             1e-10, (double*)0, rng);
//  test_svd_recomposition("vcl_complex<float>",  1e-5 , (vcl_complex<float>*)0, rng);
//  test_svd_recomposition("vcl_complex<double>", 1e-10, (vcl_complex<double>*)0, rng);

//  test_nullvector("float",               5e-7,  (float*)0, rng);
  test_nullvector("double",              5e-15, (double*)0, rng);
//  test_nullvector("vcl_complex<float>",  5e-7,  (vcl_complex<float>*)0, rng);
//  test_nullvector("vcl_complex<double>", 5e-15, (vcl_complex<double>*)0, rng);

  test_speed(rng);
}

TESTMAIN(test_svd_fixed);
