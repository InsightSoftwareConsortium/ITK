// This is core/vnl/algo/tests/test_complex_algo.cxx
#include <testlib/testlib_test.h>
//:
// \file
// \brief test miscellaneous classes and functions in vnl/algo.
// This file contains short tests for several algorithms in vnl/algo
// that use std::complex.
// Currently, the following classes or functions are tested here:
// - vnl_svd_economy
// - vnl_generalized_schur
//
// \author Peter Vanroose, ABIS Leuven.
// \date 9 January 2011

#include <vcl_complex.h>

#include <vnl/algo/vnl_complex_generalized_schur.h>
#include <vnl/algo/vnl_svd_economy.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_matrix_inverse.h>

static void test_matrix_inverse()
{
  vcl_complex<double> data[] = {1.,-1.,1.,vcl_complex<double>(0.,1.),
                                1.,1.,-1.,vcl_complex<double>(0.,-1.),
                                1.,1.,1.,1.,
                                vcl_complex<double>(1.,1.),-1.,vcl_complex<double>(-1.,-1.),1.};
  vnl_matrix<vcl_complex<double> > m(data,4,4);
  vnl_svd_economy<vcl_complex<double> > svde(m);
  vnl_matrix<vcl_complex<double> > V = svde.V();
  vnl_svd<vcl_complex<double> > svd(m);
  vnl_matrix<vcl_complex<double> > V0 = svd.V();
  TEST_NEAR("complex vnl_svd_economy", V[0][1], V0[0][1], 1e-6);

  vnl_matrix<vcl_complex<double> > inv = vnl_matrix_inverse<vcl_complex<double> >(m);
  vnl_matrix<vcl_complex<double> > identity(4,4); identity.set_identity();
  TEST_NEAR("complex vnl_matrix_inverse", (m*inv-identity).array_inf_norm(), 0, 1e-6);
}

static void test_generalized_schur()
{
  vnl_matrix<vcl_complex<float> > A(4,4,0.0f), B(4,4,0.0f), L(4,4,1.0f), R(4,4,1.0f);
  vnl_vector<vcl_complex<float> > a(4,0.0f), b(4,0.0f);
  bool r = vnl_generalized_schur(&A, &B, &a, &b, &L, &R);
  TEST("vnl_complex_generalized_schur", r, true);
}

void test_complex_algo()
{
  test_matrix_inverse();
  test_generalized_schur();
}

TESTMAIN(test_complex_algo);
