// This is core/vnl/algo/tests/test_complex_algo.cxx
#include <complex>
//:
// \file
// \brief test miscellaneous classes and functions in vnl/algo.
// This file contains short tests for several algorithms in vnl/algo
// that use std::complex.
// Currently, the following classes or functions are tested here:
// - vnl_svd_economy
//
// \author Peter Vanroose, ABIS Leuven.
// \date 9 January 2011

#include "testlib/testlib_test.h"
#include <vnl/algo/vnl_svd_economy.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_matrix_inverse.h>

static void
test_matrix_inverse()
{
  std::complex<double> data[] = { 1.,
                                  -1.,
                                  1.,
                                  std::complex<double>(0., 1.),
                                  1.,
                                  1.,
                                  -1.,
                                  std::complex<double>(0., -1.),
                                  1.,
                                  1.,
                                  1.,
                                  1.,
                                  std::complex<double>(1., 1.),
                                  -1.,
                                  std::complex<double>(-1., -1.),
                                  1. };
  const vnl_matrix<std::complex<double>> m(data, 4, 4);
  vnl_svd_economy<std::complex<double>> svde(m);
  vnl_matrix<std::complex<double>> V = svde.V();
  vnl_svd<std::complex<double>> svd(m);
  vnl_matrix<std::complex<double>> V0 = svd.V();
  TEST_NEAR("complex vnl_svd_economy", V[0][1], V0[0][1], 1e-6);

  const vnl_matrix<std::complex<double>> inv{ vnl_matrix_inverse<std::complex<double>>(m).as_matrix() };
  vnl_matrix<std::complex<double>> identity(4, 4);
  identity.set_identity();
  TEST_NEAR("complex vnl_matrix_inverse", (m * inv - identity).array_inf_norm(), 0, 1e-6);
}

void
test_complex_algo()
{
  test_matrix_inverse();
}

TESTMAIN(test_complex_algo);
