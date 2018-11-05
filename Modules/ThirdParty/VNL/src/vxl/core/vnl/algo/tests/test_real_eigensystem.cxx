// This is core/vnl/algo/tests/test_real_eigensystem.cxx
#include <iostream>
#include <complex>
#include <testlib/testlib_test.h>

//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Jan 96
//
//-----------------------------------------------------------------------------
#include <vnl/vnl_complexify.h>
#include <vnl/algo/vnl_real_eigensystem.h>

static void test_6x6()
{
  double Sdata[36] = {
    30.0000,   -3.4273,   13.9254,   13.7049,   -2.4446,   20.2380,
    -3.4273,   13.7049,   -2.4446,    1.3659,    3.6702,   -0.2282,
    13.9254,   -2.4446,   20.2380,    3.6702,   -0.2282,   28.6779,
    13.7049,    1.3659,    3.6702,   12.5273,   -1.6045,    3.9419,
    -2.4446,    3.6702,   -0.2282,   -1.6045,    3.9419,    2.5821,
    20.2380,   -0.2282,   28.6779,    3.9419,    2.5821,   44.0636,
  };
  vnl_matrix<double> S(Sdata, 6,6);

  vnl_real_eigensystem eig(S);
  vnl_diag_matrix<std::complex<double> > D(eig.D.rows());
  for (unsigned i = 0; i < eig.D.rows(); ++i)
  {
    TEST("All real", std::imag(eig.D(i,i)) < 1e-15, true);
    D(i,i) = std::real(eig.D(i,i));
  }

  std::cout << "D = " << eig.D << std::endl
           << "V = " << eig.V << std::endl;

  vnl_matrix<std::complex<double> > diff = vnl_complexify(S*eig.Vreal) - vnl_complexify(eig.Vreal)*D;
  std::cout << "X*V - V*D = " << diff << std::endl
           << "residual = " << diff.fro_norm() << std::endl;
  TEST_NEAR("recompose residual", diff.fro_norm(), 0.0, 1e-12);
}

static void test_4x4()
{
  // unsympathetic
  double Xdata[] = {
    686,   526,   701,    47,
    588,    91,   910,   736,
    930,   653,   762,   328,
    846,   415,   262,   632
  };
  vnl_matrix<double> X(Xdata, 4, 4);

  vnl_real_eigensystem eig(X);

  std::cout << "D = " << eig.D << std::endl
           << "V = " << eig.V << std::endl;

  vnl_matrix<std::complex<double> > XC = vnl_complexify(X);

  vnl_matrix<std::complex<double> > diff = XC*eig.V - eig.V*eig.D;
  std::cout << "X*V - V*D = " << diff << std::endl
           << "residual = " << diff.fro_norm() << std::endl;
  TEST_NEAR("recompose residual", diff.fro_norm(), 0.0, 1e-11);
}

static void test_real_eigensystem()
{
  test_6x6();
  test_4x4();
}

TESTMAIN(test_real_eigensystem)
