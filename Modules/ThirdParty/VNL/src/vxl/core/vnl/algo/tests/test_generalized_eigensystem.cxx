// This is core/vnl/algo/tests/test_generalized_eigensystem.cxx
#include <iostream>
#include <testlib/testlib_test.h>
//:
// \file
// \brief test program for generalized eigensystem routines.
// \author Andrew W. Fitzgibbon, Oxford RRG.
// \date 29 Aug 96

//-----------------------------------------------------------------------------

#include <vnl/algo/vnl_generalized_eigensystem.h>

void test_generalized_eigensystem()
{
  double Sdata[36] = {
   30.0000,   -3.4273,   13.9254,   13.7049,   -2.4446,   20.2380,
   -3.4273,   13.7049,   -2.4446,    1.3659,    3.6702,   -0.2282,
   13.9254,   -2.4446,   20.2380,    3.6702,   -0.2282,   28.6779,
   13.7049,    1.3659,    3.6702,   12.5273,   -1.6045,    3.9419,
   -2.4446,    3.6702,   -0.2282,   -1.6045,    3.9419,    2.5821,
   20.2380,   -0.2282,   28.6779,    3.9419,    2.5821,   44.0636,
  };
  double Cdata[36] = {
    0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  2,
    0,  0,  0,  0, -1,  0,
    0,  0,  0,  2,  0,  0,
  };

  vnl_matrix<double> S(Sdata, 6,6);
  vnl_matrix<double> C(Cdata, 6,6);

  vnl_generalized_eigensystem gev(C, S);

  std::cout << "V = " << gev.V << std::endl
           << "D = " << gev.D << std::endl
           << "residual = " << C * gev.V - S * gev.V * gev.D << std::endl;
  double err = (C * gev.V - S * gev.V * gev.D).fro_norm();
  std::cout << "Recomposition residual = " << err << std::endl;

  TEST_NEAR("Recomposition residual < 1e-12", err, 0.0, 1e-12);
}

TESTMAIN(test_generalized_eigensystem);
