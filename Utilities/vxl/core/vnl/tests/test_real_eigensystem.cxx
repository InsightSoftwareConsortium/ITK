// This is core/vnl/tests/test_real_eigensystem.cxx
#include <testlib/testlib_test.h>
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Jan 96
//
//-----------------------------------------------------------------------------
#include <vcl_iostream.h>
#include <vcl_complex.h>
#include <vnl/vnl_complexify.h>
#include <vnl/algo/vnl_real_eigensystem.h>

void test_real_eigensystem()
{
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

    {
      vnl_real_eigensystem eig(S);
      vnl_diag_matrix<vcl_complex<double> > D(eig.D.rows());
      for (unsigned i = 0; i < eig.D.rows(); ++i) {
        testlib_test_assert("All real", vcl_imag(eig.D(i,i)) < 1e-15);
        D(i,i) = vcl_real(eig.D(i,i));
      }

      vcl_cout << "D = " << eig.D << vcl_endl
               << "V = " << eig.V << vcl_endl;

      vnl_matrix<vcl_complex<double> > diff = vnl_complexify(S*eig.Vreal) - vnl_complexify(eig.Vreal)*D;
      vcl_cout << "X*V - V*D = " << diff << vcl_endl
               << "residual = " << diff.fro_norm() << vcl_endl;
      testlib_test_assert("recompose residual",  diff.fro_norm() < 1e-12);
    }
  }

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

    vcl_cout << "D = " << eig.D << vcl_endl
             << "V = " << eig.V << vcl_endl;

    vnl_matrix<vcl_complex<double> > XC = vnl_complexify(X);

    vnl_matrix<vcl_complex<double> > diff = XC*eig.V - eig.V*eig.D;
    vcl_cout << "X*V - V*D = " << diff << vcl_endl
             << "residual = " << diff.fro_norm() << vcl_endl;
    testlib_test_assert("recompose residual",  diff.fro_norm() < 1e-11);
  }
}

TESTMAIN(test_real_eigensystem)
