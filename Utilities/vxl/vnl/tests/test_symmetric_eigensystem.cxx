// This is vxl/vnl/tests/test_symmetric_eigensystem.cxx

//:
// \file
// \brief test program for symmetric eigensystem routines.
// \author Andrew W. Fitzgibbon, Oxford RRG.
// \date 29 Aug 96

//-----------------------------------------------------------------------------

#include <vcl_iostream.h>
#include <vnl/vnl_sample.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>
#include <testlib/testlib_test.h>

//extern "C"
void test_symmetric_eigensystem()
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
    vnl_symmetric_eigensystem<double> eig(S);
    vnl_matrix<double> res = eig.recompose() - S;
    vcl_cout << "V'*D*V - S = " << res << vcl_endl;
    vcl_cout << "residual = " << res.fro_norm() << vcl_endl;
    testlib_test_assert("recompose residual",  res.fro_norm() < 1e-12);

    vcl_cout<<"Eigenvalues: ";
    for (int i=0;i<6;++i)
      vcl_cout<<eig.get_eigenvalue(i)<<" ";
    vcl_cout<<vcl_endl;
  }

  double Cdata[36] = {
    0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  2,
    0,  0,  0,  0, -1,  0,
    0,  0,  0,  2,  0,  0,
  };

  vnl_matrix<double> C(Cdata, 6,6);

  {
    vnl_symmetric_eigensystem<double> eig(C);
    vnl_matrix<double> res = eig.recompose() - C;
    vcl_cout << "V'*D*V - C = " << res << vcl_endl;
    vcl_cout << "residual = " << res.fro_norm() << vcl_endl;
    testlib_test_assert("recompose residual", res.fro_norm() < 1e-12);

    vcl_cout<<"Eigenvalues: ";
    for (int i=0;i<6;++i)
      vcl_cout<<eig.get_eigenvalue(i)<<" ";
    vcl_cout<<vcl_endl;
  }

  {
    // Generate a random system
    vnl_sample_reseed();
    int n = 6;
    int s = 10;
    vnl_matrix<double> D_rand(s,n);
    for (int i=0;i<s;++i)
      for (int j=0;j<n;++j)
        D_rand(i,j)=vnl_sample_normal(1,2);

    vnl_matrix<double> S = D_rand.transpose()*D_rand;
    vnl_matrix<double> evecs(n,n);
    vnl_vector<double> evals(n);
    vnl_symmetric_eigensystem_compute(S,evecs,evals);
    vcl_cout<<"Testing random system: "<<vcl_endl;
    vcl_cout<<"evals: "<<evals<<vcl_endl;
    for (int i=1;i<n;++i)
      TEST("Eigenvalue increases",evals(i)>=evals(i-1),true);
  }
}

TESTMAIN(test_symmetric_eigensystem);
