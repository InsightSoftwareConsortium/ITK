// This is core/vnl/algo/tests/test_symmetric_eigensystem.cxx
#include <iostream>
#include <algorithm>
#include <ctime>
#include <testlib/testlib_test.h>
//:
// \file
// \brief test program for symmetric eigensystem routines.
// \author Andrew W. Fitzgibbon, Oxford RRG.
// \date 29 Aug 96

//-----------------------------------------------------------------------------


#include <vnl/vnl_double_3x3.h>
#include <vnl/vnl_double_3.h>
#include <vnl/vnl_random.h>
#include <vcl_compiler.h>
#include <vnl/vnl_c_vector.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>

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
    std::cout << "V'*D*V - S = " << res << std::endl
             << "residual = " << res.fro_norm() << std::endl;
    TEST_NEAR("recompose residual", res.fro_norm(), 0.0, 1e-12);

    std::cout<<"Eigenvalues: ";
    for (int i=0;i<6;++i)
      std::cout << eig.get_eigenvalue(i) << ' ';
    std::cout << std::endl;
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
    std::cout << "V'*D*V - C = " << res << std::endl
             << "residual = " << res.fro_norm() << std::endl;
    TEST_NEAR("recompose residual", res.fro_norm(), 0.0, 1e-12);

    std::cout<<"Eigenvalues: ";
    for (int i=0;i<6;++i)
      std::cout << eig.get_eigenvalue(i) << ' ';
    std::cout << std::endl;
  }

  {
    // Generate a random system
    vnl_random rng;
    int n = 6;
    int s = 10;
    vnl_matrix<double> D_rand(s,n);
    for (int i=0;i<s;++i)
      for (int j=0;j<n;++j)
        D_rand(i,j) = 1.0 + 2.0*rng.normal64();

    vnl_matrix<double> S = D_rand.transpose() * D_rand;
    vnl_matrix<double> evecs(n,n);
    vnl_vector<double> evals(n);
    vnl_symmetric_eigensystem_compute(S,evecs,evals);
    std::cout << "Testing random system:\n"
             << "evals: "<<evals<<std::endl;
    for (int i=1;i<n;++i)
    {
      TEST("Eigenvalue increases", evals(i) >= evals(i-1), true);
    }
  }

  {  // test I with specialised 3x3 version
    double l1, l2, l3;
    vnl_symmetric_eigensystem_compute_eigenvals(1.0, 0.0, 0.0, 1.0, 0.0, 1.0,
                                                l1, l2, l3);
    std::cout << "Eigenvals: " << l1 << ' ' << l2 << ' ' << l3 << std::endl;
    TEST("Correct eigenvalues for I", l1==1.0 && l2==1.0 && l3 ==1.0, true);
  }

  { // compare speed and values of specialised 3x3 version with nxn version
    const unsigned n = 20000;
    double fixed_data[n][3];
    double netlib_data[n][3];

    int fixed_time;
    {
      double M11, M12, M13, M22, M23, M33;
      // Generate a random system
      vnl_random rng(5);

      const std::clock_t timer_01 = std::clock();
      for (unsigned c = 0; c < n; ++c)
      {
        M11 = rng.drand64()*10.0-5.0; M12 = rng.drand64()*10.0-5.0; M13 = rng.drand64()*10.0-5.0;
                                      M22 = rng.drand64()*10.0-5.0; M23 = rng.drand64()*10.0-5.0;
                                                                    M33 = rng.drand64()*10.0-5.0;
        vnl_symmetric_eigensystem_compute_eigenvals(M11, M12, M13, M22, M23, M33,
                                                    fixed_data[c][0], fixed_data[c][1], fixed_data[c][2]);
      }
      const std::clock_t timer_02 = std::clock();
      fixed_time = ( timer_02 - timer_01)/ (CLOCKS_PER_SEC/1000);
    }

    int netlib_time;
    {
      // Generate same random system
      vnl_random rng(5);
      vnl_double_3x3 M, evecs;
      vnl_double_3 evals;

      const std::clock_t timer_03 = std::clock();
      for (unsigned c = 0; c < n; ++c)
      {
        M(0,0)=rng.drand64()*10.0-5.0; M(1,0)=M(0,1)=rng.drand64()*10.0-5.0; M(2,0)=M(0,2)= rng.drand64()*10.0-5.0;
                                       M(1,1)=rng.drand64()*10.0-5.0;        M(2,1)=M(1,2)=rng.drand64()*10.0-5.0;
                                                                             M(2,2) = rng.drand64()*10.0-5.0;

        vnl_symmetric_eigensystem_compute(M.as_ref(), evecs.as_ref().non_const(), evals.as_ref().non_const());
        netlib_data[c][0] = evals[0];
        netlib_data[c][1] = evals[1];
        netlib_data[c][2] = evals[2];
      }
      const std::clock_t timer_04 = std::clock();
      netlib_time = ( timer_04 - timer_03)/ (CLOCKS_PER_SEC/1000);
    }

    std::cout << "Fixed Time: " << fixed_time << "   netlib time: " <<netlib_time<<std::endl;
    TEST("Specialised version is faster", fixed_time < netlib_time, true);

    double sum_dsq=0.0;
    double max_dsq=0.0;
    for (unsigned c = 0; c < n; ++c)
    {
      const double dsq = vnl_c_vector<double>::euclid_dist_sq(netlib_data[c], fixed_data[c],3);
      max_dsq = std::max(dsq,max_dsq);
      sum_dsq += dsq;
    }
    std::cout << "max_dsq: " <<max_dsq<<"  mean_dsq: "<<sum_dsq/static_cast<double>(n)<<std::endl;
    TEST("Specialised version gives similar results", max_dsq < 1e-8, true);
  }

  {
    double v1, v2, v3;
    vnl_symmetric_eigensystem_compute_eigenvals(
      4199.0, 0.0, 0.0, 4199.0, 0.0, 4801.0, v1, v2, v3);
    TEST_NEAR("Numerically difficult values are ok v1", v1, 4199, 1e-3);
    TEST_NEAR("Numerically difficult values are ok v2", v2, 4199, 1e-3);
    TEST_NEAR("Numerically difficult values are ok v3", v3, 4801, 1e-7);
  }
}

TESTMAIN(test_symmetric_eigensystem);
