// Author: F. Schaffalitzky, Oxford RRG
// Created: 7 September 1999
#include <vnl/vnl_complex.h>
#include <vnl/vnl_test.h>
#include <vnl/vnl_matops.h>
#include <vnl/algo/vnl_complex_eigensystem.h>

void test_complex_eigensystem()
{
  const unsigned N=6;
  double a_real[N*N] = {
    0.5965,   -0.7781,   -1.6925,    9.8017,   -3.5993,   -1.2015,
    2.8105,    1.3566,   -3.9000,    5.7772,    9.2020,    8.6676,
   -5.8186,    5.8842,    7.4873,   -1.2268,    4.5326,    3.6666,
   -2.4036,   -8.8163,   -9.6998,   -0.0338,   -1.7609,   -5.7488,
    5.6666,    2.0574,    5.3590,   -5.7207,    4.8913,    6.7848,
    3.6169,   -8.9946,    9.4169,    2.8698,   -4.6411,    2.5757
  };
  vnl_matrix<double> A_real(a_real,N,N);

  double a_imag[N*N] = {
    6.9244,    3.6255,   -3.9077,   -6.9825,   -0.0690,   -3.1606,
    0.5030,   -2.4104,   -6.2069,    3.9580,    7.9954,   -4.2055,
   -5.9471,    6.6359,   -6.1314,   -2.4325,    6.4326,   -3.1761,
    3.4427,    0.0563,    3.6445,    7.2002,    2.8982,    0.6816,
    6.7624,    4.1894,   -3.9447,    7.0731,    6.3595,    4.5423,
   -9.6072,   -1.4222,    0.8335,    1.8713,    3.2046,   -3.8142
  };
  vnl_matrix<double> A_imag(a_imag,N,N);

  vnl_matrix<vnl_double_complex> A(N,N);
  for (unsigned i=0;i<N;i++)
    for (unsigned j=0;j<N;j++)
      A(i,j) = vnl_double_complex(A_real(i,j), A_imag(i,j));
  
  vnl_complex_eigensystem eig(A,     // compute both
			      true,  // left and right
			      true); // eigenvectors
  
  //cout << "A = " << A << endl;
  //cout << "eigenvalues = " << eig.W << endl;
  //cout << "L = " << eig.L << endl;
  //cout << "R = " << eig.R << endl;

  for (unsigned i=0;i<N;i++) {
    //cout << "i=" << i << endl;
    //
    vnl_double_complex w = eig.W[i];
    vnl_vector<vnl_double_complex> err;
    //cout << "  w = " << w << endl;
    //
    vnl_vector<vnl_double_complex> l(eig.left_eigen_vector(i));
    err = (l*A - l*w);
    //cout << "  " << err << endl;
    Assert("  Left  eigenvalue", err.magnitude() < 1e-10);
    //
    vnl_vector<vnl_double_complex> r(eig.right_eigen_vector(i));
    err = (A*r - w*r);
    //cout << "  " << err << endl;
    Assert("  Right eigenvalue", err.magnitude() < 1e-10);
  }
}

TESTMAIN(test_complex_eigensystem);
