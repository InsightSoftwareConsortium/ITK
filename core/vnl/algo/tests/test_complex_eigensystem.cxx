//:
// \file
// \author fsm, Oxford RRG
// \date    7 September 1999
#include <complex>
#include <iostream>
#include <vcl_compiler.h>
#include <vnl/algo/vnl_complex_eigensystem.h>

#include <testlib/testlib_test.h>

void test_complex_eigensystem1()
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

  vnl_matrix<std::complex<double> > A(N,N);
  for (unsigned i=0;i<N;i++)
    for (unsigned j=0;j<N;j++)
      A(i,j) = std::complex<double>(A_real(i,j), A_imag(i,j));

  vnl_complex_eigensystem eig(A,     // compute both
                              true,  // left and right
                              true); // eigenvectors
  TEST("vnl_complex_eigensystem constructor", eig.N, N);
  for (unsigned i=0;i<N;i++) {
    std::complex<double> w = eig.W[i];
    std::cout << "  W[" << i << "] = " << w << '\n';
    //
    vnl_vector<std::complex<double> > l = eig.left_eigen_vector(i);
    vnl_vector<std::complex<double> > err = l*A - l*w;
    testlib_test_assert_near("  Left  eigenvalue residue", err.magnitude());
    //
    vnl_vector<std::complex<double> > r = eig.right_eigen_vector(i);
    err = A*r - w*r;
    testlib_test_assert_near("  Right eigenvalue residue", err.magnitude());
  }
}

void test_complex_eigensystem2()
{
  // The standard version of ZLAHQR fails to converge on this 6x6 matrix
  // because the maximum number of iterations is reached. Removing the
  // upper limit makes it work, though.
  double Adata[6][6] = {
    { 6.811898476755, -0.750947244402,  0.029620459055,  0.082784816274, -0.003265374870,  0.000128799864},
    {-0.302642078990,  7.243967032503, -0.238733709072, -1.593479414193,  0.057672293761, -0.002070468886},
    {-0.224780478514,  1.663978565954,  6.516036730518, -0.364143980645, -0.711203495953,  0.056672152613},
    { 0.003361479487, -0.160548535977,  0.005288667260,  7.668002291196, -0.252593475373,  0.008320741358},
    { 0.004993323929, -0.155932510596, -0.140831520110,  3.504603640364,  6.856177569090, -0.455504863942},
    { 0.001854338541, -0.027249736525, -0.107516848058,  0.400438282672,  1.579973514772,  6.233960176641}
  };
  vnl_matrix<std::complex<double> > A(6, 6);
  for (int i=0; i<6; ++i)
    for (int j=0; j<6; ++j)
      A[i][j] = Adata[i][j]; //(0.77+i) + (0.1+j)*(0.33+j);
  vnl_complex_eigensystem eig(A);
  TEST("vnl_complex_eigensystem constructor", eig.N, 6);
  for (int i=0; i<6; ++i)
    std::cout << "  W[" << i << "] = " << eig.eigen_value(i) << '\n';
}

void test_complex_eigensystem()
{
  test_complex_eigensystem1();
  test_complex_eigensystem2();
}

TESTMAIN(test_complex_eigensystem);
