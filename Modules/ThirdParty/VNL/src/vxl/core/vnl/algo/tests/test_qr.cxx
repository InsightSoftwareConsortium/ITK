// This is core/vnl/algo/tests/test_qr.cxx
#include "test_util.h"
#include <vcl_iostream.h>
#include <vcl_complex.h>
#include <testlib/testlib_test.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/algo/vnl_qr.h>

//--------------------------------------------------------------------------------

void test_matrix(char const* name, const vnl_matrix<double>& A, double det = 0)
{
  vnl_qr<double> qr(A);

  vcl_string n(name); n+= ": ";
  TEST_NEAR((n+"Q * R residual").c_str(), (qr.Q() * qr.R() - A).fro_norm(), 0.0, 1e-12);
  TEST((n+"Q * Q = I").c_str(), (qr.Q().transpose() * qr.Q()).is_identity(1e-12), true);

  if (det) { TEST_NEAR((n+ "Determinant").c_str(), qr.determinant(), det, 1e-9); }
}

void double_test()
{
  double A_data[] = {
    89,    21,    27,
    62,    71,     0,
    84,    13,    41,
    16,     9,     3,
  };
  vnl_matrix<double> A(A_data, 4,3);

  test_matrix("A", A);
  test_matrix("AT", A.transpose());

  test_matrix("A-102", A-102);
  test_matrix("AT-12", A.transpose() - 12);

  test_matrix("AA'*1e-3 - 1", A*A.transpose()*1e-3 - 1,  -2.77433958399998);

  double b_data[] = {
    68, 39, 39, 50
  };

  vnl_vector<double> b(b_data, 4);
  vnl_qr<double> qr(A);

  vnl_matlab_print(vcl_cout, qr.Q(), "Q");
  vnl_matlab_print(vcl_cout, qr.R(), "R");

  vnl_vector<double> x = qr.solve(b);

  double res = (A * x - b).magnitude();

  TEST_NEAR("Solve residual", res, 37.8841, 1e-3);

  {
    double S_data[] = {
      89,          21,    27,
      62,          71,     0,
      84,          13,    41,
    };
    vnl_matrix<double> S(S_data, 3,3);
    test_matrix("S", S, 66431);
    test_matrix("S-100", S-100, -79869);
  }
}

//--------------------------------------------------------------------------------

inline float  eps(float *) { return 1e-5f; }
inline double eps(double *) { return 1e-12; }
inline float  eps(vcl_complex<float> *) { return 1e-5f; }
inline double eps(vcl_complex<double> *) { return 1e-12; }
#define rounding_epsilon(T) ::eps((T*)0)

template <class T>
void new_test(T *)
{
  vnl_random rng(1000);
  unsigned m = 5; // m must be >= n when using the netlib QR algorithms,
  unsigned n = 5; // but n >= m for a random A and b to have exact solution.

  vnl_matrix<T> A(m, n);
  test_util_fill_random(A.begin(), A.end(), rng);
  vnl_matlab_print(vcl_cout, A, "A");

  vnl_vector<T> b(m);
  test_util_fill_random(b.begin(), b.end(), rng);
  vnl_matlab_print(vcl_cout, b, "b");

  vnl_qr<T> qr(A);
  vnl_matrix<T> const &Q = qr.Q();
  vnl_matrix<T> const &R = qr.R();
  vnl_vector<T> x = qr.solve(b);

  vnl_matlab_print(vcl_cout, Q, "Q");
  vnl_matlab_print(vcl_cout, R, "R");
  vnl_matlab_print(vcl_cout, x, "x");

  vnl_matrix<T> QR(Q * R);
  vnl_matlab_print(vcl_cout, QR, "QR");

  vnl_matrix<T> I(m, m); I.set_identity();
  TEST_NEAR("||Q'Q - 1||", (Q.conjugate_transpose()*Q - I).fro_norm(), 0, rounding_epsilon(T));
  TEST_NEAR("||A - QR||", (A - QR).fro_norm(), 0, rounding_epsilon(T));
  TEST_NEAR("||Ax - b||", (A*x - b).two_norm(), 0, rounding_epsilon(T));
}

#define inst(T) \
template void new_test(T *);
inst(float);
inst(double);
inst(vcl_complex<float>);
inst(vcl_complex<double>);
#undef inst

void complex_test()
{
  typedef vcl_complex<double> ct;

  vnl_matrix<ct> A(5,4); // #rows must be >= #cols when using netlib QR decomposition
  vnl_vector<ct> b(5);

  A(0,0)=ct( -0.1370,0.5573);
  A(1,0)=ct(  0.6187,0.3482);
  A(2,0)=ct( -0.4402,0.6825);
  A(3,0)=ct(  0.7284,0.7294);
  A(4,0)=ct( -0.5840,0.5004);
  A(0,1)=ct( -0.4108,0.7201);
  A(1,1)=ct( -0.5621,0.6056);
  A(2,1)=ct(  0.4312,0.1262);
  A(3,1)=ct(  0.9796,0.6049);
  A(4,1)=ct( -0.1388,0.4999);
  A(0,2)=ct(  0.7219,0.5105);
  A(1,2)=ct(  0.9562,0.7896);
  A(2,2)=ct( -0.1356,0.2092);
  A(3,2)=ct( -0.0847,0.7457);
  A(4,2)=ct(  0.9721,0.5243);
  A(0,3)=ct(  0.2085,0.3057);
  A(1,3)=ct( -0.0903,0.5162);
  A(2,3)=ct( -0.8424,0.5799);
  A(3,3)=ct( -0.6948,0.0472);
  A(4,3)=ct(  0.8900,0.5085);

  b(0)=ct( 0.9764,0.2280);
  b(1)=ct( 0.5994,0.0454);
  b(2)=ct(-0.2385,0.4884);
  b(3)=ct( 0.0538,0.0402);
  b(4)=ct( 1.8634,.64558);
                                    vnl_matlab_print(vcl_cout, b, "b");
  vnl_qr<ct> qr(A);                 vnl_matlab_print(vcl_cout, A, "A");
  const vnl_matrix<ct>& Q = qr.Q(); vnl_matlab_print(vcl_cout, Q, "Q");
  const vnl_matrix<ct>& R = qr.R(); vnl_matlab_print(vcl_cout, R, "R");
  vnl_vector<ct> x = qr.solve(b);   vnl_matlab_print(vcl_cout, x, "solve");
  TEST_NEAR("||Ax - b||", (A*x - b).two_norm(), 0, 1e-5);
}

//--------------------------------------------------------------------------------

extern "C" void test_qr()
{
  vcl_cout << "-------------------- double_complex\n";
  complex_test();
  vcl_cout << "-------------------- double\n";
  double_test();

  vcl_cout << "-------------------- float\n";
  new_test((float*)0);
  vcl_cout << "-------------------- double\n";
  new_test((double*)0);
  vcl_cout << "-------------------- float_complex\n";
  new_test((vcl_complex<float>*)0);
  vcl_cout << "-------------------- double_complex\n";
  new_test((vcl_complex<double>*)0);
}

TESTMAIN(test_qr);
