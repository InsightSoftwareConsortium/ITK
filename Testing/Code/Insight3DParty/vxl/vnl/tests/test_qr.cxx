#include <vcl_iostream.h>
#include <vcl_cstdlib.h>
#include <vnl/vnl_test.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_complex.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/algo/vnl_qr.h>

//--------------------------------------------------------------------------------

void test_matrix(char const* name, const vnl_matrix<double>& A, double det = 0)
{
  vnl_qr<double> qr(A);

  vcl_string n(name); n+= ": ";
  AssertNear(n+"Q * R residual", (qr.Q() * qr.R() - A).fro_norm());
  Assert(n+"Q * Q = I", (qr.Q().transpose() * qr.Q()).is_identity(1e-12));

  if (det)
    AssertNear(n+ "Determinant", qr.determinant(), det, 1e-10);
} 

void old_test()
{
  double A_data[] = {
    89,	   21,	  27,
    62,	   71,	   0,
    84,	   13,	  41,
    16,	    9,	   3,
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

  AssertNear("Solve residual", res, 37.8841, 1e-3);

  {
    double S_data[] = {
      89,	   21,	  27,
      62,	   71,	   0,
      84,	   13,	  41,
    };
    vnl_matrix<double> S(S_data, 3,3);
    test_matrix("S", S, 66431);
    test_matrix("S-100", S-100, -79869);
  }

}

//--------------------------------------------------------------------------------

template <class T> class traits;

VCL_DEFINE_SPECIALIZATION 
class traits<double> {
public:
  static double eps() { return 1e-12; }
  static double rand(double const &) { return 2*double(::rand())/double(RAND_MAX) - 1; }
};

VCL_DEFINE_SPECIALIZATION 
class traits<float> {
public:
  static float eps() { return 1e-5; }
  static float rand(float const &) { return float( traits<double>::rand(0) ); }
};

VCL_DEFINE_SPECIALIZATION 
class traits<vnl_float_complex> {
public:
  static float eps() { return traits<float>::eps(); }
  static vnl_float_complex rand(vnl_float_complex const &) { 
    return vnl_float_complex(traits<float>::rand(0), traits<float>::rand(0));
  }
};

VCL_DEFINE_SPECIALIZATION 
class traits<vnl_double_complex> {
public:
  static double eps() { return traits<double>::eps(); }
  static vnl_double_complex rand(vnl_double_complex const &) { 
    return vnl_double_complex(traits<double>::rand(0), traits<double>::rand(0));
  }
};

template <class T>
void new_test(T *) {
  unsigned m = 4;
  unsigned n = 5;
  
  vnl_matrix<T> A(m, n);
  A = A.apply(traits<T>::rand);
  vnl_matlab_print(vcl_cout, A, "A");

  vnl_vector<T> b(m);
  b = b.apply(traits<T>::rand);
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
  vnl_test_assert_near("||Q'Q - 1||", (Q.conjugate_transpose()*Q - I).fro_norm(), 0, traits<T>::eps());
  vnl_test_assert_near("||A - QR||", (A - QR).fro_norm(), 0, traits<T>::eps());
  vnl_test_assert_near("||Ax - b||", (A*x - b).two_norm(), 0, traits<T>::eps());
}

#define inst(T) \
template void new_test(T *);
inst(float);
inst(double);
inst(vnl_float_complex);
inst(vnl_double_complex);
#undef inst

//--------------------------------------------------------------------------------

extern "C" void test_qr() {
  old_test();

  vcl_cout << "-------------------- float" << vcl_endl;
  new_test((float*)0);
  vcl_cout << "-------------------- double" << vcl_endl;
  new_test((double*)0);
  vcl_cout << "-------------------- float_complex" << vcl_endl;
  new_test((vnl_float_complex*)0);
  vcl_cout << "-------------------- double_complex" << vcl_endl;
  new_test((vnl_double_complex*)0);
}

TESTMAIN(test_qr);
