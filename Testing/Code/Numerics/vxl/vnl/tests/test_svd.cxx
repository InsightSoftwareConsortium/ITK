#include <vcl_cmath.h>
#include <vcl_cstdlib.h>
#include <vcl_iostream.h>
#include <vcl_complex.h>

#include <vnl/vnl_test.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_double_3.h>
#include <vnl/vnl_matops.h>
#include <vnl/algo/vnl_svd.h>

vnl_matrix<double> solve_with_warning(const vnl_matrix<double>& M,
                                      const vnl_matrix<double>& B)
// Solve LS problem M x = B, warning if M is nearly singular.
{
  // Take svd of vnl_matrix<double> M, trim the singular values at 1e-8,
  // and hold the result.
  vnl_svd<double> svd(M, 1e-8);
  // Check for rank-deficiency
  if (svd.singularities() > 1)
    vcl_cout << "Warning: Singular matrix, condition = " << svd.well_condition() << vcl_endl;
  return svd.solve(B);
}

void test_hilbert()
{
  // Test inversion and recomposition of 5x5 hilbert matrix
  vnl_matrix<double> H(5,5);
  for(int i = 0; i < 5; ++i)
    for(int j = 0; j < 5; ++j)
      H(i,j) = 1.0 / (i+j+1); // sic, because i,j are zero based

  vcl_cout << "H = [ " << H << "]\n";

  vnl_svd<double> svd(H);

  vcl_cout << "rcond(H) = " << svd.well_condition() << vcl_endl;

  vnl_matrix<double> Hinv = svd.inverse();

  vnl_matrix<double> X = Hinv * H;

  vcl_cout << "H*inv(H) = " << X << vcl_endl;

  vnl_matrix<double> I(5,5);
  I = 0.0;
  I.fill_diagonal(1.0);

  vnl_matrix<double> res = X - I;
  vnl_test_assert("Hilbert recomposition residual", res.fro_norm() < 1.1e-10);
}

// Test recovery of parameters of least-squares parabola fit.
void test_ls()
{
  double a = 0.15;
  double b = 1.2;
  double c = 3.1;

  // Generate parabola design matrix
  vnl_matrix<double> D(100, 3);
  for(int n = 0; n < 100; ++n) {
    double x = n;
    D(n, 0) = x*x;
    D(n, 1) = x;
    D(n, 2) = 1.0;
  }

  // Generate Y vector
  vnl_vector<double> y(100);
  {for(int n = 0; n < 100; ++n) {
    double x = n;
    double fx = a * x * x + b * x + c;
    // Add sawtooth "noise"
    y(n) = fx + (n%4 - 2) / 10.0;
  }}
  vcl_cout << "y = [" << y << "]\n";

  // Extract vnl_svd<double>
  vnl_svd<double> svd(D);

  // Solve for parameteers
  vnl_double_3 A = svd.solve(y);
  vcl_cout << "A = " << A << "\n";

  vnl_double_3 T(a,b,c);
  vcl_cout << "residual = " << (A - T).squared_magnitude() << vcl_endl;
  vnl_test_assert("Least squares residual", (A - T).squared_magnitude() < 0.005);
}

// temporarily unused
double test_fmatrix()
{
  double pdata[] = {
    2, 0, 0, 0,
    3, 10, 5, 5,
    5, 12, 6, 6,
  };
  vnl_matrix<double> P(pdata, 3,4);
  vnl_svd<double> svd(P);
  vnl_matrix<double> N = svd.nullspace();
  vcl_cout << "null(P) = " << N << vcl_endl;

  vcl_cout << "P * null(P) = " << P*N << vcl_endl;

  return vcl_sqrt(dot_product(P*N, P*N));
}

// Test nullspace extraction of rank=2 3x4 matrix.
void test_pmatrix()
{
  double pdata[] = {
    2, 0, 0, 0,
    3, 10, 5, 5,
    5, 12, 6, 6,
  };
  vnl_matrix<double> P(pdata, 3,4);
  vnl_svd<double> svd(P, 1e-8);

  vnl_matrix<double> res = svd.recompose() - P;
  vcl_cout << "Recomposition residual = " << res.fro_norm() << vcl_endl;
  vnl_test_assert("PMatrix recomposition residual", res.fro_norm() < 1e-12);
  vcl_cout << " Inv = " << svd.inverse() << vcl_endl;

  vnl_test_assert("singularities = 2", svd.singularities() == 2);
  vnl_test_assert("rank = 2", svd.rank() == 2);

  vnl_matrix<double> N = svd.nullspace();
  vnl_test_assert("nullspace dimension", N.columns() == 2);
  vcl_cout << "null(P) = \n" << N << vcl_endl;

  vnl_matrix<double> PN = P*N;
  vcl_cout << "P * null(P) = \n" << PN << vcl_endl;
  vcl_cout << "nullspace residual = " << PN.fro_norm() << vcl_endl;
  vnl_test_assert("P nullspace residual", PN.fro_norm() < 1e-12);

  vnl_vector<double> n = svd.nullvector();
  vcl_cout << "nullvector residual = " << (P*n).magnitude() << vcl_endl;
  vnl_test_assert("P nullvector residual", (P*n).magnitude() < 1e-12);

  vnl_vector<double> l = svd.left_nullvector();
  vcl_cout << "left_nullvector(P) = " << l << vcl_endl;
  vcl_cout << "left_nullvector residual = " << (l*P).magnitude() << vcl_endl;
  vnl_test_assert("P left nullvector residual", (l*P).magnitude() < 1e-12);
}

void test_I()
{
  double Idata[] = {
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
  };
  vnl_matrix<double> P(3, 4, 12, Idata);
  vnl_svd<double> svd(P);
  vcl_cout << svd;
}

template <class T>
void test_svd_recomposition(char const *type, double maxres, T * /*tag*/)
{
  // Test inversion of 5x5 matrix of T :
  vcl_cout << "----- testing vnl_svd<" << type << "> recomposition -----" << vcl_endl;

  vnl_matrix<T> A(5,5);
  vnl_test_fill_random(A.begin(), A.end());

  vcl_cout << "A = [ " << vcl_endl << A << "]" << vcl_endl;
  vnl_svd<T> svd(A);

  vnl_matrix<T> B=svd.recompose();
  vcl_cout << "B = [ " << vcl_endl << B << "]" << vcl_endl;

  double residual=(A - B).fro_norm();
  vcl_cout << "residual=" << residual << vcl_endl;
  vnl_test_assert("vnl_svd<float> recomposition residual", residual < maxres);
}

template void test_svd_recomposition(char const *, double, float *);
template void test_svd_recomposition(char const *, double, double *);
template void test_svd_recomposition(char const *, double, vcl_complex<float> *);
template void test_svd_recomposition(char const *, double, vcl_complex<double> *);

// Driver
void test_svd()
{
  test_hilbert();
  test_ls();
  test_pmatrix();
  test_I();
  test_svd_recomposition("float",              1e-5 , (float*)0);
  test_svd_recomposition("double",             1e-10, (double*)0);
  test_svd_recomposition("vcl_complex<float>",  1e-5 , (vcl_complex<float>*)0);
  test_svd_recomposition("vcl_complex<double>", 1e-10, (vcl_complex<double>*)0);
}

TESTMAIN(test_svd);
