/*
  fsm
*/

// The purpose of this is to check there are no
// clashes between vcl_sqrt() and vcl_abs().
#include <vcl_complex.h>
#include <vcl_cmath.h>
#include <vcl_cstdlib.h>

#include <vcl_iostream.h>

int test_cmath_main(int /*argc*/,char* /*argv*/[])
{
  {
    int    xi = 314159265;
    long   xl = 314159265L;
    float  xf = 13.14159265358979323846f;
    double xd = 23.14159265358979323846;
    long double ld = xd;
    vcl_complex<double> xc(xd,0.0);

#define macro(var, type) \
do { \
  if (vcl_abs(var) == var && vcl_abs(- var) == var) \
    vcl_cout << "vcl_abs(" #type ") PASSED" << vcl_endl; \
  else \
    vcl_cerr << "vcl_abs(" #type ") *** FAILED *** " << vcl_endl; \
} while (false)
    macro(xi, int);
    macro(xl, long);
    macro(xf, float);
    macro(xd, double);
    macro(ld, long double);
    macro(xc, vcl_complex<double>);
#undef macro
  }

  {
    // This shows why
    //   #define vcl_cos cos
    // isn't good enough. It has to be
    //   #define vcl_cos ::cos
    // or
    //   #define vcl_cos std::cos
    double theta = 0.1234;
    double cos = vcl_cos(theta);
    double sin = vcl_sin(theta);
    double tan = vcl_tan(theta);
    (void)theta; (void)cos; (void)sin; (void)tan; // quell 'unused variable' warning.
  }

#define macro(T, eps) \
  do { \
    T x = 2; \
    T y = vcl_sqrt(x); \
    if (vcl_abs(x - y*y) < eps) \
      vcl_cout << "vcl_sqrt(" #T ") PASSED" << vcl_endl; \
    else \
      vcl_cout << "vcl_sqrt(" #T ") *** FAILED *** " << vcl_endl; \
  } while (false)
  macro(float, 1e-6);        // actually sqrtf()
  macro(double, 1e-14);
  macro(long double, 1e-14); // actually sqrtl()
#undef macro

  return 0;
}
