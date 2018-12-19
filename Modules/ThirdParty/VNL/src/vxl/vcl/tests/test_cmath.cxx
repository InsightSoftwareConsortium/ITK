/*
  fsm
*/

// The purpose of this is to check there are no
// clashes between std::sqrt() and std::abs().
#include <complex>
#include <iostream>
#include <cmath>
#include <cstdlib>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif


int test_cmath_main(int /*argc*/,char* /*argv*/[])
{
  {
    int    xi = 314159265;
    long   xl = 314159265L;
    float  xf = 13.14159265358979323846f;
    double xd = 23.14159265358979323846;
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    long double ld = xd;
#endif
    std::complex<double> xc(xd,0.0);

#define macro(var, type) \
do { \
  if (std::abs(var) == (var) && std::abs(- (var)) == (var)) \
    std::cout << "std::abs(" #type ") PASSED" << std::endl; \
  else \
    std::cerr << "std::abs(" #type ") *** FAILED *** " << std::endl; \
} while (false)
    macro(xi, int);
    macro(xl, long);
    macro(xf, float);
    macro(xd, double);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
    macro(ld, long double);
#endif
    macro(xc, std::complex<double>);
#undef macro
  }

  {
    // This shows why
    //   #define std::cos cos
    // isn't good enough. It has to be
    //   #define std::cos ::cos
    // or
    //   #define std::cos std::cos
    double theta = 0.1234;
    double cos = std::cos(theta);
    double sin = std::sin(theta);
    double tan = std::tan(theta);
    (void)theta; (void)cos; (void)sin; (void)tan; // quell 'unused variable' warning.
  }

#define macro(T, eps) \
  do { \
    T x = 2; \
    T y = std::sqrt(x); \
    if (std::abs(x - y*y) < (eps)) \
      std::cout << "std::sqrt(" #T ") PASSED" << std::endl; \
    else \
      std::cout << "std::sqrt(" #T ") *** FAILED *** " << std::endl; \
  } while (false)
  macro(float, 1e-6);        // actually sqrtf()
  macro(double, 1e-14);
#ifdef INCLUDE_LONG_DOUBLE_TESTS
  macro(long double, 1e-14); // actually sqrtl()
#endif
#undef macro

  return 0;
}
