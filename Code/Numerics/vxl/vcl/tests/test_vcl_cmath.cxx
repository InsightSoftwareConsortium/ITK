/*
  fsm@robots.ox.ac.uk
*/

// The purpose of this is to check there are no
// clashes between vcl_sqrt() and vcl_abs().
#include <vcl_complex.h>
#include <vcl_cmath.h>
#include <vcl_cstdlib.h>

#include <vcl_iostream.h>

int main()
{
  {
    int    xi = 314159265;
    long   xl = 314159265L;
    float  xf = 13.14159265358979323846;
    double xd = 23.14159265358979323846;
    // + long double
    
#define macro(cond, type) \
do { \
  if (cond) \
    vcl_cout << "vcl_abs(" #type ") PASSED" << vcl_endl; \
  else \
    vcl_cerr << "vcl_abs(" #type ") *** FAILED *** " << vcl_endl; \
} while (false)
    macro(vcl_abs(- xi) == xi, int);
    macro(vcl_abs(- xl) == xl, long);
    macro(vcl_abs(- xf) == xf, float);
    macro(vcl_abs(- xd) == xd, double);
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
    theta = cos + sin + tan; // quell 'unused variable' warning.
  }
  
#define macro(T) \
  do { \
    T x = 2; \
    T y = vcl_sqrt(x); \
    vcl_cout << x - y*y << vcl_endl; \
  } while (false)
  macro(float);       // e-08
  macro(double);      // e-16
  macro(long double); // e-19
#undef macro
  
  return 0;
}
