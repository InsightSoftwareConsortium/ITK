/*
  fsm@robots.ox.ac.uk
*/
#ifdef __GNUC__
#pragma implementation
#endif

#include "vnl_complex.h"
#include <vnl/vnl_math.h>

//--------------------------------------------------------------------------------

// these are actuall suitable for inlining

bool vnl_math_isnan(const vnl_complex<double>& x)
{
  return vnl_math_isnan(x.real()) || vnl_math_isnan(x.imag());
}

bool vnl_math_isfinite(const vnl_complex<double>& x)
{
  return vnl_math_isfinite(x.real()) && vnl_math_isfinite(x.imag());
}

bool vnl_math_isnan(const vnl_complex<float>& x)
{
  return vnl_math_isnan(x.real()) || vnl_math_isnan(x.imag());
}

bool vnl_math_isfinite(const vnl_complex<float>& x)
{
  return vnl_math_isfinite(x.real()) && vnl_math_isfinite(x.imag());
}

/*
float    vnl_math_abs(vcl_complex<float> const& x) { return ::abs(x); }
double   vnl_math_abs(vcl_complex<double> const& x) { return ::abs(x); }

vcl_complex<float>  vnl_math_sqr(vcl_complex<float> const& x) { return x*x; }
vcl_complex<double> vnl_math_sqr(vcl_complex<double> const& x) { return x*x; }

float    vnl_math_squared_magnitude(vcl_complex<float> const& x) { return ::norm(x); }
double   vnl_math_squared_magnitude(vcl_complex<double> const& x) { return ::norm(x); }
*/
