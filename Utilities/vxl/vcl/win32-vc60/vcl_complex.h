#ifndef vcl_win32_vc60_complex_h_
#define vcl_win32_vc60_complex_h_

// fsm: complex<T> is derived from _Complex_base<T>, which is not
// declared with __declspec(dllimport). So complex<T> shouldn't be either
// (else the compiler will emit an error). Whether or not it is depends on
// the value of _CRTIMP being set, e.g. in <math.h>

// These stream includes may not appear necessary, but lots of link
// errors may appear if they aren't here. IMS
// It seems that these includes must appear for the first time before
// _CRTIMP is modified below.  Otherwise, basic_filebuf (and others?)
// member functions get defined more than once.  It is possible that
// other stream headers should be included here as well. FWW
# include <vcl_iostream.h>      // don't remove
# include <vcl_fstream.h>       // don't remove

# include <vcl_cmath.h>
# pragma warning (push)
# pragma warning (disable: 4273)
# undef _CRTIMP
# define _CRTIMP
# include <ctype.h>
# include <complex>
# pragma warning (pop)

#ifndef vcl_complex
# define vcl_complex std :: complex
#endif

template <class T> inline
T norm(vcl_complex<T> const &z) 
{ return z.real()*z.real() + z.imag()*z.imag(); }
template <class T> inline
T abs(vcl_complex<T> const &z)
{ return vcl_sqrt(norm(z)); }

template <class T> 
inline  vcl_complex<T> sqrt(vcl_complex<T> const &x)
{
  T r = abs (x);
  T nr, ni;
  if (r == 0.0)
    nr = ni = r;
  else if (x.real () > 0)
    {
      nr = vcl_sqrt (0.5 * (r + x.real()));
      ni = x.imag() / nr / 2;
    }
  else
    {
      ni = vcl_sqrt (0.5 * (r - x.real()));
      if (x.imag() < 0)
        ni = - ni;
      nr = x.imag() / ni / 2;
    }
  return vcl_complex<T> (nr, ni);
}

// complex exponent: scale by exp(x.real) and rotate by x.imag .
template <class T> 
inline  vcl_complex<T> exp(vcl_complex<T> const& x)
{
  T r = exp(x.real());
  return vcl_complex<T> (r*cos(x.imag()), r*sin(x.imag()));
}

#define vcl_generic_complex_STD std
#include "../generic/vcl_complex.h"

// Add forwarding functions for std::cos, etc..  These should not be templated, as the
// ANSI ones are not templated.  If they were, they would mess up user code which
// tries to declare other cos functions. 
// We can't just define vcl_cos to be std :: cos because it will not then resolve
// overloads for cos(double) -- hmm. this may be a problem with the std, not with
// vcl...
inline std :: complex<long double> cos(std :: complex<long double> const& z) { return std :: cos( z ); }
inline std :: complex<double> cos(std :: complex<double> const& z) { return std :: cos( z ); }
inline std :: complex<float> cos(std :: complex<float> const& z) { return std :: cos( z ); }

inline std :: complex<long double> sin(std :: complex<long double> const& z) { return std :: sin( z ); }
inline std :: complex<double> sin(std :: complex<double> const& z) { return std :: sin( z ); }
inline std :: complex<float> sin(std :: complex<float> const& z) { return std :: sin( z ); }

inline std :: complex<long double> pow(std :: complex<long double> const& z, long double y) { return std :: pow( z , y ); }
inline std :: complex<double> pow(std :: complex<double> const& z, double y) { return std :: pow( z , y ); }
inline std :: complex<float> pow(std :: complex<float> const& z, float y) { return std :: pow( z , y ); }

inline std :: complex<long double> pow(std :: complex<long double> const& z, int y) { return std :: pow( z , y ); }
inline std :: complex<double> pow(std :: complex<double> const& z, int y) { return std :: pow( z , y ); }
inline std :: complex<float> pow(std :: complex<float> const& z, int y) { return std :: pow( z , y ); }

inline std :: complex<long double> pow(std :: complex<long double> const& z, std :: complex<long double> const&  y)
  { return std :: pow( z , y ); }
inline std :: complex<double> pow(std :: complex<double> const& z, std :: complex<double> const& y) { return std :: pow( z , y ); }
inline std :: complex<float> pow(std :: complex<float> const& z, std :: complex<float> const& y) { return std :: pow( z , y ); }

inline std :: complex<long double> pow(long double z, std :: complex<long double> const&  y) { return std :: pow( z , y ); }
inline std :: complex<double> pow(double z, std :: complex<double> const& y) { return std :: pow( z , y ); }
inline std :: complex<float> pow(float z, std :: complex<float> const& y) { return std :: pow( z , y ); }


#endif // vcl_win32_vc60_complex_h_
