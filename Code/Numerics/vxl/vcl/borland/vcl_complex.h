#ifndef vcl_borland_complex_h_
#define vcl_borland_complex_h_

// fsm@robots: complex<T> is derived from _Complex_base<T>, which is not
// declared with __declspec(dllimport). So complex<T> shouldn't be either
// (else the compiler will emit an error). Whether or not it is depends on
// the value of _CRTIMP being set, e.g. in <math.h>
# include <vcl_iostream.h>
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
{ return sqrt(::norm(z)); }

template <class T> 
inline  vcl_complex<T> sqrt(vcl_complex<T> const &x)
{
  T r = abs (x);
  T nr, ni;
  if (r == 0.0)
    nr = ni = r;
  else if (x.real () > 0)
    {
      nr = vcl_sqrt (0.5 * (r + x.real ()));
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

#define vcl_generic_complex_STD std
#include "../generic/vcl_complex.h"

#endif // vcl_borland_complex_h_
