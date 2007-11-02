#ifndef vcl_win32_vc60_complex_h_
#define vcl_win32_vc60_complex_h_


// fsm: complex<T> is derived from _Complex_base<T>, which is not
// declared with __declspec(dllimport). So complex<T> shouldn't be either
// (else the compiler will emit an error). Whether or not it is depends on
// the value of _CRTIMP being set, e.g. in <math.h>

// However, modifying _CRTIMP is bad because it leads to inconsistent
// dll linkage issues for other functions. <complex> includes other
// things, like the streams. If _CRTIMP is changes, then the
// declaration of the streams effectively change. This causes all
// kinds of problems.
//
// The compiler issues a warning about the inconsistency in <complex>,
// but it seems that the warning can be safely ignored provided the
// implementors wrote the code correcting. Since this is part of the
// compiler system, we can assume that the implementers have
// addressed the issue, and that we can safely ignore the warning. A
// couple of articles on the web seem to support this position:
// http://support.microsoft.com/default.aspx?scid=http://support.microsoft.com:80/support/kb/articles/Q134/9/80.asp&NoWebContent=1
// http://www.osl.iu.edu/MailArchives/mtl-devel/msg00395.php

// Disable "warning C4275: non dll-interface class
// 'std::_Complex_base<float>' used as base for dll-interface class
// 'std::complex<float>'" for <complex>
//
#pragma warning (push)
#pragma warning (disable: 4275)
#include <complex>
#pragma warning (pop)

// It used to necessary to bring the complex math functions from the
// std namespace into the global namespace to avoid conflicts with the
// (incorrect) cmath and cstdlib headers.  Then these headers were
// updated to define the functions with a vcl_ prefix.  We must do the
// same here.

// We want the generic version of the header to define only
// vcl_complex.
#ifndef vcl_real
# define vcl_real vcl_real
#endif
#ifndef vcl_imag
# define vcl_imag vcl_imag
#endif
#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif
#ifndef vcl_arg
# define vcl_arg vcl_arg
#endif
#ifndef vcl_norm
# define vcl_norm vcl_norm
#endif
#ifndef vcl_conj
# define vcl_conj vcl_conj
#endif
#ifndef vcl_polar
# define vcl_polar vcl_polar
#endif
#ifndef vcl_cos
# define vcl_cos vcl_cos
#endif
#ifndef vcl_cosh
# define vcl_cosh vcl_cosh
#endif
#ifndef vcl_exp
# define vcl_exp vcl_exp
#endif
#ifndef vcl_log
# define vcl_log vcl_log
#endif
#ifndef vcl_log10
# define vcl_log10 vcl_log10
#endif
#ifndef vcl_sin
# define vcl_sin vcl_sin
#endif
#ifndef vcl_sinh
# define vcl_sinh vcl_sinh
#endif
#ifndef vcl_sqrt
# define vcl_sqrt vcl_sqrt
#endif
#ifndef vcl_tan
# define vcl_tan vcl_tan
#endif
#ifndef vcl_tanh
# define vcl_tanh vcl_tanh
#endif

#define vcl_generic_complex_STD std
#include "../generic/vcl_complex.h"

template <class T> T vcl_real(const vcl_complex<T>& x) { return std::real(x); }
template <class T> T vcl_imag(const vcl_complex<T>& x) { return std::imag(x); }
template <class T> T vcl_abs(const vcl_complex<T>& x) { return std::abs(x); }
template <class T> T vcl_arg(const vcl_complex<T>& x) { return std::arg(x); }
template <class T> T vcl_norm(const vcl_complex<T>& x) { return std::norm(x); }
template <class T> vcl_complex<T> vcl_conj(const vcl_complex<T>& x) { return std::conj(x); }
template <class T> vcl_complex<T> vcl_polar(const T& rho, const T& theta = 0) { return std::polar(rho, theta); }
template <class T> vcl_complex<T> vcl_cos(const vcl_complex<T>& x) { return std::cos(x); }
template <class T> vcl_complex<T> vcl_cosh(const vcl_complex<T>& x) { return std::cosh(x); }
template <class T> vcl_complex<T> vcl_exp(const vcl_complex<T>& x) { return std::exp(x); }
template <class T> vcl_complex<T> vcl_log(const vcl_complex<T>& x) { return std::log(x); }
template <class T> vcl_complex<T> vcl_log10(const vcl_complex<T>& x) { return std::log10(x); }
template <class T> vcl_complex<T> vcl_sin(const vcl_complex<T>& x) { return std::sin(x); }
template <class T> vcl_complex<T> vcl_sinh(const vcl_complex<T>& x) { return std::sinh(x); }
template <class T> vcl_complex<T> vcl_sqrt(const vcl_complex<T>& x) { return std::sqrt(x); }

// Visual Studio 6 does not provide tan or tanh for complex.
// These are adapted from the VS 7.0 implementation.
template <class T> vcl_complex<T> vcl_tanh(const vcl_complex<T>& x)
{
  typedef std::_Ctr<T> Ctr;
  const T one = 1;
  T t = ::tan(vcl_imag(x));
  T s = Ctr::_Sinh(vcl_real(x), one);
  T b = s * (one + t * t);
  T d = one + b * s;
  return vcl_complex<T>(Ctr::sqrt(one + s * s) * b / d, t / d);
}
template <class T> vcl_complex<T> vcl_tan(const vcl_complex<T>& x)
{
  vcl_complex<T> z(vcl_tanh(vcl_complex<T>(-vcl_imag(x), vcl_real(x))));
  return vcl_complex<T>(vcl_imag(z), -vcl_real(z));
}

#endif // vcl_win32_vc60_complex_h_
