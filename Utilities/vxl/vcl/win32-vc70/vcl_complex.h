#ifndef vcl_win32_vc70_complex_h_
#define vcl_win32_vc70_complex_h_

#include <complex>

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
#ifndef vcl_pow
# define vcl_pow vcl_pow
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
template <class T> vcl_complex<T> vcl_pow(const vcl_complex<T>& x, int y) { return std::pow(x, y); }
template <class T> vcl_complex<T> vcl_pow(const vcl_complex<T>& x, const vcl_complex<T>& y) { return std::pow(x, y); }
template <class T> vcl_complex<T> vcl_pow(const T& x, const vcl_complex<T>& y) { return std::pow(x, y); }
template <class T> vcl_complex<T> vcl_sin(const vcl_complex<T>& x) { return std::sin(x); }
template <class T> vcl_complex<T> vcl_sinh(const vcl_complex<T>& x) { return std::sinh(x); }
template <class T> vcl_complex<T> vcl_sqrt(const vcl_complex<T>& x) { return std::sqrt(x); }
template <class T> vcl_complex<T> vcl_tan(const vcl_complex<T>& x) { return std::tan(x); }
template <class T> vcl_complex<T> vcl_tanh(const vcl_complex<T>& x) { return std::tanh(x); }

#endif // vcl_win32_vc70_complex_h_
