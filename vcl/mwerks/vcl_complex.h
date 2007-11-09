#ifndef vcl_mwerks_complex_h_
#define vcl_mwerks_complex_h_

#include <complex>
#define vcl_generic_complex_STD std
#include "generic/vcl_complex.h"

// the following functions are declared in both <cmath> and <complex>
#undef  vcl_abs
#define vcl_abs vcl_abs
template <typename T> inline T              vcl_abs(vcl_complex<T> z) { return std::abs(z); }

#undef  vcl_sqrt
#define vcl_sqrt vcl_sqrt
template <typename T> inline vcl_complex<T> vcl_sqrt(vcl_complex<T> z) { return std::sqrt(z); }

#undef  vcl_exp
#define vcl_exp vcl_exp
template <typename T> inline vcl_complex<T> vcl_exp(vcl_complex<T> z) { return std::exp(z); }

#undef  vcl_log
#define vcl_log vcl_log
template <typename T> inline vcl_complex<T> vcl_log(vcl_complex<T> z) { return std::log(z); }

#undef  vcl_log10
#define vcl_log10 vcl_log10
template <typename T> inline vcl_complex<T> vcl_log10(vcl_complex<T> z) { return std::log10(z); }

#undef  vcl_cos
#define vcl_cos vcl_cos
template <typename T> inline vcl_complex<T> vcl_cos(vcl_complex<T> z) { return std::cos(z); }

#undef  vcl_cosh
#define vcl_cosh vcl_cosh
template <typename T> inline vcl_complex<T> vcl_cosh(vcl_complex<T> z) { return std::cosh(z); }

#undef  vcl_sin
#define vcl_sin vcl_sin
template <typename T> inline vcl_complex<T> vcl_sin(vcl_complex<T> z) { return std::sin(z); }

#undef  vcl_sinh
#define vcl_sinh vcl_sinh
template <typename T> inline vcl_complex<T> vcl_sinh(vcl_complex<T> z) { return std::sinh(z); }

#undef  vcl_tan
#define vcl_tan vcl_tan
template <typename T> inline vcl_complex<T> vcl_tan(vcl_complex<T> z) { return std::tan(z); }

#undef  vcl_tanh
#define vcl_tanh vcl_tanh
template <typename T> inline vcl_complex<T> vcl_tanh(vcl_complex<T> z) { return std::tanh(z); }

#endif
