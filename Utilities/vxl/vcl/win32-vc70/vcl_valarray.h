#ifndef vcl_win32_vc70_valarray_h_
#define vcl_win32_vc70_valarray_h_

// VC7 does not define math functions correctly in cmath and cstdlib.
// The vcl versions of these headers declare the functions with vcl_
// prefixes, so we must do the same here.

#include <valarray>

#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif
#ifndef vcl_acos
# define vcl_acos vcl_acos
#endif
#ifndef vcl_asin
# define vcl_asin vcl_asin
#endif
#ifndef vcl_atan
# define vcl_atan vcl_atan
#endif
#ifndef vcl_atan2
# define vcl_atan2 vcl_atan2
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

#define vcl_generic_valarray_STD std
#include "../generic/vcl_valarray.h"

template <class T> vcl_valarray<T> vcl_abs(const vcl_valarray<T>& x) { return std::abs(x); }
template <class T> vcl_valarray<T> vcl_acos(const vcl_valarray<T>& x) { return std::acos(x); }
template <class T> vcl_valarray<T> vcl_asin(const vcl_valarray<T>& x) { return std::asin(x); }
template <class T> vcl_valarray<T> vcl_atan(const vcl_valarray<T>& x) { return std::atan(x); }
template <class T> vcl_valarray<T> vcl_atan2(const vcl_valarray<T>& x, const vcl_valarray<T>& y) { return std::atan2(x,y); }
template <class T> vcl_valarray<T> vcl_atan2(const vcl_valarray<T>& x, const T& y) { return std::atan2(x,y); }
template <class T> vcl_valarray<T> vcl_atan2(const T& x, const vcl_valarray<T>& y) { return std::atan2(x,y); }
template <class T> vcl_valarray<T> vcl_cos(const vcl_valarray<T>& x) { return std::cos(x); }
template <class T> vcl_valarray<T> vcl_cosh(const vcl_valarray<T>& x) { return std::cosh(x); }
template <class T> vcl_valarray<T> vcl_exp(const vcl_valarray<T>& x) { return std::exp(x); }
template <class T> vcl_valarray<T> vcl_log(const vcl_valarray<T>& x) { return std::log(x); }
template <class T> vcl_valarray<T> vcl_log10(const vcl_valarray<T>& x) { return std::log10(x); }
template <class T> vcl_valarray<T> vcl_pow(const vcl_valarray<T>& x, const vcl_valarray<T>& y) { return std::pow(x,y); }
template <class T> vcl_valarray<T> vcl_pow(const vcl_valarray<T>& x, const T& y) { return std::pow(x,y); }
template <class T> vcl_valarray<T> vcl_pow(const T& x, const vcl_valarray<T>& y) { return std::pow(x,y); }
template <class T> vcl_valarray<T> vcl_sin(const vcl_valarray<T>& x) { return std::sin(x); }
template <class T> vcl_valarray<T> vcl_sinh(const vcl_valarray<T>& x) { return std::sinh(x); }
template <class T> vcl_valarray<T> vcl_sqrt(const vcl_valarray<T>& x) { return std::sqrt(x); }
template <class T> vcl_valarray<T> vcl_tan(const vcl_valarray<T>& x) { return std::tan(x); }
template <class T> vcl_valarray<T> vcl_tanh(const vcl_valarray<T>& x) { return std::tanh(x); }

#endif // vcl_win32_vc70_valarray_h_
