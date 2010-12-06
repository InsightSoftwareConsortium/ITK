#ifndef vcl_win32_vc_8_valarray_h_
#define vcl_win32_vc_8_valarray_h_

// VC7 does not define abs functions correctly in  cstdlib.
// The vcl versions of these headers declare the functions with vcl_
// prefixes, so we must do the same here.

#include <valarray>

#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif

#define vcl_generic_valarray_STD std
#include "../generic/vcl_valarray.h"

template <class T> inline vcl_valarray<T> vcl_abs(const vcl_valarray<T>& x) { return std::abs(x); }

#endif // vcl_win32_vc_8_valarray_h_
