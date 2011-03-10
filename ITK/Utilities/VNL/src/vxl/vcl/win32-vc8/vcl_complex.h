#ifndef vcl_win32_vc_8_complex_h_
#define vcl_win32_vc_8_complex_h_

#include <complex>

// It used to necessary to bring the complex abs functions from the
// std namespace into the global namespace to avoid conflicts with the
// (incorrect) cstdlib headers.  Then these headers were
// updated to define the functions with a vcl_ prefix.  We must do the
// same here.

#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif


#define vcl_generic_complex_STD std
#include "../generic/vcl_complex.h"

template <class T> inline T vcl_abs(const vcl_complex<T>& x) { return std::abs(x); }

#endif // vcl_win32_vc_8_complex_h_
