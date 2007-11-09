#ifndef vcl_mwerks_cstdlib_h_
#define vcl_mwerks_cstdlib_h_

#include <cstdlib>
#define vcl_generic_cstdlib_STD /* */
#include "generic/vcl_cstdlib.h"

// the following functions are declared in both <cmath> and <cstdlib>
#undef  vcl_abs
#define vcl_abs vcl_abs
inline int vcl_abs(int x) { return ::abs(x); }
inline long vcl_abs(long x) { return (x >= 0) ? x : -x; } // no "long ::abs(long)"

#endif
