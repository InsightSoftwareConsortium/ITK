#ifndef vcl_win32_vc60_cstdlib_h_
#define vcl_win32_vc60_cstdlib_h_

#include <cstdlib>

#define vcl_generic_cstdlib_STD

inline long abs(long x) { return x >= 0 ? x : -x; }

#include "../generic/vcl_cstdlib.h"

#endif // vcl_win32_vc60_cstdlib_h_
