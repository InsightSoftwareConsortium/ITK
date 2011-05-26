#ifndef vcl_win32_vc70_cstdlib_h_
#define vcl_win32_vc70_cstdlib_h_

#include <cstdlib>

#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif

#define vcl_generic_cstdlib_STD std
#include "../generic/vcl_cstdlib.h"

inline int vcl_abs(int x) { return x >= 0 ? x : -x; }
inline long vcl_abs(long x) { return x >= 0 ? x : -x; }
inline long long vcl_abs(long long x) {return _abs64(x); }
#endif // vcl_win32_vc70_cstdlib_h_
