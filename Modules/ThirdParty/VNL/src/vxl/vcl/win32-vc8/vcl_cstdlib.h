#ifndef vcl_win32_vc_8_cstdlib_h_
#define vcl_win32_vc_8_cstdlib_h_

#include <cstdlib>

#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif

#define vcl_generic_cstdlib_STD std
#include "../generic/vcl_cstdlib.h"

// Use compiler intrinsics 
// see http://msdn.microsoft.com/en-us/library/5704bbxw(VS.80).aspx
inline int vcl_abs(int x) { return abs(x); }
inline long vcl_abs(long x) { return labs(x); }
inline long long vcl_abs(long long x) { return _abs64(x); }
#endif // vcl_win32_vc_8_cstdlib_h_
