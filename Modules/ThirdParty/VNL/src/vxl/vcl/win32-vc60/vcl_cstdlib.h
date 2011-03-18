#ifndef vcl_win32_vc60_cstdlib_h_
#define vcl_win32_vc60_cstdlib_h_

#include <cstdlib>

// VC6 does not declare the cstdlib functions in the std namespace.

#ifndef vcl_abs
# define vcl_abs vcl_abs
#endif
#ifndef vcl_labs
# define vcl_labs vcl_labs
#endif
#ifndef vcl_div
# define vcl_div vcl_div
#endif
#ifndef vcl_ldiv
# define vcl_ldiv vcl_ldiv
#endif

#define vcl_generic_cstdlib_STD
#include "../generic/vcl_cstdlib.h"

inline int vcl_abs(int x) { return x >= 0 ? x : -x; }
inline long vcl_abs(long x) { return x >= 0 ? x : -x; }
inline long vcl_labs(long x) { return x >= 0 ? x : -x; }
inline div_t vcl_div(int x, int y) { return ::div(x,y); }
inline ldiv_t vcl_div(long x, long y) { return ::ldiv(x,y); }  
inline ldiv_t vcl_ldiv(long x, long y) { return ::ldiv(x,y); }

#endif // vcl_win32_vc60_cstdlib_h_
