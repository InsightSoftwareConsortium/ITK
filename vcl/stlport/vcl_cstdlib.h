#ifndef vcl_stlport_cstdlib_h_
#define vcl_stlport_cstdlib_h_

#if defined (VCL_GCC)
# include "../gcc/vcl_cstdlib.h"

#elif VCL_VC60
# define vcl_generic_cstdlib_STD /* */
# include "../generic/vcl_cstdlib.h"

# include <stdlib.h>
# undef  vcl_abs
inline int vcl_abs(int x) { return x >= 0 ? x : -x; }
inline long vcl_abs(long x) { return x >= 0 ? x : -x; }
# define vcl_abs vcl_abs

//#undef  vcl_div
//#define vcl_div vcl_div
//inline div_t vcl_div(int x, int y) { return ::div(x,y); }
//inline ldiv_t vcl_div(long x, long y) { return ::ldiv(x,y); }

//#undef  vcl_ldiv
//#define vcl_ldiv vcl_ldiv
//inline ldiv_t vcl_ldiv(long x, long y) { return ::ldiv(x,y); }

#else
#include "../iso/vcl_cmath.h"
#endif

#endif // vcl_stlport_cstdlib_h_
