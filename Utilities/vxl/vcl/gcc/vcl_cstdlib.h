// This is vcl/gcc/vcl_cstdlib.h
#ifndef vcl_gcc_cstdlib_h_
#define vcl_gcc_cstdlib_h_

// Include system stdlib.h
#if defined(VCL_CXX_HAS_HEADER_CSTDLIB) && !VCL_CXX_HAS_HEADER_CSTDLIB
# include <stdlib.h>
# define vcl_generic_cstdlib_STD /* */
# include "../generic/vcl_cstdlib.h"
# undef vcl_abs
inline int  vcl_abs(int  x) { return x >= 0 ? x : -x; }
inline long vcl_abs(long x) { return x >= 0 ? x : -x; }
#define vcl_abs vcl_abs

#else
# include "../iso/vcl_cstdlib.h"
#endif

#endif // vcl_gcc_cstdlib_h_
