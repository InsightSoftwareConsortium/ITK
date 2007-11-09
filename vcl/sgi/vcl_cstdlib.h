#ifndef vcl_sgi_cstdlib_h_
#define vcl_sgi_cstdlib_h_

// Include system stdlib.h
#if defined(VCL_SGI_CC_720) || (defined(VCL_CXX_HAS_HEADER_CSTDLIB) && !VCL_CXX_HAS_HEADER_CSTDLIB)
# include <stdlib.h>
# define vcl_generic_cstdlib_STD /* */
# include "../generic/vcl_cstdlib.h"
# undef vcl_abs
# define vcl_abs vcl_abs
inline int  vcl_abs(int  x) { return x >= 0 ? x : -x; }
inline long vcl_abs(long x) { return x >= 0 ? x : -x; }

#else
# include "../iso/vcl_cstdlib.h"
#endif

#endif // vcl_sgi_cstdlib_h_
