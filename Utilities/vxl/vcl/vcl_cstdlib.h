// This is vcl/vcl_cstdlib.h
#ifndef vcl_cstdlib_h_
#define vcl_cstdlib_h_
//:
// \file
//
// [26.5.4] In addition to the signatures from <stdlib.h> the C++
// header <cstdlib> adds the overloaded signatures :
//   long   abs(long);        // labs()
//   ldiv_t div(long, long);  // ldiv()
//
// NB: size_t is declared in <cstddef>, not <cstdlib>

#include "vcl_compiler.h"

#if defined(VCL_GCC) && !defined(GNU_LIBSTDCXX_V3)
# include "gcc/vcl_cstdlib.h"
#elif defined(VCL_SGI_CC)
# include "sgi/vcl_cstdlib.h"
#elif defined(VCL_SUNPRO_CC)
# include "sunpro/vcl_cstdlib.h"
#elif VCL_VC60
# include "win32-vc60/vcl_cstdlib.h"
#elif VCL_VC70
# include "win32-vc70/vcl_cstdlib.h" 
#elif defined(VCL_METRO_WERKS)
# include "mwerks/vcl_cstdlib.h"
#else
# include "iso/vcl_cstdlib.h"
#endif

#endif // vcl_cstdlib_h_
