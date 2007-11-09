#ifndef vcl_csetjmp_h_
#define vcl_csetjmp_h_
/*
  Peter Vanroose, esat.kuleuven.be
*/

/* This should define C-style stack unwinding */

#include "vcl_compiler.h"

// SunPro 5.0's <csetjmp> is broken.
// VisualC++'s <csetjmp> does not use namespace std.
#if !VCL_CXX_HAS_HEADER_CSETJMP || defined(VCL_SUNPRO_CC_5) || defined(VCL_VC60)
# include <setjmp.h>
# define vcl_generic_csetjmp_STD /* */
# include "generic/vcl_csetjmp.h"
#else
# include "iso/vcl_csetjmp.h"
#endif

// In ISO C, setjmp() is a macro. So in vcl it should be
// a macro without the vcl_ prefix, just like assert().
// It is good that people know assert() is a macro because
// they treat it with more care than a function. If vcl
// provided a `vcl_setjmp' preprocessor macro people might
// think they were using a function and not a macro.

#endif // vcl_csetjmp_h_
