#ifndef vcl_climits_h_
#define vcl_climits_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CLIMITS
# include <limits.h>
#elif defined(VCL_SUNPRO_CC_5)
# include <limits.h> // <climits> is broken -- mismatched braces.
#else
# include "iso/vcl_climits.h"
#endif

#endif // vcl_climits_h_
