#ifndef vcl_ciso646_h_
#define vcl_ciso646_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CISO646
# if defined(VCL_BORLAND_55) // Borland C++ 5.5 does not provide this at all.
#  include "borland55/vcl_ciso646.h"
# else
#  include <iso646.h>
#endif
#else
# include "iso/vcl_ciso646.h"
#endif

#endif // vcl_ciso646_h_
