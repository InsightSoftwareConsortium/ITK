#ifndef vcl_ciso646_h_
#define vcl_ciso646_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CISO646
# if !VCL_CXX_HAS_HEADER_ISO646_H
#  include "emulation/vcl_ciso646.h"
# else
#  include <iso646.h>
#endif
#else
# include "iso/vcl_ciso646.h"
#endif

#endif // vcl_ciso646_h_
