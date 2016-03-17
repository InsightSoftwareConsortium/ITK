// This is vcl/vcl_ios.h
#ifndef vcl_ios_h_
#define vcl_ios_h_
//:
// \file
// \author fsm

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_IOS
# include "vcl_iostream.h" // should do it
# define vcl_generic_ios_STD /* */
# include "generic/vcl_ios.h"

#else // -------------------- ISO
# include "iso/vcl_ios.h"
#endif

#endif // vcl_ios_h_
