// This is vcl/vcl_limits.h
#ifndef vcl_limits_h_
#define vcl_limits_h_
//:
// \file
// \author fsm

#include "vcl_compiler.h"

#if VCL_CXX_HAS_HEADER_LIMITS
#include <limits>
#else
#include <limits.h>
#include <float.h>
#endif

#endif // vcl_limits_h_
