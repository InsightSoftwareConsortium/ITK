#ifndef vcl_limits_h_
#define vcl_limits_h_
/*
  fsm@robots.ox.ac.uk
*/

#include "vcl_compiler.h"

#if VCL_HAS_HEADER_LIMITS
#include <limits>
#else
#include <limits.h>
#include <float.h>
#endif

#endif // vcl_limits_h_
