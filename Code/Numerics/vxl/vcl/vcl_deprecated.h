#ifndef vcl_deprecated_h_
#define vcl_deprecated_h_
/*
  fsm@robots.ox.ac.uk
*/

#include "vcl_compiler.h"

#if defined(VCL_GCC)
# warning "deprecated"

#elif defined(VCL_SGI_CC)
  int /* deprecated */;

#elif defined(VCL_SUNPRO_CC)
# error "deprecated"

#else
// # pragma warning deprecated
#endif

#endif // vcl_deprecated_h_
