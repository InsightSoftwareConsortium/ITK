#ifndef vcl_iomanip_h_
#define vcl_iomanip_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if defined(VCL_GCC_295)
# include <gcc-295/vcl_iomanip.h>
#elif defined(VCL_SGI_CC_720)
# include <sgi/vcl_iomanip.h>
#else
# include "iso/vcl_iomanip.h"
#endif

#endif // vcl_iomanip_h_
