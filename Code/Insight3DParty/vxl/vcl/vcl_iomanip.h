#ifndef vcl_iomanip_h_
#define vcl_iomanip_h_
/*
  fsm@robots.ox.ac.uk
*/

#include "vcl_compiler.h"

#if defined(VCL_SGI_CC_720)
# include <iomanip.h>
# define vcl_generic_iomanip_STD /* */
# include "generic/vcl_iomanip.h"

#else
# include "iso/vcl_iomanip.h"
#endif

#endif // vcl_iomanip_h_
