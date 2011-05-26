#ifndef vcl_clocale_h_
#define vcl_clocale_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CLOCALE
# include <locale.h>
#else
# include "iso/vcl_clocale.h"
#endif

#endif // vcl_clocale_h_
