#ifndef vcl_cwchar_h_
#define vcl_cwchar_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CWCHAR
# include <wchar.h>
#else
# include "iso/vcl_cwchar.h"
#endif

#endif // vcl_cwchar_h_
