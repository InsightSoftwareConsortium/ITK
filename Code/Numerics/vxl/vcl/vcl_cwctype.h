#ifndef vcl_cwctype_h_
#define vcl_cwctype_h_
/*
  fsm@robots.ox.ac.uk
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CWCTYPE
# include <wctype.h>
#elif defined(hpux)
// PVr: HP does not have /usr/include/wctype.h
// fsm: but the compiler might supply <cwctype>?
// PVr: but that file probably just #includes wctype.h?
# include <wchar.h>
#else
# include "iso/vcl_cwctype.h"
#endif

#endif // vcl_cwctype_h_
