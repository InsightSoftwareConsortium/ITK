#ifndef vcl_cwctype_h_
#define vcl_cwctype_h_
#ifdef __GNUC__
#pragma interface
#endif
/*
  fsm@robots.ox.ac.uk
*/

#include "vcl_compiler.h"

#ifdef VCL_SGI_CC_720
# include <wctype.h>
#elif defined(hpux)
// someone: HP does not have /usr/include/wctype.h
// fsm: but the compiler might supply <cwctype>?
# include <wchar.h>
#else
# include "iso/vcl_cwctype.h"
#endif

#endif // vcl_cwctype_h_
