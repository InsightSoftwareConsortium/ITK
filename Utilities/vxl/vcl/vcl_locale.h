// This is vcl/vcl_locale.h
#ifndef vcl_locale_h_
#define vcl_locale_h_
//:
// \file
// \author fsm

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_LOCALE
# include <locale.h>
#else
# include <iso/vcl_locale.h>
#endif

#endif // vcl_locale_h_
