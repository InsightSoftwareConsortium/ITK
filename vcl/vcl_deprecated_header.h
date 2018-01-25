#ifndef vcl_deprecated_header_h_
#define vcl_deprecated_header_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if defined(VCL_GCC)
# warning "deprecated"

#elif defined(VCL_VC)
// This warning is issued if your source file includes a deprecated
// header (e.g. vcl_strstream.h)  It can't figure out exactly where
// the include came from, so you'll have to see which .cxx file
// produced the warning, and then remove includes till it goes away....
#pragma message( "" __FILE__ "(13):warning(from VXL): an unknown deprecated header has been included." )

#else
// # pragma warning deprecated
#endif

#endif // vcl_deprecated_header_h_
