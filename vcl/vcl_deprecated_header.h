#ifndef vcl_deprecated_header_h_
#define vcl_deprecated_header_h_
/*
  fsm
*/

#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif

#include <vcl_compiler_detection.h>
#if defined(VXL_COMPILER_IS_GNU)
# warning "deprecated"

#elif defined(_MSC_VER)
// This warning is issued if your source file includes a deprecated
// header (e.g. vcl_strstream.h)  It can't figure out exactly where
// the include came from, so you'll have to see which .cxx file
// produced the warning, and then remove includes till it goes away....
#pragma message( "" __FILE__ "(13):warning(from VXL): an unknown deprecated header has been included." )

#else
// # pragma warning deprecated
#endif

#endif // vcl_deprecated_header_h_
