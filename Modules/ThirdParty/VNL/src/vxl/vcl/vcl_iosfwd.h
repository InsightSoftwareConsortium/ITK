#ifndef vcl_iosfwd_h_
#define vcl_iosfwd_h_

#include <iosfwd>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#if VXL_LEGACY_FUTURE_REMOVE
  # error  "Deprecated.  Use equivalent C++11 header instead. see: vxl/scripts/UseStandardHeaders.py"
#else
  # warning "This header will be removed in future versions of VXL.  Use equivalent C++11 header instead. see: vxl/scripts/UseStandardHeaders.py"
#endif
#endif // vcl_iosfwd_h_
