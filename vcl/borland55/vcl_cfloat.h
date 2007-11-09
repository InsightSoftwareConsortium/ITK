#ifndef vcl_borland55_cfloat_h_
#define vcl_borland55_cfloat_h_

// Borland C++ 5.6 defines macros like FLT_MAX to point at symbols,
// but does not add the std:: namespace in the macro when it decides
// to put the symbols in std (when the header is included as
// <cfloat>).  We just include it as <float.h> instead.

#include <float.h>

#endif // vcl_borland55_cfloat_h_
