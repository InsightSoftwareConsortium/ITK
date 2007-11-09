#ifndef vcl_strstream_h_
#define vcl_strstream_h_
/*
  fsm
*/

// <strstream> is deprecated in favour of <sstream>, i.e. it is normative
// for the 1998 version of the C++ standard, but is not in the 2002 version.
// E.g., gcc 3.1 issues a warning when using it.
#include "vcl_deprecated_header.h"

// this is to get the vcl_ios_* macros.
#include "vcl_iostream.h"

// include compiler header.
#if defined(VCL_SGI_CC_720)
# include <strstream.h>
# define vcl_generic_strstream_STD /* */
# include "generic/vcl_strstream.h"

#else // -------------------- ISO
# include "iso/vcl_strstream.h"
#endif

#endif // vcl_strstream_h_
