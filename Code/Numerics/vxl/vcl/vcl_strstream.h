#ifndef vcl_strstream_h_
#define vcl_strstream_h_
/*
  fsm@robots.ox.ac.uk
*/

// <strstream> is deprecated, i.e. it is normative for the
// current (1998) version of the standard, but may not be
// in future standards.
// But as it is used a lot in TargetJr, don't issue this warning - PVr
//#include "vcl_deprecated.h"

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
