#ifndef vcl_sstream_h_
#define vcl_sstream_h_
/*
  fsm@robots.ox.ac.uk
*/

// this is to get the vcl_ios_* macros.
#include "vcl_iostream.h"

#if (defined(VCL_GCC) && !defined(GNU_LIBSTDCXX_V3)) || defined(VCL_SGI_CC_720)
# include <strstream.h>

# undef  vcl_istringstream
# define vcl_istringstream vcl_istringstream
struct vcl_istringstream : istrstream
{
  vcl_istringstream(vcl_string const &s) : istrstream(s.c_str()) { }
};

# undef  vcl_ostringstream
# define vcl_ostringstream vcl_ostringstream
struct vcl_ostringstream : ostrstream
{
  vcl_string str() { return ostrstream::str(); }
};

#else
# include "iso/vcl_sstream.h"
#endif

#endif // vcl_sstream_h_
