#ifndef vcl_strstream_h_
#define vcl_strstream_h_
/*
  fsm@robots.ox.ac.uk
*/

// <strstream> is deprecated, i.e. it is normative for the
// current (1998) version of the standard, but may not be
// in future standards.
#include "vcl_deprecated.h"

// this is to get the vcl_ios_* macros.
#include "vcl_iostream.h"

// include compiler header.
#if defined(VCL_SGI_CC_720)
# include <strstream.h>
# define vcl_generic_strstream_STD /* */
# include "generic/vcl_strstream.h"

/*
#elif defined(__GNUC__) && (__GNUC__ <= 2) && (__GNUC_MINOR__ >= 97)
# include <sstream>
# include "vcl_string.h"
struct vcl_istrstream : std::stringstream
{
  typedef std::stringstream base;
  vcl_istrstream(char const *s) : base(s) { }
  vcl_istrstream(char const *s, unsigned n) : base(vcl_string(s, s+n)) { }
};
struct vcl_ostrstream : std::stringstream
{
  typedef std::stringstream base;
  char const *str() { return base::str().c_str(); }
};
*/

#else // -------------------- ISO
# include "iso/vcl_strstream.h"
#endif

#endif // vcl_strstream_h_
