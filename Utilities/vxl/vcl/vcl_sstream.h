#ifndef vcl_sstream_h_
#define vcl_sstream_h_
// fsm
#include "vcl_compiler.h"

#if VCL_CXX_HAS_HEADER_SSTREAM
# include "iso/vcl_sstream.h"
#else // assuming <strstream.h> exists
# include "vcl_iostream.h" // this is to get the vcl_ios_* macros.
# include <strstream.h>
# include <vcl_string.h>

# undef  vcl_stringstream
# define vcl_stringstream vcl_stringstream
struct vcl_stringstream : public strstream
{
  vcl_stringstream(int /*which*/ = vcl_ios_in | vcl_ios_out) { }
  vcl_stringstream(vcl_string const &s, int which = vcl_ios_in | vcl_ios_out)
    : strstream((char*)s.c_str(), (int)strlen(s.c_str()), which) { }

  // [27.7.4]
  vcl_string str() { return strstream::str(); }
  //FIXME void str(vcl_string const &s);
};

# undef  vcl_istringstream
# define vcl_istringstream vcl_istringstream
struct vcl_istringstream : public istrstream
{
  vcl_istringstream(vcl_string const &s) : istrstream(s.c_str()) { }

  // [27.7.2.2]
  vcl_string str() { return istrstream::rdbuf()->str(); }
  //FIXME void str(vcl_string const &s);
};

# undef  vcl_ostringstream
# define vcl_ostringstream vcl_ostringstream
struct vcl_ostringstream : public ostrstream
{
  // [27.7.3.2]
  vcl_string str() { return ostrstream::str(); }
  //FIXME void str(vcl_string const &s);
};

#endif // VCL_CXX_HAS_HEADER_SSTREAM

#endif // vcl_sstream_h_
