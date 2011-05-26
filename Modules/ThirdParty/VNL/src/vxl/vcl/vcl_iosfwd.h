//-*- c++ -*-------------------------------------------------------------------
#ifndef vcl_iosfwd_h_
#define vcl_iosfwd_h_

// ANSI standard iostream forward decls.
// You can't write "class ostream" and expect it to work

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# if defined(VCL_WIN32)
#  include "iso/vcl_iosfwd.h"
# else
  class istream;
  class ostream;
  class ifstream;
  class ofstream;
# define vcl_generic_iosfwd_STD /* */
# include "generic/vcl_iosfwd.h"
# endif

#elif defined(VCL_SGI_CC_720)
# include <stream.h> // this #includes iostream.h, fstream.h and iomanip.h
# define vcl_generic_iosfwd_STD /* */
# include "generic/vcl_iosfwd.h"

#else // -------------------- iso
# include "iso/vcl_iosfwd.h"
#endif

#endif // vcl_iosfwd_h_
