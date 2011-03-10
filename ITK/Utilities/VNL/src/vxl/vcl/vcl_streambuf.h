#ifndef vcl_streambuf_h_
#define vcl_streambuf_h_
/*
  fsm
*/

// Purpose: to #define vcl_streambuf to std::streambuf on 
// conforming implementations and to fix the non-conforming 
// ones.

#include "vcl_iostream.h" // to get vcl_ios_*

//#ifdef VCL_WIN32
//# include <streambuf>
//# define vcl_streambuf std::streambuf

#if defined(VCL_SGI_CC_720)
# include <iostream.h>
# define vcl_generic_streambuf_STD /* */
# include "generic/vcl_streambuf.h"

#elif defined(VCL_GCC) && !defined(GNU_LIBSTDCXX_V3)
# include <streambuf.h>
# define vcl_generic_streambuf_STD /* */
# include "generic/vcl_streambuf.h"

#else
# include "iso/vcl_streambuf.h"
#endif

#endif // vcl_streambuf_h_
