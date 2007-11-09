#ifndef vcl_exception_h_
#define vcl_exception_h_

#include "vcl_compiler.h"

#if !defined(VCL_HAS_EXCEPTIONS)
#if defined(VCL_WIN32) || defined(__EXCEPTIONS) || _COMPILER_VERSION == 721
# define VCL_HAS_EXCEPTIONS 1
#else
# define VCL_HAS_EXCEPTIONS 0
#endif
#endif


// This is just syntax at the moment.  I can't figure out how to
// cover (...) and (char* e) yet...

#if VCL_HAS_EXCEPTIONS

# define vcl_throw     throw
# define vcl_try       try
# define vcl_catch     catch
# define vcl_catch_all catch (...)

# include "iso/vcl_exception.h"

#else
//

#include "vcl_cstdlib.h"
#include "vcl_iostream.h"
struct vcl_throw_handler {
  vcl_throw_handler(int line, char const* file) {
    vcl_cerr << file << ":" << line << ": EXCEPTION: ";
  }
  ~vcl_throw_handler() { vcl_abort (); }
  vcl_ostream& s() { return vcl_cerr; }
};

# define vcl_throw        vcl_throw_handler(__LINE__,__FILE__).s() << 
# define vcl_try          if (0/* setjmp(buf) */) { /* handle longjmp */ } else
# define vcl_catch(x)     for ( x = 0; false; )
# define vcl_catch_all    if (1) /* nothing */ ; else

#endif

#endif // vcl_exception_h_
