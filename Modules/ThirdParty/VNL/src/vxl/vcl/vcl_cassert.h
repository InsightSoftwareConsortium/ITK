// This is vcl/vcl_cassert.h

//- The <cassert> header does not
//- have inclusion guards. The
//- C and C++ standards say so.

//:
// \file
// \author fsm

#include "vcl_compiler.h"

// This header file should do something like this:
//   #undef assert
//   #ifdef NDEBUG
//   # define assert(x) ((void) 0)
//   #else
//   extern void vcl_cassert_failure(char const *, int, char const *);
//   # define assert(x) do { if (!(x)) vcl_cassert_failure(__FILE__, __LINE__, #x); } while (false)
//   #endif
// If the system/compiler version works, use that instead.


// Only redefine the macro with MSVC 6.0
#ifdef VCL_VC_6
# undef assert
# ifdef NDEBUG
#  define assert(x) ((void) 0)
# else
   extern void vcl_cassert_failure(char const *, int, char const *);
#  define assert(x) do { if (!(x)) vcl_cassert_failure(__FILE__, __LINE__, #x); } while (false)
# endif 

#else   // For all other compilers, include the standard C/CXX header

# ifdef VCL_METRO_WERKS
// for some reason, MW's <cassert> doesn't have its own printf() and abort() declarations.
#  include <vcl_cstdio.h>
#  include <vcl_cstdlib.h>
# endif

# if !VCL_CXX_HAS_HEADER_CASSERT
#  include <assert.h>
# else
#  include "iso/vcl_cassert.h"
# endif
#endif

// fsm: There should not be a vcl_assert macro as there is no
// std::assert symbol. If your assert macro is broken, fix it
// here using #undef and #define.
