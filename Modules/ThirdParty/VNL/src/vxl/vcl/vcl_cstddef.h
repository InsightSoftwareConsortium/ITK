#ifndef vcl_cstddef_h_
#define vcl_cstddef_h_
/*
  Peter Vanroose, esat.kuleuven.be
*/

#include "vcl_compiler.h"

/* This should define: sizeof(), size_t, and ptrdiff_t */
#if !VCL_CXX_HAS_HEADER_CSTDDEF
# include <stddef.h>
#elif defined(VCL_GCC) && !defined(GNU_LIBSTDCXX_V3)
# include <stddef.h>
#elif defined(VCL_VC60)
# include <cstddef>
#elif defined(VCL_METRO_WERKS)
# include <cstddef>
#else
# undef __size_t // this is a bug fix for HPPA
# include "iso/vcl_cstddef.h"
using std::size_t;
using std::ptrdiff_t;
#endif

#ifndef vcl_size_t
#define vcl_size_t size_t
#endif

#ifndef vcl_ptrdiff_t
#define vcl_ptrdiff_t ptrdiff_t
#endif

#endif // vcl_cstddef_h_
