#ifndef vcl_cstdio_h_
#define vcl_cstdio_h_
/*
  fsm@robots.ox.ac.uk
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CSTDIO
# include <stdio.h>
# define vcl_generic_cstdio_STD /* */
# include "generic/vcl_cstdio.h"
#elif defined(VCL_SUNPRO_CC_50)
# include <iosfwd> // <cstdio> breaks <iosfwd>
# include "iso/vcl_cstdio.h"
#elif defined(VCL_VC)
# include "win32/vcl_cstdio.h"
#elif defined(VCL_METRO_WERKS)
# include <cstdio>
# define vcl_generic_cstdio_STD std
# include "generic/vcl_cstdio.h"
# undef  vcl_printf
# define vcl_printf ::printf
# undef  vcl_sprintf
# define vcl_sprintf ::sprintf
#else
# include "iso/vcl_cstdio.h"
#endif

#endif // vcl_cstdio_h_
