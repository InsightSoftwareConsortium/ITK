#ifndef vcl_cerrno_h_
#define vcl_cerrno_h_
/*
  fsm@robots.ox.ac.uk
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CERRNO
# include <errno.h>
#else
# include "iso/vcl_cerrno.h"
#endif

#endif // vcl_cerrno_h_
