#ifndef vcl_cerrno_h_
#define vcl_cerrno_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if !VCL_CXX_HAS_HEADER_CERRNO
# include <errno.h>
#else
# include "iso/vcl_cerrno.h"
#endif

#ifdef linux // bug fix: errno.h erroneously declares __errno_location() as C++
extern "C" inline int* __errno_location__Fv() { return __errno_location(); }
extern "C" inline int* _Z16__errno_locationv() { return __errno_location(); }
#endif

#endif // vcl_cerrno_h_
