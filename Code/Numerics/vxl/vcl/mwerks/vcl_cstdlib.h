#ifndef vcl_mwerks_cstdlib_h_
#define vcl_mwerks_cstdlib_h_

#include <cstdlib>
#define vcl_generic_cstdlib_STD std
#include "generic/vcl_cstdlib.h"

// the following functions are declared in both <cmath> and <cstdlib>
//#undef  vcl_abs
//#define vcl_abs vcl_abs
//inline int vcl_abs(int x) { return ::fabsl(x); }
//inline long vcl_abs(long x) { return ::fabsl(x); }

#warning HEI
#undef  vcl_abort
#define vcl_abort ::abort
#undef  vcl_malloc
#define vcl_malloc ::malloc
#undef  vcl_realloc
#define vcl_realloc ::realloc

#endif
