#ifndef vcl_new_h_
#define vcl_new_h_
/*
  fsm
*/

#include "vcl_compiler.h"

// -------------------- emulation
#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_new.h"

// -------------------- old SGI CC
#elif defined(VCL_SGI_CC_720)
# include <new.h>

// -------------------- gcc with old library
#elif defined(VCL_GCC) && !defined(GNU_LIBSTDCXX_V3)
# include <new.h>

// -------------------- iso
#else
# include <new>
#endif

// Provide vcl_destroy() and vcl_construct() :
#if VCL_USE_NATIVE_STL
template <class T>
inline
void vcl_destroy(T *p) { p->~T(); }

template <class U, class V>
inline
void vcl_construct(U * p, V const & value) { new (p) U(value); }
#endif

#endif // vcl_new_h_
