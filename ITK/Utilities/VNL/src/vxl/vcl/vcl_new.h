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

// -------------------- old MSVC

#elif defined(VCL_VC_60)
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

#define vcl_bad_alloc std::bad_alloc

#ifdef VCL_VC_60
// Provide dummy set new handler
// It should be possible to get set_new_handler to work eith VC6 - 
// but I don't have a working VC6 to test/debug it - IMS
typedef void (__cdecl *new_handler)();
inline new_handler __cdecl vcl_set_new_handler(new_handler) {return 0;}
#else
# define vcl_set_new_handler std::set_new_handler
#endif

#endif // vcl_new_h_
