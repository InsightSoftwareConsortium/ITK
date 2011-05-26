#ifndef vcl_valarray_h_
#define vcl_valarray_h_
/*
  fsm
*/

#include "vcl_compiler.h"

#if defined(VCL_GCC_295)
# include <valarray> // 2.95
# define vcl_valarray valarray

#elif defined(VCL_VC_6)
# include "win32-vc60/vcl_valarray.h"

#elif defined(VCL_VC_70)
# include "win32-vc70/vcl_valarray.h"

#elif defined(VCL_VC_8) || defined(VCL_VC_9)
# include "win32-vc8/vcl_valarray.h"

#else
# include "iso/vcl_valarray.h"
#endif

#if !VCL_COMPLEX_POW_WORKS
// deal with corrections to pow(complex...)
# undef vcl_pow
# define vcl_pow vcl_pow
template<class T> inline vcl_valarray<T>
  vcl_pow(const vcl_valarray<T>& x, const vcl_valarray<T>& y)
{ return std::pow(x, y); }

template<class T> inline vcl_valarray<T>
  vcl_pow(const vcl_valarray<T>& x, const T& y)
{ return std::pow(x, y); }

template<class T> inline vcl_valarray<T>
  vcl_pow(const T& x, const vcl_valarray<T>& y)
{ return std::pow(x, y); }

#endif

#endif // vcl_valarray_h_
