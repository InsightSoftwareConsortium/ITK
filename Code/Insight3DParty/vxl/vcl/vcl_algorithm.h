#ifndef vcl_algorithm_h_
#define vcl_algorithm_h_

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_algorithm.h"

// -------------------- iso
#else
# include "iso/vcl_algorithm.h"
#endif


#ifdef VCL_VC60
# undef  vcl_max
# define vcl_max vcl_max
# undef  vcl_min
# define vcl_min vcl_min
template <typename T>
inline T vcl_max(T const& a, T const& b)
{
  return (a > b) ? a : b;
}

template <typename T>
inline T vcl_min(T const& a, T const& b)
{
  return (a < b) ? a : b;
}
#endif

#endif // vcl_algorithm_h_
