#ifndef vcl_algorithm_h_
#define vcl_algorithm_h_

#include "vcl_compiler.h"

#include "iso/vcl_algorithm.h"


#ifdef VCL_VC
# undef  vcl_max
# define vcl_max vcl_max
# undef  vcl_min
# define vcl_min vcl_min
template <class T>
inline T vcl_max(T const& a, T const& b)
{
  return (a > b) ? a : b;
}

template <class T>
inline T vcl_min(T const& a, T const& b)
{
  return (a < b) ? a : b;
}
#endif

#include "vcl_algorithm.hxx"

#endif // vcl_algorithm_h_
