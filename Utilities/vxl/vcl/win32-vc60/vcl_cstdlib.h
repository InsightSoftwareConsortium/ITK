#ifndef vcl_win32_vc60_cstdlib_h_
#define vcl_win32_vc60_cstdlib_h_

#include <cstdlib>

// VC6 does not declare the cstdlib functions in the std namespace.
// If we define vcl_abs, for example, to ::abs, we have conflicts
// with std::abs(std::complex<T>) which *is* declared in the
// std namespace. To avoid these issues, we inject the math
// functions into the std namespace. The many other functions
// in are left in the global namespace.

namespace std {
  typedef ::div_t div_t;
  typedef ::ldiv_t ldiv_t;
  
  inline int abs(int x) { return x >= 0 ? x : -x; }
  inline long abs(long x) { return x >= 0 ? x : -x; }
  
  inline long labs(long x) { return x >= 0 ? x : -x; }
 
  inline div_t div(int x, int y) { return ::div(x,y); }
  inline ldiv_t div(long x, long y) { return ::ldiv(x,y); }
  
  inline ldiv_t ldiv(long x, long y) { return ::ldiv(x,y); }
}

#ifndef vcl_abs
#define vcl_abs std::abs
#endif
#ifndef vcl_labs
#define vcl_labs std::labs
#endif
#ifndef vcl_div
#define vcl_div std::div
#endif
#ifndef vcl_ldiv
#define vcl_ldiv std::ldiv
#endif

#define vcl_generic_cstdlib_STD

#include "../generic/vcl_cstdlib.h"

#endif // vcl_win32_vc60_cstdlib_h_
