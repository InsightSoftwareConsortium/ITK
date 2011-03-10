#ifndef vcl_borland56_complex_h_
#define vcl_borland56_complex_h_

#include <complex>

#ifdef vcl_generic_complex_STD
  ** error **
#else
# define vcl_generic_complex_STD std
#endif

// Borland C++ 5.6 switches between Rogue Wave and stlport based on
// the _USE_OLD_RW_STL macro.  A bug in the compiler prevents the
// stlport definitions of the complex<T> overloads of standard math
// functions from being included in the std namespace by a
// "using namespace _STL" present in the included headers.
// We add them explicitly.  - Brad King
#if !defined(_USE_OLD_RW_STL)
namespace std
{
  using _STL::abs;
  using _STL::arg;
  using _STL::polar;
  using _STL::sqrt;
  using _STL::exp;
  using _STL::log;
  using _STL::log10;
  using _STL::pow;
  using _STL::sin;
  using _STL::cos;
  using _STL::tan;
  using _STL::sinh;
  using _STL::cosh;
  using _STL::tanh;
}
#endif

#include "../generic/vcl_complex.h"

#endif // vcl_borland56_complex_h_
