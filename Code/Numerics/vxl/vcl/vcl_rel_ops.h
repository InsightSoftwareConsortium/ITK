#ifndef vcl_rel_ops_h_
#define vcl_rel_ops_h_
/*
  fsm@robots.ox.ac.uk
*/

// Purpose: To provide a portable way of getting inline
// function templates
//      operator!=      ... !(x == y)
//      operator>       ...  (y <  x)
//      operator<=      ... !(y <  x)
//      operator>=      ... !(x <  y)
//
// For ISO compilers, these live in namespace std::rel_ops.
//
// PLEASE DON'T USE THIS HEADER FILE. If you are using a class
// type, A, which provides operator== but not operator!= you
// can do
//   #include <utility>
//   using std::operator!=;
//
// before you do
//   if (x != y) { ...
//
// but if one reflects on it dispassionately, it should be clear
// that this solution is much more verbose than than the obvious
// one, which is to just write
//   if (!(x == y)) { ...
//
// instead. If you find you need operator!= a lot, it is better
// to provide an operator!= near the declaration of class A, to
// save the user the trouble of #including <utility> and typing
// a `using' statement.
//
// See also http://gcc.gnu.org/ml/libstdc++/2001-01/msg00247.html

#include "vcl_compiler.h"

#if !VCL_USE_NATIVE_STL
# include "emulation/vcl_rel_ops.h"

#elif defined(VCL_GCC) && !defined(GNU_LIBSTDCXX_V3)
// this header is wrong. v2 of the GNU library is wrong wrong wrong.
# include "vcl_functional.h"

#elif defined(VCL_SGI_CC)
# include "vcl_utility.h"
using std::operator!=;
using std::operator> ;
using std::operator<=;
using std::operator>=;

#else // -------------------- ISO
# include "vcl_utility.h"
using std::rel_ops::operator!=;
using std::rel_ops::operator> ;
using std::rel_ops::operator<=;
using std::rel_ops::operator>=;
#endif

// instantiation macro for compilers that need it.
#define VCL_REL_OPS_INSTANTIATE(T) \
VCL_INSTANTIATE_INLINE(bool operator!=(T const &, T const &)); \
VCL_INSTANTIATE_INLINE(bool operator> (T const &, T const &)); \
VCL_INSTANTIATE_INLINE(bool operator<=(T const &, T const &)); \
VCL_INSTANTIATE_INLINE(bool operator>=(T const &, T const &));

#endif // vcl_rel_ops_h_
