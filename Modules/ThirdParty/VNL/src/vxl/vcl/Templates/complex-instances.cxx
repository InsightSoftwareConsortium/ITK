// This file is supposed to define any template instances needed
// to give a sensible complex type for float, double and long double.
//
// E.g. ensure that "operator/(complex<float>, float)" exists
//
// It is in Templates because it may need implicit templates to work properly
//
// Note to maintainers: the format of this file should be:
// #if compiler_1
//  ...
// #elif compiler_2
//  ...
// ........
//  ...
// #elif compiler_n
// ..
// #else // ISO
//
// #endif
//
// "Many sections style" is better than complex conditional logic.
//
// If you get problems with multiply defined symbols for static builds,
// try to avoid breaking the shared builds by removing instantiations
// it needs. With gcc, using #pragma weak may be an option.

#include <vcl_iostream.h>
#include <vcl_complex.hxx>

// this function will tickle implicit templates for
// some compilers and detect missing instances for others.
template <class T>
vcl_complex<T> vcl_complex_instances_ticker(T *)
{
  vcl_complex<T> z(1, 2);
  vcl_complex<T> x = vcl_arg(z);
  x += vcl_conj(z);
  x -= vcl_abs(z);
  x *= vcl_polar(T(3), T(4));
  x /= vcl_sqrt(z);
  return x + vcl_norm(z);
}
template vcl_complex<float > vcl_complex_instances_ticker(float  *);
template vcl_complex<double> vcl_complex_instances_ticker(double *);
template vcl_complex<long double> vcl_complex_instances_ticker(long double *);

// macro to implement an operator>>, for compilers that need it.
# define implement_rsh(T) \
vcl_istream &operator>>(vcl_istream &is, vcl_complex<T > &z) { \
  T r, i; \
  is >> r >> i; \
  z = vcl_complex<T >(r, i); \
  return is; \
}
