#include <vcl_iostream.h>
#include <vnl/vnl_rational.h>
#include <vcl_complex.hxx>

// this function will tickle implicit templates for
// some compilers and detect missing instances for others.
template <class T>
vcl_complex<T> vcl_complex_instances_ticker(T *)
{
  vcl_complex<T> z(1, 2);
  return vcl_conj(z);
}

template vcl_complex<vnl_rational> vcl_complex_instances_ticker(vnl_rational *);

// macro to implement an operator>>, for compilers that need it.
# define implement_rsh(T) \
vcl_istream &operator>>(vcl_istream &is, vcl_complex<T > &z) { \
  T r, i; \
  is >> r >> i; \
  z = vcl_complex<T >(r, i); \
  return is; \
}
