#ifndef vcl_gcc_295_complex_h_
#define vcl_gcc_295_complex_h_

// The GCC 2.95.3 complex does not have the value_type typedef,
// so we inherit and provide the typedef. vcl_complex is automatically
// convertable to and from std::complex, so everything else should
// work transparently. -- Amitha Perera

#include <complex.h>

template<class T>
class vcl_complex : public complex<T>
{
public:
  typedef T value_type;
  vcl_complex(T r = 0, T i = 0) : complex<T>(r,i) { }
  vcl_complex( complex<T> const& o ) : complex<T>(o) { }
};

#define vcl_complex vcl_complex

#define vcl_generic_complex_STD std
#include "../generic/vcl_complex.h"

#endif // vcl_gcc_295_complex_h_
