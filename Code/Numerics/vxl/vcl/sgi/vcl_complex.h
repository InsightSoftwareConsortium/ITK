# include "iso/vcl_complex.h"
# undef  vcl_abs
# define vcl_abs vcl_abs
template <typename T>
inline T vcl_abs(std::complex<T> const &z) { return std::abs(z); }

