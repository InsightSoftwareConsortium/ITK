

# include "iso/vcl_complex.h"
# undef  vcl_abs
# define vcl_abs vcl_abs
template <typename T>
inline T vcl_abs(std::complex<T> const &z) { return std::abs(z); }

# undef vcl_log10
# define vcl_log10 vcl_log10
template <typename T>
inline T vcl_log10(std::complex<T> const &z) { return ::log10(z); }
