#ifndef vcl_sgi_complex_h_
#define vcl_sgi_complex_h_

# include "iso/vcl_complex.h"
# undef  vcl_abs
# define vcl_abs vcl_abs
template <class T>
inline T vcl_abs(std::complex<T> const &z) { return std::abs(z); }

# undef vcl_log10
# define vcl_log10 vcl_log10
template <class T>
inline T vcl_log10(std::complex<T> const &z) { return ::log10(z); }

# undef vcl_exp
# define vcl_exp vcl_exp
template <class T>
inline std::complex<T> vcl_exp(std::complex<T> const& z) { return vcl_polar(T(exp(z.real())), z.imag()); }

# undef vcl_pow
# define vcl_pow vcl_pow
template <class T>
inline std::complex<T> vcl_pow(std::complex<T> const& z, T p) { return vcl_polar(T(pow(std::norm(z),p/2)),std::arg(z)*p); }
// Note that I'm using norm(z)^(p/2) instead of abs(z)^p, to have only one time-consuming operation - PVr

template <class T>
inline std::complex<T> vcl_pow(std::complex<T> const& z, std::complex<T> const& p) {
  T x=std::norm(z), y=std::arg(z);
  return vcl_polar(T(exp(-p.imag()*y)*pow(x,p.real()/2)),T(p.real()*y+p.imag()*log(x)/2)); }

# undef vcl_sqrt
# define vcl_sqrt vcl_sqrt
template <class T>
inline std::complex<T> vcl_sqrt(std::complex<T> const &z) {
  // was: return vcl_exp(vcl_log(z)/T(2));
  // NOW implemented without using log() or exp() which are
  // not necessarily available with type T - PVr, May 2002.
  T c = vcl_abs(z);
  return std::complex<T>(::sqrt((c+z.real())/T(2)), ::sqrt((c-z.real())/T(2)));
}

# undef vcl_cos
# define vcl_cos vcl_cos
template <class T>
inline std::complex<T> vcl_cos(std::complex<T> const &z)
{ return std::complex<T>(vcl_cos(z.real())*vcl_cosh(z.imag()),-vcl_sin(z.real())*vcl_sinh(z.imag())); }

# undef vcl_sin
# define vcl_sin vcl_sin
template <class T>
inline std::complex<T> vcl_sin(std::complex<T> const &z)
{ return std::complex<T>(vcl_sin(z.real())*vcl_cosh(z.imag()),vcl_cos(z.real())*vcl_sinh(z.imag())); }

#endif // vcl_sgi_complex_h_
