#include <vcl_iostream.h>
#include <vnl/vnl_rational.h>
#include <vcl_complex.txx>

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

// ---------- emulation
#if !VCL_USE_NATIVE_COMPLEX
// ** make sure gcc 2.7 sees this **
VCL_COMPLEX_INSTANTIATE(vnl_rational);

// ---------- egcs
# elif defined(VCL_EGCS)
# if !VCL_HAS_TEMPLATE_SYMBOLS
template vcl_ostream& operator<<(vcl_ostream &, vcl_complex<vnl_rational> const &);
template vcl_complex<vnl_rational> operator/ (vcl_complex<vnl_rational>const&,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator/ (vcl_complex<vnl_rational>const&,vnl_rational);
implement_rsh(vnl_rational);
#include <std/complext.cc>
template vcl_complex<vnl_rational>& __doadv<vnl_rational>(vcl_complex<vnl_rational>*, vcl_complex<vnl_rational> const&);
# endif

// ---------- gcc 2.95
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# if !VCL_HAS_TEMPLATE_SYMBOLS
# define VCL_COMPLEX_INSTANTIATE_INLINE(x) template x
template bool operator==(vcl_complex<vnl_rational>const&,vcl_complex<vnl_rational>const&);
template bool operator==(vnl_rational,vcl_complex<vnl_rational>const&);
template bool operator==(vcl_complex<vnl_rational>const&,vnl_rational);
template vnl_rational vcl_imag(vcl_complex<vnl_rational>const&);
template vnl_rational vcl_real(vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator+(vcl_complex<vnl_rational>const&,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator+(vcl_complex<vnl_rational>const&,vnl_rational);
template vcl_complex<vnl_rational> operator+(vnl_rational,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator-(vcl_complex<vnl_rational>const&,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator-(vcl_complex<vnl_rational>const&,vnl_rational);
template vcl_complex<vnl_rational> operator-(vnl_rational,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator*(vcl_complex<vnl_rational>const&,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator*(vcl_complex<vnl_rational>const&,vnl_rational);
template vcl_complex<vnl_rational> operator*(vnl_rational,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator/(vcl_complex<vnl_rational>const&,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> operator/(vcl_complex<vnl_rational>const&,vnl_rational);
template vcl_complex<vnl_rational> operator/(vnl_rational,vcl_complex<vnl_rational>const&);
template vcl_complex<vnl_rational> vcl_pow (vcl_complex<vnl_rational>const&,int);
template vcl_ostream& operator<<(vcl_ostream&, vcl_complex<vnl_rational>const&);
implement_rsh(vnl_rational);
#include <std/complext.cc>
template vcl_complex<vnl_rational>& __doadv<vnl_rational>(vcl_complex<vnl_rational>*, vcl_complex<vnl_rational> const&);
# endif

// ---------- sunpro
#elif defined(VCL_SUNPRO_CC)
template vcl_complex<vnl_rational> vcl_conj<vnl_rational>(vcl_complex<vnl_rational> const &);

// ---------- ISO
#else
// ISO compilers are magic as far as instantiation goes.
#endif
