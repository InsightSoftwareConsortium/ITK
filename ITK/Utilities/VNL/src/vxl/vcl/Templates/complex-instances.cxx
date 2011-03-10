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
#include <vcl_complex.txx>

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


// ---------- emulation
#if !VCL_USE_NATIVE_COMPLEX
// ** make sure gcc 2.7 sees this **
VCL_COMPLEX_INSTANTIATE(float);
VCL_COMPLEX_INSTANTIATE(double);
VCL_COMPLEX_INSTANTIATE(long double);

// ---------- egcs
# elif defined(VCL_EGCS)
# if !VCL_HAS_TEMPLATE_SYMBOLS
# define do_inlines(FLOAT) \
template vcl_ostream& operator<<(vcl_ostream &, vcl_complex<FLOAT > const &); \
template vcl_complex<FLOAT > vcl_sqrt (vcl_complex<FLOAT >const& x); \
template vcl_complex<FLOAT > operator / (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&); \
template vcl_complex<FLOAT > operator / (vcl_complex<FLOAT >const&,FLOAT); \
implement_rsh(FLOAT)

do_inlines(float);
do_inlines(double);
do_inlines(long double);
# endif

// ---------- gcc 2.95
#elif defined(VCL_GCC_295) && !defined(GNU_LIBSTDCXX_V3)
# if !VCL_HAS_TEMPLATE_SYMBOLS
# define VCL_COMPLEX_INSTANTIATE_INLINE(x) template x
# define do_inlines(FLOAT) \
VCL_COMPLEX_INSTANTIATE_INLINE(bool operator==(vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(bool operator==(FLOAT,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(bool operator==(vcl_complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT vcl_imag(vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT vcl_real(vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_sqrt (vcl_complex<FLOAT >const& x));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator + (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator + (vcl_complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator + (FLOAT,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator - (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator - (vcl_complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator - (FLOAT,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator * (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator * (vcl_complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator * (FLOAT,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator / (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator / (vcl_complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > operator / (FLOAT,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_polar (FLOAT,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_pow (vcl_complex<FLOAT >const&,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_pow (vcl_complex<FLOAT >const&,FLOAT));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_pow (vcl_complex<FLOAT >const&,int));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_pow (FLOAT,vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_exp (vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT > vcl_log (vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT vcl_arg (vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT vcl_abs (vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(FLOAT vcl_norm (vcl_complex<FLOAT >const&));\
VCL_COMPLEX_INSTANTIATE_INLINE(vcl_complex<FLOAT>& __doadv (vcl_complex<FLOAT>* ths, const vcl_complex<FLOAT>& y));\
template vcl_ostream& operator<<(vcl_ostream &, vcl_complex<FLOAT > const &);\
implement_rsh(FLOAT)

do_inlines(float);
do_inlines(double);
do_inlines(long double);
# endif

// ---------- sunpro
#elif defined(VCL_SUNPRO_CC)
# define do_inlines(FLOAT) \
template vcl_complex<FLOAT > vcl_conj<FLOAT >(vcl_complex<FLOAT > const &)

do_inlines(float);
do_inlines(double);
do_inlines(long double);

// ---------- ISO
#else
// ISO compilers are magic as far as instantiation goes.
#endif
