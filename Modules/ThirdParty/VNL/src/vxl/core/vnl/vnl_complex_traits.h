// This is core/vnl/vnl_complex_traits.h
#ifndef vnl_complex_traits_h_
#define vnl_complex_traits_h_
//:
// \file
// \brief To allow templated algorithms to determine appropriate actions of conjugation, complexification etc.
// \author fsm, Oxford RRG, 26 Mar 1999
//
// \verbatim
//  Modifications
//   LSB (Manchester) 26/3/01 Documentation tidied
// \endverbatim
//-----------------------------------------------------------------------------

#include <complex>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl/vnl_export.h"

template <class T> // the primary template is empty, by design.
struct vnl_complex_traits;

#define VCL_DEFINE_SPECIALIZATION_MACRO(T) \
template <> struct VNL_EXPORT vnl_complex_traits<T > \
{ \
  enum { isreal = true }; \
  static T conjugate(T x) { return x; } \
  static std::complex<T> complexify(T x) { return std::complex<T >(x, (T)0); } \
}
#define VCL_DEFINE_SPECIALIZATION_MACRO_SIGNED_UNSIGNED(T) \
VCL_DEFINE_SPECIALIZATION_MACRO(signed T); \
VCL_DEFINE_SPECIALIZATION_MACRO(unsigned T)
VCL_DEFINE_SPECIALIZATION_MACRO_SIGNED_UNSIGNED(char);
VCL_DEFINE_SPECIALIZATION_MACRO_SIGNED_UNSIGNED(short);
VCL_DEFINE_SPECIALIZATION_MACRO_SIGNED_UNSIGNED(int);
VCL_DEFINE_SPECIALIZATION_MACRO_SIGNED_UNSIGNED(long);
//long long - target type will have width of at least 64 bits. (since C++11)
VCL_DEFINE_SPECIALIZATION_MACRO_SIGNED_UNSIGNED(long long);

//3.9.1 Fundamental types [basic.fundamental]
//Plain char, signed char, and unsigned char are three distinct types
// We must explicitly instantiate the char type without signed/unsigned prefix
VCL_DEFINE_SPECIALIZATION_MACRO(char);
#undef VCL_DEFINE_SPECIALIZATION_MACRO_SIGNED_UNSIGNED
#undef VCL_DEFINE_SPECIALIZATION_MACRO


template <> struct VNL_EXPORT vnl_complex_traits<float>
{
  enum { isreal = true };
  static float conjugate(float x) { return x; }
  static std::complex<float> complexify(float x) { return {x, 0.0f}; }
};

template <> struct VNL_EXPORT vnl_complex_traits<double>
{
  enum { isreal = true };
  static double conjugate(double x) { return x; }
  static std::complex<double> complexify(double x) { return {x, 0.0}; }
};

template <> struct VNL_EXPORT vnl_complex_traits<long double>
{
  enum { isreal = true };
  static long double conjugate(long double x) { return x; }
  static std::complex<long double> complexify(long double x) { return {x, 0.0}; }
};

template <> struct VNL_EXPORT vnl_complex_traits<std::complex<float> >
{
  enum { isreal = false };
  static std::complex<float> conjugate(std::complex<float> x) { return std::conj(x); }
  static std::complex<float> complexify(float x) { return x; }
};

template <> struct VNL_EXPORT vnl_complex_traits<std::complex<double> >
{
  enum { isreal = false };
  static std::complex<double> conjugate(std::complex<double> x) { return std::conj(x); }
  static std::complex<double> complexify(double x) { return x; }
};

template <> struct VNL_EXPORT vnl_complex_traits<std::complex<long double> >
{
  enum { isreal = false };
  static std::complex<long double> conjugate(std::complex<long double> x) { return std::conj(x); }
  static std::complex<long double> complexify(long double x) { return x; }
};

#include <vnl/vnl_bignum.h>

template <> struct VNL_EXPORT vnl_complex_traits<vnl_bignum>
{
  enum { isreal = true };
  static vnl_bignum conjugate(vnl_bignum x) { return x; }
  static std::complex<vnl_bignum> complexify(vnl_bignum x) { return std::complex<vnl_bignum>(x,vnl_bignum(0L)); }
};

template <> struct VNL_EXPORT vnl_complex_traits<std::complex<vnl_bignum> >
{
  enum { isreal = false };
  static std::complex<vnl_bignum> conjugate(std::complex<vnl_bignum> x) { return std::complex<vnl_bignum>(x.real(),-x.imag()); }
  static std::complex<vnl_bignum> complexify(std::complex<vnl_bignum> x) { return x; }
};

#include <vnl/vnl_rational.h>

template <> struct VNL_EXPORT vnl_complex_traits<vnl_rational>
{
  enum { isreal = true };
  static vnl_rational conjugate(vnl_rational x) { return x; }
  static std::complex<vnl_rational> complexify(vnl_rational x) { return std::complex<vnl_rational>(x, vnl_rational(0,1)); }
};

template <> struct VNL_EXPORT vnl_complex_traits<std::complex<vnl_rational> >
{
  enum { isreal = false };
  static std::complex<vnl_rational> conjugate(std::complex<vnl_rational> x) {return std::complex<vnl_rational>(x.real(),-x.imag());}
  static std::complex<vnl_rational> complexify(std::complex<vnl_rational> x) { return x; }
};

#endif // vnl_complex_traits_h_
