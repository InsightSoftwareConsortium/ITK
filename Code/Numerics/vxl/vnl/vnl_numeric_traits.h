#ifndef vnl_numeric_traits_h_
#define vnl_numeric_traits_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_numeric_traits.h

//: \file
//  \brief Templated zero/one/precision
//  \author Andrew W. Fitzgibbon, Oxford RRG, 04 Sep 96
//  To allow templated numerical algorithms to determine appropriate
//    values for zero, one, maxval, and types for double precision,
//    maximum product etc.

//     Modifications:
//     980212 AWF Initial version.
//     AWF 010498 Moved to math
//     LSB (Manchester) 23/3/01 Documentation tidied 
//
//-----------------------------------------------------------------------------

#include <vcl_complex_fwd.h>

// this is an empty class template.
// only the specializations make sense.
template <class T>
class vnl_numeric_traits;
////: Additive identity
//  static const T zero;
//
////: Multiplicative identity
//  static const T one;
//
////: Return value of abs()
//  typedef T abs_t;
//
////: Name of a type twice as long as this one for accumulators and products.
//  typedef /* long */ double double_t;
//
////: Name of type which results from multiplying this type with a double
//  typedef double real_t;

#if 0 // if anyone uses these, he or she should be shot -- fsm.
#if defined(i386)
// 16 bit int
typedef short long_char;
typedef long int long_short;
typedef long int long_int;
typedef unsigned short long_uchar;
typedef unsigned long int long_ushort;
typedef unsigned long long long_uint;
#else
// 32 bit
typedef short long_char;
typedef int long_short;
typedef long long_int;
typedef unsigned short long_uchar;
typedef unsigned int long_ushort;
typedef unsigned long long_uint;
#endif
#endif

#ifndef NO_STD_BOOL
VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<bool> {
public:
  static const bool zero VCL_STATIC_CONST_INIT_INT(0);
  static const bool one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned int abs_t;
  typedef unsigned int double_t;
  typedef double real_t;
};
#endif

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned char> {
public:
  static const unsigned char zero VCL_STATIC_CONST_INIT_INT(0);
  static const unsigned char one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned char abs_t;
  typedef unsigned short double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<signed char> {
public:
  static const signed char zero VCL_STATIC_CONST_INIT_INT(0);
  static const signed char one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned char abs_t;
  typedef signed short double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned short> {
public:
  static const unsigned short zero VCL_STATIC_CONST_INIT_INT(0);
  static const unsigned short one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned short abs_t;
  typedef unsigned int double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<signed short> {
public:
  static const signed short zero VCL_STATIC_CONST_INIT_INT(0);
  static const signed short one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned short abs_t;
  typedef signed int double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned int> {
public:
  static const unsigned int zero VCL_STATIC_CONST_INIT_INT(0);
  static const unsigned int one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned int abs_t;
  typedef unsigned int double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<signed int> {
public:
  static const signed int zero VCL_STATIC_CONST_INIT_INT(0);
  static const signed int one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned int abs_t;
  typedef signed int double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<signed long> {
public:
  static const signed long zero VCL_STATIC_CONST_INIT_INT(0);
  static const signed long one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned long abs_t;
  typedef signed long double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<unsigned long> {
public:
  static const unsigned long zero VCL_STATIC_CONST_INIT_INT(0);
  static const unsigned long one VCL_STATIC_CONST_INIT_INT(1);
  typedef unsigned long abs_t;
  typedef unsigned long double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<float> {
public:
  static const float zero VCL_STATIC_CONST_INIT_FLOAT(0.0F);
  static const float one VCL_STATIC_CONST_INIT_FLOAT(1.0F);
  typedef float abs_t;
  typedef double double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<double> {
public:
  static const double zero VCL_STATIC_CONST_INIT_FLOAT(0.0);
  static const double one VCL_STATIC_CONST_INIT_FLOAT(1.0);
  typedef double abs_t;
  typedef long double double_t;
  typedef double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits<long double> {
public:
  static const long double zero VCL_STATIC_CONST_INIT_FLOAT(0.0);
  static const long double one VCL_STATIC_CONST_INIT_FLOAT(1.0);
  typedef long double abs_t;
  typedef long double double_t; // ahem
  typedef long double real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits< vcl_complex<float> > {
public:
  static const vcl_complex<float> zero;
  static const vcl_complex<float> one;
  typedef float abs_t;
  typedef vcl_complex<vnl_numeric_traits<float>::double_t> double_t;
  typedef vcl_complex<float> real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits< vcl_complex<double> > {
public:
  static const vcl_complex<double> zero;
  static const vcl_complex<double> one;
  typedef double abs_t;
  typedef vcl_complex<vnl_numeric_traits<double>::double_t> double_t;
  typedef vcl_complex<double> real_t;
};

VCL_DEFINE_SPECIALIZATION
class vnl_numeric_traits< vcl_complex<long double> > {
public:
  static const vcl_complex<long double> zero;
  static const vcl_complex<long double> one;
  typedef long double abs_t;
  typedef vcl_complex<vnl_numeric_traits<long double>::double_t> double_t;
  typedef vcl_complex<long double> real_t;
};

#endif // vnl_numeric_traits_h_
