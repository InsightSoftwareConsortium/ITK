#ifndef vnl_complex_traits_h_
#define vnl_complex_traits_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_complex_traits
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_complex_traits.h
// .FILE	vnl_complex_traits.cxx
//
// .SECTION Description
//    To allow templated real or complex algorithms to determine appropriate
//    actions of conjugation, complexification etc.
//
// .SECTION Author
//     F. Schaffalitzky, Oxford RRG, 26 Mar 1999
//
// .SECTION Modifications:
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_complex.h>

// default is real.
template <class T>
class vnl_complex_traits {
public:
  // -- whether complex or not
  enum { isreal = true };

  // -- complex conjugation
  static T conjugate(T x) { return x; }

  // -- complexification
  static vnl_complex<T> complexify(T x) { return vnl_complex<T>(x,T(0)); }
};

// -- override for vnl_complex<REAL> :

// vnl_complex<float>
VCL_DEFINE_SPECIALIZATION
class vnl_complex_traits< vnl_complex<float> > {
public:
  enum { isreal = false };
  static vnl_complex<float> conjugate(vnl_complex<float> z)
    { return vnl_complex<float>(z.real(), -z.imag()); }
  static vnl_complex<float> complexify(vnl_complex<float> z)
    { return z; }
};

// vnl_complex<double>
VCL_DEFINE_SPECIALIZATION
class vnl_complex_traits< vnl_complex<double> > {
public:
  enum { isreal = false };
  static vnl_complex<double> conjugate(vnl_complex<double> z)
    { return vnl_complex<double>(z.real(), -z.imag()); }
  static vnl_complex<double> complexify(vnl_complex<double> z)
    { return z; }
};

#endif // vnl_complex_traits_h_
