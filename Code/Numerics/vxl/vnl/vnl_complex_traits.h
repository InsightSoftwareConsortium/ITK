#ifndef vnl_complex_traits_h_
#define vnl_complex_traits_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_complex_traits.h

//: \file
//  \brief To allow templated real or complex algorithms to determine appropriate actions of conjugation, complexification etc.
//  \author F. Schaffalitzky, Oxford RRG, 26 Mar 1999


//     
//
// Modifications:
// LSB (Manchester) 26/3/01 Documentation tidied 
//
//-----------------------------------------------------------------------------

#include <vcl_complex.h>
//: To allow templated real or complex algorithms to determine appropriate
//    actions of conjugation, complexification etc.
// Default is real.
template <class T>
class vnl_complex_traits {
public:
  //: Whether complex or not
  enum { isreal = true };

  //: Complex conjugation
  static T conjugate(T x) { return x; }

  //: Complexification
  static vcl_complex<T> complexify(T x) { return vcl_complex<T>(x,T(0)); }
};

//: override for vcl_complex<REAL> :

// vcl_complex<float>
VCL_DEFINE_SPECIALIZATION
class vnl_complex_traits< vcl_complex<float> > {
public:
  enum { isreal = false };
  static vcl_complex<float> conjugate(vcl_complex<float> z)
    { return vcl_complex<float>(z.real(), -z.imag()); }
  static vcl_complex<float> complexify(vcl_complex<float> z)
    { return z; }
};

// vcl_complex<double>
VCL_DEFINE_SPECIALIZATION
class vnl_complex_traits< vcl_complex<double> > {
public:
  enum { isreal = false };
  static vcl_complex<double> conjugate(vcl_complex<double> z)
    { return vcl_complex<double>(z.real(), -z.imag()); }
  static vcl_complex<double> complexify(vcl_complex<double> z)
    { return z; }
};

#endif // vnl_complex_traits_h_
