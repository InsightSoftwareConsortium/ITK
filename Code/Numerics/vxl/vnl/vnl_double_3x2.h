#ifndef vnl_double_3x2_h_
#define vnl_double_3x2_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_double_3x2.h

//: \file
// \brief
// \author Andrew W. Fitzgibbon, Oxford RRG, 23 Dec 96

// Modifications:
//   Peter Vanroose, 25 June 1999: no need to use #pragma instantiate anymore
//   Peter Vanroose, 21 Oct 1999: vnl_matrix_fixed<double,2,3> already instantiated
//   4/4/01 LSB (Manchester) Tidied documentation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_double_3.h>

class vnl_double_3x2 : public vnl_matrix_fixed<double, 3, 2> {
  typedef vnl_matrix_fixed<double, 3, 2> Base;
public:

  vnl_double_3x2() {}

  vnl_double_3x2(double r00, double r01,
                 double r10, double r11,
                 double r20, double r21) {
    vnl_matrix<double>& M = *this;
    M(0,0) = r00;    M(0,1) = r01;
    M(1,0) = r10;    M(1,1) = r11;
    M(2,0) = r20;    M(2,1) = r21;
  }
};

#endif // vnl_double_3x2_h_
