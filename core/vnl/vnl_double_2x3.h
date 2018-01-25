// This is core/vnl/vnl_double_2x3.h
#ifndef vnl_double_2x3_h_
#define vnl_double_2x3_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief 2x3 matrix of double
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Dec 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, 25 June 1999: no need to use #pragma instantiate anymore
//   Peter Vanroose, 21 Oct 1999: vnl_matrix_fixed<double,2,3> already instantiated
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_double_3.h>
#include "vnl/vnl_export.h"

class VNL_EXPORT vnl_double_2x3 : public vnl_matrix_fixed<double, 2, 3>
{
 private:
  typedef vnl_matrix_fixed<double, 2, 3> Base;
 public:

  vnl_double_2x3() {}
  vnl_double_2x3(const vnl_double_3& row1, const vnl_double_3& row2)
  {
    vnl_double_2x3& M = *this;
    M(0,0) = row1[0];    M(0,1) = row1[1];    M(0,2) = row1[2];
    M(1,0) = row2[0];    M(1,1) = row2[1];    M(1,2) = row2[2];
  }

  vnl_double_2x3(double r00, double r01, double r02,
                 double r10, double r11, double r12)
  {
    vnl_double_2x3& M = *this;
    M(0,0) = r00;    M(0,1) = r01;    M(0,2) = r02;
    M(1,0) = r10;    M(1,1) = r11;    M(1,2) = r12;
  }

  vnl_double_2x3(Base const& M) : Base(M) { }
};

#endif // vnl_double_2x3_h_
