// This is core/vnl/vnl_int_matrix.h
#ifndef vnl_int_matrix_h_
#define vnl_int_matrix_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Specializes vnl_matrix for integers
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   27 Dec 96
//
// \verbatim
//  Modifications
//   LSB (Manchester) 23/3/01 Tidied documentation
// \endverbatim
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matrix.h>

//: Specializes vnl_matrix for integers, adding a vnl_matrix<double> ctor.
class vnl_int_matrix : public vnl_matrix<int>
{
  typedef vnl_matrix<int> Base;
 public:

  vnl_int_matrix() {}
  vnl_int_matrix(char const* filename);
  vnl_int_matrix(unsigned r, unsigned c): Base(r, c) {}
  vnl_int_matrix(unsigned r, unsigned c, int fillvalue): Base(r, c, fillvalue) {}
  vnl_int_matrix(const vnl_matrix<double>& d);
  vnl_int_matrix(const vnl_matrix<int>& d):Base(d) {}
  vnl_int_matrix& operator=(const vnl_matrix<int>& d) { return (vnl_int_matrix&)Base::operator=(d); }
};

#endif // vnl_int_matrix_h_
