// This is core/vnl/vnl_matops.h
#ifndef vnl_matops_h_
#define vnl_matops_h_
//:
// \file
// \brief A collection of Matrix operations
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   05 Aug 96
//
// \verbatim
//  Modifications
//   23 may 97, Peter Vanroose - "NO_COMPLEX" option added
//   LSB (Manchester) 23/3/01 Documentation tidied
// \endverbatim
//
//-----------------------------------------------------------------------------

#include "vnl_vector.h"
#include "vnl_matrix.h"
#include "vnl/vnl_export.h"

//:   A collection of vnl_matrix operations, provided as static methods.
//    Highlights include matrix glueing, and type conversions.
//    matlab_print has been moved to vnl_matlab_print.h.
class VNL_EXPORT vnl_matops
{
public:
  static double
  homg_diff(const vnl_matrix<double> & A, const vnl_matrix<double> & B);

  //: Laminating
  static vnl_matrix<double>
  cat(const vnl_matrix<double> & A, const vnl_matrix<double> & B);
  static vnl_matrix<double>
  cat(const vnl_matrix<double> & A, const vnl_vector<double> & B);
  static vnl_matrix<double>
  cat(const vnl_vector<double> & A, const vnl_matrix<double> & B);

  static vnl_matrix<double>
  vcat(const vnl_matrix<double> & A, const vnl_matrix<double> & B);

  //: Conversions
  static vnl_matrix<double>
  f2d(const vnl_matrix<float> &);
  static vnl_vector<double>
  f2d(const vnl_vector<float> &);
  static vnl_matrix<float>
  d2f(const vnl_matrix<double> &);
  static vnl_vector<float>
  d2f(const vnl_vector<double> &);
};

#endif // vnl_matops_h_
