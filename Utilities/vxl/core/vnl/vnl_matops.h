// This is core/vnl/vnl_matops.h
#ifndef vnl_matops_h_
#define vnl_matops_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
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

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
//:   A collection of Matrix operations
//    mostly declared as static methods.
//    Highlights include matrix gluing, and type conversions.
//    matlab_print has been moved to vnl_matlab_print.h.
class vnl_matops
{
 public:
  static double homg_diff(vnl_matrix<double> const& A, vnl_matrix<double> const& B);

  //: Laminating
  static vnl_matrix<double> cat(vnl_matrix<double> const& A, vnl_matrix<double> const& B);
  static vnl_matrix<double> cat(vnl_matrix<double> const& A, vnl_vector<double> const& B);
  static vnl_matrix<double> cat(vnl_vector<double> const& A, vnl_matrix<double> const& B);

  static vnl_matrix<double> vcat(vnl_matrix<double> const& A, vnl_matrix<double> const& B);

  //: Conversions
  static vnl_matrix<double> f2d(vnl_matrix<float> const&);
  static vnl_vector<double> f2d(vnl_vector<float> const&);
  static vnl_matrix<float>  d2f(vnl_matrix<double> const&);
  static vnl_vector<float>  d2f(vnl_vector<double> const&);
};

#endif // vnl_matops_h_
