#ifndef vnl_matops_h_
#define vnl_matops_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_matops - A collection of Matrix operations
// .HEADER	vxl package
// .LIBRARY	vnl
// .INCLUDE	vnl/vnl_matops.h
// .FILE	vnl_matops.cxx
//
// .SECTION Description
//    matops is a collection of Matrix operations, mostly declared
//    as static methods.  Highlights include the in-place transpose,
//    and type conversions.
//    matlab_print has been moved to vnl_matlab_print.h.
//
// .SECTION Author
//    Andrew W. Fitzgibbon, Oxford RRG, 05 Aug 96
//
// .SECTION Modifications:
//    23 may 97, Peter Vanroose - "NO_COMPLEX" option added
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

class vnl_matops {
public:
  static double homg_diff(const vnl_matrix<double>& A, const vnl_matrix<double>& B);

  // Laminating
  static vnl_matrix<double> cat(const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static vnl_matrix<double> cat(const vnl_matrix<double>& A, const vnl_vector<double>& B);
  static vnl_matrix<double> cat(const vnl_vector<double>& A, const vnl_matrix<double>& B);

  static vnl_matrix<double> vcat(const vnl_matrix<double>& A, const vnl_matrix<double>& B);

  // Conversions
  static vnl_matrix<double> f2d(const vnl_matrix<float>&);
  static vnl_matrix<float>  d2f(const vnl_matrix<double>&);
  static vnl_vector<double> f2d(const vnl_vector<float>&);
  static vnl_vector<float>  d2f(const vnl_vector<double>&);
};

#endif // vnl_matops_h_
