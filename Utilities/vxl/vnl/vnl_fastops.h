// This is vxl/vnl/vnl_fastops.h
#ifndef vnl_fastops_h_
#define vnl_fastops_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
//  \file
//  \brief Collection of C-style matrix functions
//  \author Andrew W. Fitzgibbon, Oxford RRG
//  \date   09 Dec 96
//
// \verbatim
//   Modifications
//   LSB (Manchester) 23/3/01 Tidied documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: Collection of C-style matrix functions for the most time-critical applications.
// In general, however one should consider using the vnl_transpose envelope-letter
// class to achieve the same results with about a 10% speed penalty.
class vnl_fastops
{
 public:
  static void AtA(const vnl_matrix<double>& A, vnl_matrix<double>* out);

  static void AB(const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out);
  static void AtB(const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out);
  static void AtB(const vnl_matrix<double>& A, const vnl_vector<double>& B, vnl_vector<double>* out);
  static void ABt(const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out);

  static void inc_X_by_AtB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void inc_X_by_AtB(vnl_vector<double>& X, const vnl_matrix<double>& A, const vnl_vector<double>& B);
  static void inc_X_by_AtA(vnl_matrix<double>& X, const vnl_matrix<double>& A);

  static void dec_X_by_AtB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void dec_X_by_AtA(vnl_matrix<double>& X, const vnl_matrix<double>& A);

  static void dec_X_by_ABt(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);

  // BLAS-like operations
  static double dot(const double* a, const double* b, int n);
};

#endif // vnl_fastops_h_
