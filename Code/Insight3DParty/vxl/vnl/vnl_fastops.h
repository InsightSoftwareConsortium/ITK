#ifndef vnl_fastops_h_
#define vnl_fastops_h_
#ifdef __GNUC__
#pragma interface
#endif
// .NAME	vnl_fastops
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_fastops.h
// .FILE	vnl_fastops.cxx
//
// .SECTION Description
//    vnl_fastops is a collection of C-style matrix functions for the most
//    time-critical applications.  In general, however one should consider
//    using the vnl_transpose envelope-letter class to achieve the same results
//    with about a 10% speed penalty.
//
// .SECTION Author
//    Andrew W. Fitzgibbon, Oxford RRG, 09 Dec 96

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

class vnl_fastops {
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
