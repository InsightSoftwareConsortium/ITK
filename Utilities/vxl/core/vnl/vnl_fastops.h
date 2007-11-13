// This is core/vnl/vnl_fastops.h
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
//  Modifications
//   Feb.2002 -Peter Vanroose- brief doxygen comment placed on single line
//   Jun.2004 -Peter Vanroose- Added inc_X_by_ABt dec_X_by_AtB {inc,dec}_X_by_AB
//   Jun.2004 -Peter Vanroose- First step to migrate towards non-pointer args
//   Mar.2007 -Peter Vanroose- Commented deprecated versions of the functions
// \endverbatim

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//: Collection of C-style matrix functions for the most time-critical applications.
// In general, however one should consider using the vnl_transpose envelope-letter
// class to achieve the same results with about a 10% speed penalty.
class vnl_fastops
{
 public:
  static void AtA(vnl_matrix<double>& out, const vnl_matrix<double>& A);
  static void AB (vnl_matrix<double>& out, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void AtB(vnl_matrix<double>& out, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void AtB(vnl_vector<double>& out, const vnl_matrix<double>& A, const vnl_vector<double>& b);
  static void Ab (vnl_vector<double>& out, const vnl_matrix<double>& A, const vnl_vector<double>& b);
  static void ABt(vnl_matrix<double>& out, const vnl_matrix<double>& A, const vnl_matrix<double>& B);

  static double btAb (const vnl_matrix<double>& A, const vnl_vector<double>& b);

  static void ABAt(vnl_matrix<double>& out, const vnl_matrix<double>& A, const vnl_matrix<double>& B);

  static void inc_X_by_AtA(vnl_matrix<double>& X, const vnl_matrix<double>& A);
  static void inc_X_by_AB (vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void inc_X_by_AtB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void inc_X_by_AtB(vnl_vector<double>& X, const vnl_matrix<double>& A, const vnl_vector<double>& b);
  static void inc_X_by_ABt(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void inc_X_by_ABAt(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);

  static void dec_X_by_AtA(vnl_matrix<double>& X, const vnl_matrix<double>& A);
  static void dec_X_by_AB (vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void dec_X_by_AtB(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);
  static void dec_X_by_AtB(vnl_vector<double>& X, const vnl_matrix<double>& A, const vnl_vector<double>& b);
  static void dec_X_by_ABt(vnl_matrix<double>& X, const vnl_matrix<double>& A, const vnl_matrix<double>& B);

#if 0 // deprecated; use the ref-style versions instead!
  static void AtA(const vnl_matrix<double>& A, vnl_matrix<double>* out) {
    VXL_DEPRECATED("vnl_fastops::AtA"); AtA(*out, A); }
  static void AB (const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out) {
    VXL_DEPRECATED("vnl_fastops::AA"); AB(*out, A,B); }
  static void AtB(const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out) {
    VXL_DEPRECATED("vnl_fastops::AtB"); AtB(*out, A,B); }
  static void AtB(const vnl_matrix<double>& A, const vnl_vector<double>& b, vnl_vector<double>* out) {
    VXL_DEPRECATED("vnl_fastops::AtB"); AtB(*out, A,b); }
  static void ABt(const vnl_matrix<double>& A, const vnl_matrix<double>& B, vnl_matrix<double>* out) {
    VXL_DEPRECATED("vnl_fastops::ABt"); ABt(*out, A,B); }
#endif // 0

 private:
  // BLAS-like operations
  static double dot(const double* a, const double* b, unsigned int n);
};

#endif // vnl_fastops_h_
