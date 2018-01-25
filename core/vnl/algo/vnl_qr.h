// This is core/vnl/algo/vnl_qr.h
#ifndef vnl_qr_h_
#define vnl_qr_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Calculate inverse of a matrix using QR
// \author  Andrew W. Fitzgibbon, Oxford RRG
// \date   08 Dec 1996
//
// \verbatim
//  Modifications
//   081296 AWF Temporarily abandoned as I realized my problem was symmetric...
//   080697 AWF Recovered, implemented solve().
//   200897 AWF Added determinant().
//   071097 AWF Added Q(), R().
//   Christian Stoecklin, ETH Zurich, added QtB(v)
//   31-mar-2000 fsm: templated
//   28/03/2001 - dac (Manchester) - tidied up documentation
//   13 Jan.2003 - Peter Vanroose - added missing implementation for inverse(),
//                                tinverse(), solve(matrix), extract_q_and_r().
// \endverbatim

#include <iosfwd>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_algo_export.h>
#include <vcl_compiler.h>

//: Extract the Q*R decomposition of matrix M.
//  The decomposition is stored in a compact and time-efficient
// packed form, which is most easily used via the "solve" and
// "determinant" methods.

VCL_TEMPLATE_EXPORT template <class T>
class vnl_qr
{
 public:
  vnl_qr(vnl_matrix<T> const & M);
 ~vnl_qr();

  //: return the inverse matrix of M
  vnl_matrix<T> inverse () const;
  //: return the transpose of the inverse matrix of M
  vnl_matrix<T> tinverse () const;
  //: return the original matrix M
  vnl_matrix<T> recompose () const;

  //: Solve equation M x = rhs for x using the computed decomposition.
  vnl_matrix<T> solve (const vnl_matrix<T>& rhs) const;
  //: Solve equation M x = rhs for x using the computed decomposition.
  vnl_vector<T> solve (const vnl_vector<T>& rhs) const;

  //: Return the determinant of M.  This is computed from M = Q R as follows:
  // |M| = |Q| |R|.
  // |R| is the product of the diagonal elements.
  // |Q| is (-1)^n as it is a product of Householder reflections.
  // So det = -prod(-r_ii).
  T determinant() const;
  //: Unpack and return unitary part Q.
  vnl_matrix<T> const& Q() const;
  //: Unpack and return R.
  vnl_matrix<T> const& R() const;
  //: Return residual vector d of M x = b -> d = Q'b
  vnl_vector<T> QtB(const vnl_vector<T>& b) const;

  void extract_q_and_r(vnl_matrix<T>* q, vnl_matrix<T>* r) const { *q = Q(); *r = R(); }

 private:
  vnl_matrix<T> qrdc_out_;
  vnl_vector<T> qraux_;
  vnl_vector<long> jpvt_;
  mutable vnl_matrix<T>* Q_;
  mutable vnl_matrix<T>* R_;

  // Disallow assignment.
  vnl_qr(const vnl_qr<T> &) { }
  vnl_qr<T>& operator=(const vnl_qr<T> &) { return *this; }
};

//: Compute determinant of matrix "M" using QR.
template <class T>
inline T vnl_qr_determinant(vnl_matrix<T> const& m)
{
  return vnl_qr<T>(m).determinant();
}

VCL_TEMPLATE_EXPORT template <class T>
std::ostream& operator<<(std::ostream&, vnl_qr<T> const & qr);

#endif // vnl_qr_h_
