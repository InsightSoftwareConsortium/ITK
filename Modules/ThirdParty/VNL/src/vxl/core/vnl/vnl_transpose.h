// This is core/vnl/vnl_transpose.h
#ifndef vnl_transpose_h_
#define vnl_transpose_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Efficient matrix transpose
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   23 Dec 96
//
// \verbatim
//  Modifications
//  LSB (Manchester) 19/3/01 Tidied documentation
// \endverbatim

#include <vcl_iostream.h>
#include <vnl/vnl_fastops.h>

//: Efficient matrix transpose
//  vnl_transpose is an efficient way to write C = vnl_transpose(A) * B.
//  The vnl_transpose class holds a reference to the original matrix
//  and when involved in an operation for which it has been specialized,
//  performs the operation without copying.
//
//  If the operation has not been specialized, the vnl_transpose performs
//  a copying conversion to a matrix, printing a message to stdout.
//  At that stage, the user may choose to implement the particular operation
//  or use vnl_transpose::asMatrix() to clear the warning.
//
//  NOTE: This is a reference class, so should be shorter-lived than the
//  matrix to which it refers.
//
//  NOTE: This only works for arguments of type vnl_matrix<double>

class vnl_transpose
{
  const vnl_matrix<double>& M_;
 public:

  //: Make a vnl_transpose object referring to matrix M
  vnl_transpose(const vnl_matrix<double>& M): M_(M) {}

  //: Noisily convert a vnl_transpose to a matrix
  operator vnl_matrix<double> () const {
    vcl_cerr << "vnl_transpose being converted to matrix -- help! I don't wanna go!\n";
    return M_.transpose();
  }

  //: Quietly convert a vnl_transpose to a matrix
  vnl_matrix<double> asMatrix () const { return M_.transpose(); }

  //: Return M' * O
  vnl_matrix<double> operator* (const vnl_matrix<double>& O) {
    vnl_matrix<double> ret(M_.columns(), O.columns());
    vnl_fastops::AtB(ret, M_, O);
    return ret;
  }

  //: Return M' * O
  vnl_vector<double> operator* (const vnl_vector<double>& O) {
    vnl_vector<double> ret(M_.columns());
    vnl_fastops::AtB(ret, M_, O);
    return ret;
  }

  //: Return A * B'
  friend vnl_matrix<double> operator* (const vnl_matrix<double>& A, const vnl_transpose& B) {
    vnl_matrix<double> ret(A.rows(), B.M_.rows());
    vnl_fastops::ABt(ret, A, B.M_);
    return ret;
  }
};

#endif // vnl_transpose_h_
