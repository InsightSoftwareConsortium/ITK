// This is core/vnl/algo/vnl_symmetric_eigensystem.h
#ifndef vnl_symmetric_eigensystem_h_
#define vnl_symmetric_eigensystem_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Find eigenvalues of a symmetric matrix
//
//    vnl_symmetric_eigensystem_compute()
//    solves the eigenproblem $A x = \lambda x$, with $A$ symmetric.
//    The resulting eigenvectors and values are sorted in increasing order
//    so <CODE> V.column(0) </CODE> is the eigenvector corresponding to the
//    smallest eigenvalue.
//
//    As a matrix decomposition, this is $A = V D V^t$
//
//    Uses the EISPACK routine RS, which in turn calls TRED2 to reduce A
//    to tridiagonal form, followed by TQL2, to find the eigensystem.
//    This is summarized in Golub and van Loan, pgf 8.2.  The following are
//    the original subroutine headers:
//
// \remark TRED2 is a translation of the Algol procedure tred2,
//     Num. Math. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
//     Handbook for Auto. Comp., Vol.ii-Linear Algebra, 212-226(1971).
//
// \remark This subroutine reduces a real symmetric matrix to a
//     symmetric tridiagonal matrix using and accumulating
//     orthogonal similarity transformations.
//
// \remark TQL2 is a translation of the Algol procedure tql2,
//     Num. Math. 11, 293-306(1968) by Bowdler, Martin, Reinsch, and Wilkinson.
//     Handbook for Auto. Comp., Vol.ii-Linear Algebra, 227-240(1971).
//
// \remark This subroutine finds the eigenvalues and eigenvectors
//     of a symmetric tridiagonal matrix by the QL method.
//     the eigenvectors of a full symmetric matrix can also
//     be found if  tred2  has been used to reduce this
//     full matrix to tridiagonal form.
//
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   29 Aug 96
//
// \verbatim
//  Modifications
//   fsm, 5 March 2000: templated
//   dac (Manchester) 28/03/2001: tidied up documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
//   Jan.2003 - Peter Vanroose - added missing implementation for solve(b,x)
//   Mar.2010 - Peter Vanroose - also made vnl_symmetric_eigensystem_compute()
//                               & vnl_symmetric_eigensystem_compute_eigenvals() templated
// \endverbatim

#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>
#include <vnl/algo/vnl_algo_export.h>

//: Find eigenvalues of a symmetric 3x3 matrix
// Eigenvalues will be returned so that l1 <= l2 <= l3.
// \verbatim
// Matrix is   M11  M12  M13
//             M12  M22  M23
//             M13  M23  M33
// \endverbatim
template <class T>
void vnl_symmetric_eigensystem_compute_eigenvals(
       T M11, T M12, T M13,
              T M22, T M23,
                     T M33,
       T &l1, T &l2, T &l3);

//: Find eigenvalues of a symmetric matrix
template <class T>
bool vnl_symmetric_eigensystem_compute(vnl_matrix<T> const & A,
                                       vnl_matrix<T> & V,
                                       vnl_vector<T> & D);

//: Computes and stores the eigensystem decomposition of a symmetric matrix.

VCL_TEMPLATE_EXPORT template <class T>
class vnl_symmetric_eigensystem
{
 public:
  //: Solve real symmetric eigensystem $A x = \lambda x$
  vnl_symmetric_eigensystem(vnl_matrix<T> const & M);

 protected:
  // need this here to get inits in correct order, but still keep gentex
  // in the right order.
  int n_;

 public:
  //: Public eigenvectors.
  //  After construction, the columns of V are the eigenvectors, sorted by
  // increasing eigenvalue, from most negative to most positive.
  vnl_matrix<T> V;

  //: Public eigenvalues.
  //  After construction,  D contains the eigenvalues, sorted as described above.
  //  Note that D is a vnl_diag_matrix, and is therefore stored as a std::vector while behaving as a matrix.
  vnl_diag_matrix<T> D;

  //: Recover specified eigenvector after computation.
  vnl_vector<T> get_eigenvector(int i) const;

  //: Recover specified eigenvalue after computation.
  T             get_eigenvalue(int i) const;

  //: Convenience method to get least-squares nullvector.
  // It is deliberate that the signature is the same as on vnl_svd<T>.
  vnl_vector<T> nullvector() const { return get_eigenvector(0); }

  //: Return the matrix $V  D  V^\top$.
  //  This can be useful if you've modified $D$.  So an inverse is obtained using
  // \code
  //   vnl_symmetric_eigensystem} eig(A);
  //   eig.D.invert_in_place}();
  //   vnl_matrix<double> Ainverse = eig.recompose();
  // \endcode

  vnl_matrix<T> recompose() const { return V * D * V.transpose(); }

  //: return product of eigenvalues.
  T determinant() const;

  //: return the pseudoinverse.
  vnl_matrix<T> pinverse() const;

  //: return the square root, if positive semi-definite.
  vnl_matrix<T> square_root() const;

  //: return the inverse of the square root, if positive semi-definite.
  vnl_matrix<T> inverse_square_root() const;

  //: Solve LS problem M x = b
  vnl_vector<T> solve(vnl_vector<T> const & b);

  //: Solve LS problem M x = b
  void solve(vnl_vector<T> const & b, vnl_vector<T> * x) { *x = solve(b); }
};

#define VNL_SYMMETRIC_EIGENSYSTEM_INSTANTIATE(T) \
extern "please include vnl/algo/vnl_symmetric_eigensystem.hxx first"

#endif // vnl_symmetric_eigensystem_h_
