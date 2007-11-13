// This is core/vnl/algo/vnl_svd.h
#ifndef vnl_svd_h_
#define vnl_svd_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Holds the singular value decomposition of a vnl_matrix.
// \author Andrew W. Fitzgibbon, Oxford IERG
// \date   15 Jul 96
//
// \verbatim
//  Modifications
//   fsm, Oxford IESRG, 26 Mar 1999
//     1. The singular values are now stored as reals (not complexes) when T is complex.
//     2. Fixed bug : for complex T, matrices have to be conjugated as well as transposed.
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim

#include <vnl/vnl_numeric_traits.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>
#include <vcl_iosfwd.h>

//: Holds the singular value decomposition of a vnl_matrix.
//
//  The class holds three matrices U, W, V such that the original matrix
//  $M = U W V^\top$.  The DiagMatrix W stores the singular values in decreasing
//  order.  The columns of U which correspond to the nonzero singular values
//  form a basis for range of M, while the columns of V corresponding to the
//  zero singular values are the nullspace.
//
//  The SVD is computed at construction time, and inquiries may then be made
//  of the SVD.  In particular, this allows easy access to multiple
//  right-hand-side solves without the bother of putting all the RHS's into a
//  Matrix.
//
//  This class is supplied even though there is an existing vnl_matrix method
//  for several reasons:
//
//  It is more convenient to use as it manages all the storage for
//  the U,S,V matrices, allowing repeated queries of the same SVD
//  results.
//
//  It avoids namespace clutter in the Matrix class.   While svd()
//  is a perfectly reasonable method for a Matrix, there are many other
//  decompositions that might be of interest, and adding them all would
//  make for a very large Matrix class.
//
//  It demonstrates the holder model of compute class, implementing an
//  algorithm on an object without adding a member that may not be of
//  general interest.  A similar pattern can be used for other
//  decompositions which are not defined as members of the library Matrix
//  class.
//
//  It extends readily to n-ary operations, such as generalized
//  eigensystems, which cannot be members of just one matrix.

export template <class T>
class vnl_svd
{
 public:
  //: The singular values of a matrix of complex<T> are of type T, not complex<T>
  typedef typename vnl_numeric_traits<T>::abs_t singval_t;

  //:
  // Construct an vnl_svd<T> object from $m \times n$ matrix $M$.  The
  // vnl_svd<T> object contains matrices $U$, $W$, $V$ such that
  // $U W V^\top = M$.
  //
  // Uses linpack routine DSVDC to calculate an ``economy-size'' SVD
  // where the returned $U$ is the same size as $M$, while $W$
  // and $V$ are both $n \times n$.  This is efficient for
  // large rectangular solves where $m > n$, typical in least squares.
  //
  // The optional argument zero_out_tol is used to mark the zero singular
  // values: If nonnegative, any s.v. smaller than zero_out_tol in
  // absolute value is set to zero.  If zero_out_tol is negative, the
  // zeroing is relative to |zero_out_tol| * sigma_max();

  vnl_svd(vnl_matrix<T> const &M, double zero_out_tol = 0.0);
 ~vnl_svd() {}

  // Data Access---------------------------------------------------------------

  //: find weights below threshold tol, zero them out, and update W_ and Winverse_
  void            zero_out_absolute(double tol = 1e-8); //sqrt(machine epsilon)

  //: find weights below tol*max(w) and zero them out
  void            zero_out_relative(double tol = 1e-8); //sqrt(machine epsilon)
  int             singularities () const { return W_.rows() - rank(); }
  unsigned int    rank () const { return rank_; }
  singval_t       well_condition () const { return sigma_min()/sigma_max(); }

  //: Calculate determinant as product of diagonals in W.
  singval_t       determinant_magnitude () const;
  singval_t       norm() const;

  //: Return the matrix U.
  vnl_matrix<T>      & U()       { return U_; }

  //: Return the matrix U.
  vnl_matrix<T> const& U() const { return U_; }

  //: Return the matrix U's (i,j)th entry (to avoid svd.U()(i,j); ).
  T U(int i, int j) const { return U_(i,j); }

  //: Get at DiagMatrix (q.v.) of singular values, sorted from largest to smallest
  vnl_diag_matrix<singval_t>       & W()             { return W_; }

  //: Get at DiagMatrix (q.v.) of singular values, sorted from largest to smallest
  vnl_diag_matrix<singval_t> const & W() const       { return W_; }
  vnl_diag_matrix<singval_t>       & Winverse()      { return Winverse_; }
  vnl_diag_matrix<singval_t> const & Winverse() const { return Winverse_; }
  singval_t                   & W(int i, int j) { return W_(i,j); }
  singval_t                   & W(int i)        { return W_(i,i); }
  singval_t     sigma_max() const { return W_(0,0); }       // largest
  singval_t     sigma_min() const { return W_(n_-1,n_-1); } // smallest

  //: Return the matrix V.
  vnl_matrix<T>      & V()       { return V_; }

  //: Return the matrix V.
  vnl_matrix<T> const& V() const { return V_; }

  //: Return the matrix V's (i,j)th entry (to avoid svd.V()(i,j); ).
  T V(int i, int j) const { return V_(i,j); }

  //:
  inline vnl_matrix<T> inverse () const { return pinverse(); }

  //: pseudo-inverse (for non-square matrix) of desired rank.
  vnl_matrix<T> pinverse (unsigned int rank = ~0u) const; // ~0u == (unsigned int)-1

  //: Calculate inverse of transpose, using desired rank.
  vnl_matrix<T> tinverse (unsigned int rank = ~0u) const; // ~0u == (unsigned int)-1

  //: Recompose SVD to U*W*V', using desired rank.
  vnl_matrix<T> recompose (unsigned int rank = ~0u) const; // ~0u == (unsigned int)-1

  //: Solve the matrix equation M X = B, returning X
  vnl_matrix<T> solve (vnl_matrix<T> const& B) const;

  //: Solve the matrix-vector system M x = y, returning x.
  vnl_vector<T> solve (vnl_vector<T> const& y) const;
  void          solve (T const *rhs, T *lhs) const; // min ||A*lhs - rhs||

  //: Solve the matrix-vector system M x = y.
  // Assuming that the singular values W have been preinverted by the caller.
  void solve_preinverted(vnl_vector<T> const& rhs, vnl_vector<T>* out) const;

  //: Return N such that M * N = 0
  vnl_matrix<T> nullspace() const;

  //: Return N such that M' * N = 0
  vnl_matrix<T> left_nullspace() const;

  //: Return N such that M * N = 0
  vnl_matrix<T> nullspace(int required_nullspace_dimension) const;

  //: Implementation to be done yet; currently returns left_nullspace(). - PVR.
  vnl_matrix<T> left_nullspace(int required_nullspace_dimension) const;

  //: Return the rightmost column of V.
  //  Does not check to see whether or not the matrix actually was rank-deficient -
  // the caller is assumed to have examined W and decided that to his or her satisfaction.
  vnl_vector<T> nullvector() const;

  //: Return the rightmost column of U.
  //  Does not check to see whether or not the matrix actually was rank-deficient.
  vnl_vector<T> left_nullvector() const;

  bool valid() const { return valid_; }

 private:

  int m_, n_;              // Size of M, local cache.
  vnl_matrix<T> U_;        // Columns Ui are basis for range of M for Wi != 0
  vnl_diag_matrix<singval_t> W_;// Singular values, sorted in decreasing order
  vnl_diag_matrix<singval_t> Winverse_;
  vnl_matrix<T> V_;       // Columns Vi are basis for nullspace of M for Wi = 0
  unsigned rank_;
  bool have_max_;
  singval_t max_;
  bool have_min_;
  singval_t min_;
  double last_tol_;
  bool valid_;        // false if the NETLIB call failed.

  // Disallow assignment.
  vnl_svd(vnl_svd<T> const &) { }
  vnl_svd<T>& operator=(vnl_svd<T> const &) { return *this; }
};

template <class T>
inline
vnl_matrix<T> vnl_svd_inverse(vnl_matrix<T> const& m)
{
  return vnl_svd<T>(m).inverse();
}

export template <class T>
vcl_ostream& operator<<(vcl_ostream&, vnl_svd<T> const& svd);

#endif // vnl_svd_h_
