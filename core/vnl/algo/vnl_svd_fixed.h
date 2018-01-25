// This is core/vnl/algo/vnl_svd_fixed.h
#ifndef vnl_svd_fixed_h_
#define vnl_svd_fixed_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Holds the singular value decomposition of a vnl_matrix_fixed.
// \author Andrew W. Fitzgibbon, Ian Scott
// \date   12 Oct 2009
//

#include <iosfwd>
#include <vnl/vnl_numeric_traits.h>
#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>
#include <vnl/vnl_diag_matrix_fixed.h>
#include <vnl/algo/vnl_algo_export.h>
#include <vcl_compiler.h>

//: Holds the singular value decomposition of a vnl_matrix_fixed.
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

VCL_TEMPLATE_EXPORT template <class T, unsigned int R, unsigned int C>
class vnl_svd_fixed
{
 public:
  //: The singular values of a matrix of complex<T> are of type T, not complex<T>
  typedef typename vnl_numeric_traits<T>::abs_t singval_t;

  //:
  // Construct a vnl_svd_fixed<T> object from $m \times n$ matrix $M$.  The
  // vnl_svd_fixed<T> object contains matrices $U$, $W$, $V$ such that
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

  vnl_svd_fixed(vnl_matrix_fixed<T,R,C> const &M, double zero_out_tol = 0.0);
 ~vnl_svd_fixed() {}

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
  vnl_matrix_fixed<T,R,C>      & U()       { return U_; }

  //: Return the matrix U.
  vnl_matrix_fixed<T,R,C> const& U() const { return U_; }

  //: Return the matrix U's (i,j)th entry (to avoid svd.U()(i,j); ).
  T U(int i, int j) const { return U_(i,j); }

  //: Get at DiagMatrix (q.v.) of singular values, sorted from largest to smallest
  vnl_diag_matrix_fixed<singval_t, C>       & W()             { return W_; }

  //: Get at DiagMatrix (q.v.) of singular values, sorted from largest to smallest
  vnl_diag_matrix_fixed<singval_t, C> const & W() const       { return W_; }
  vnl_diag_matrix_fixed<singval_t, C>       & Winverse()      { return Winverse_; }
  vnl_diag_matrix_fixed<singval_t, C> const & Winverse() const { return Winverse_; }
  singval_t                   & W(int i, int j) { return W_(i,j); }
  singval_t                   & W(int i)        { return W_(i,i); }
  singval_t     sigma_max() const { return W_(0,0); }       // largest
  singval_t     sigma_min() const { return W_(C-1,C-1); } // smallest

  //: Return the matrix V.
  vnl_matrix_fixed<T,C,C>      & V()       { return V_; }

  //: Return the matrix V.
  vnl_matrix_fixed<T,C,C> const& V() const { return V_; }

  //: Return the matrix V's (i,j)th entry (to avoid svd.V()(i,j); ).
  T V(int i, int j) const { return V_(i,j); }

  //:
  inline vnl_matrix_fixed<T,C,R> inverse () const { return pinverse(); }

  //: pseudo-inverse (for non-square matrix) of desired rank.
  vnl_matrix_fixed<T,C,R> pinverse (unsigned int rank = ~0u) const; // ~0u == (unsigned int)-1

  //: Calculate inverse of transpose, using desired rank.
  vnl_matrix_fixed<T,R,C> tinverse (unsigned int rank = ~0u) const; // ~0u == (unsigned int)-1

  //: Recompose SVD to U*W*V', using desired rank.
  vnl_matrix_fixed<T,R,C> recompose (unsigned int rank = ~0u) const; // ~0u == (unsigned int)-1

  //: Solve the matrix equation M X = B, returning X
  vnl_matrix<T> solve (vnl_matrix<T> const& B) const;

  //: Solve the matrix-vector system M x = y, returning x.
  vnl_vector_fixed<T, C> solve (vnl_vector_fixed<T, R> const& y) const;
  void          solve (T const *rhs, T *lhs) const; // min ||A*lhs - rhs||

  //: Solve the matrix-vector system M x = y.
  // Assuming that the singular values W have been preinverted by the caller.
  void solve_preinverted(vnl_vector_fixed<T,R> const& rhs, vnl_vector_fixed<T,C>* out) const;

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
  vnl_vector_fixed<T,C> nullvector() const;

  //: Return the rightmost column of U.
  //  Does not check to see whether or not the matrix actually was rank-deficient.
  vnl_vector_fixed<T,R> left_nullvector() const;

  bool valid() const { return valid_; }

 private:

  vnl_matrix_fixed<T, R, C> U_;        // Columns Ui are basis for range of M for Wi != 0
  vnl_diag_matrix_fixed<singval_t, C> W_;// Singular values, sorted in decreasing order
  vnl_diag_matrix_fixed<singval_t, C> Winverse_;
  vnl_matrix_fixed<T, C, C> V_;       // Columns Vi are basis for nullspace of M for Wi = 0
  unsigned rank_;
  bool have_max_;
  singval_t max_;
  bool have_min_;
  singval_t min_;
  double last_tol_;
  bool valid_;        // false if the NETLIB call failed.

  // Disallow assignment.
  vnl_svd_fixed<T,R,C>(vnl_svd_fixed<T,R,C> const &) { }
  vnl_svd_fixed<T,R,C>& operator=(vnl_svd_fixed<T,R,C> const &) { return *this; }
};

template <class T, unsigned int R, unsigned int C>
inline
vnl_matrix_fixed<T,C,R> vnl_svd_fixed_inverse(vnl_matrix_fixed<T,R,C> const& m)
{
  return vnl_svd_fixed<T,R,C>(m).inverse();
}

VCL_TEMPLATE_EXPORT template <class T, unsigned int R, unsigned int C>
std::ostream& operator<<(std::ostream&, vnl_svd_fixed<T,R,C> const& svd);

#endif // vnl_svd_fixed_h_
