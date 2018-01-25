// This is core/vnl/algo/vnl_ldl_cholesky.h
#ifndef vnl_ldl_cholesky_h_
#define vnl_ldl_cholesky_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Updateable Cholesky decomposition: A=LDL'
// \author Tim Cootes
// \date   29 Mar 2006

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_algo_export.h>

//: Updateable Cholesky decomposition: A=LDL'
//  A class to hold the Cholesky decomposition of a positive definite
//  symmetric matrix, using the form A=LDL', where L is lower triangular
//  with ones on the leading diagonal, and D is a diagonal matrix.
//  This differs from vnl_cholesky, which decomposes as A=LL', without
//  a constraint on the diagonal.  The advantage of the LDL' form is that
//  it can be efficiently updated.  Thus given the decomposition of A,
//  we can compute the decomposition of (A+v.v') in O(n^2) time.
//
//  This can be used to solve linear systems, compute determinants and inverses.
//
//  To check that the decomposition can be used safely for solving a linear
//  equation it is wise to construct with mode==estimate_condition and
//  check that rcond()>sqrt(machine precision).  If this is not the case
//  it might be a good idea to use vnl_svd instead.
class VNL_ALGO_EXPORT vnl_ldl_cholesky
{
 public:
  //: Modes of computation.  See constructor for details.
  enum Operation {
    quiet,
    verbose,
    estimate_condition
  };

  //: Make cholesky decomposition of M optionally computing the reciprocal condition number.
  vnl_ldl_cholesky(vnl_matrix<double> const& M, Operation mode = verbose);
 ~vnl_ldl_cholesky() {}

  //: Solve LS problem M x = b
  vnl_vector<double> solve(vnl_vector<double> const& b) const;

  //: Solve LS problem M x = b
  void solve(vnl_vector<double> const& b, vnl_vector<double>* x) const;

  //: Solve equation of form Lx=y (in-place)
  //  x is overwritten with solution
  void solve_lx(vnl_vector<double>& y);

  //: Compute determinant
  double determinant() const;

  //: Compute rank-1 update, ie the decomposition of (M+v.v')
  //  If the initial state is the decomposition of M, then
  //  L and D are updated so that on exit  LDL'=M+v.v'
  void rank1_update(const vnl_vector<double>& v);

  //: Multi-rank update, ie the decomposition of (M+W.W')
  //  If the initial state is the decomposition of M, then
  //  L and D are updated so that on exit  LDL'=M+W.W'
  void update(const vnl_matrix<double>& W);

  //:   Compute inverse.  Not efficient.
  //  Note that you rarely need the inverse - backsubstitution
  //  is faster and less prone to rounding errors.
  vnl_matrix<double> inverse() const;

  //: Return lower-triangular factor.
  const vnl_matrix<double>& lower_triangle() const { return L_; }

  //: Return upper-triangular factor.
  vnl_matrix<double> upper_triangle() const { return L_.transpose(); }

  //: Return elements of diagonal matrix D in LDL'
  const vnl_vector<double>& diagonal() const { return d_; }

  //: Efficient computation of x' * inv(M) * x
  //  Useful when M is a covariance matrix!
  double xt_m_inv_x(const vnl_vector<double>& x) const;

  //: Efficient computation of x' * M * x
  //  Twice as fast as explicitly computing x' * M * x
  double xt_m_x(const vnl_vector<double>& x) const;

  //: A Success/failure flag
  int rank_deficiency() const { return num_dims_rank_def_; }

  //: Return reciprocal condition number (smallest/largest singular values).
  // As long as rcond()>sqrt(precision) the decomposition can be used for
  // solving equations safely.
  // Not calculated unless Operation mode at construction was estimate_condition.
  double rcond() const { return rcond_; }

  //: Return computed nullvector.
  // Not calculated unless Operation mode at construction was estimate_condition.
  vnl_vector<double>      & nullvector()       { return nullvector_; }
  vnl_vector<double> const& nullvector() const { return nullvector_; }

 protected:
  // Data Members--------------------------------------------------------------

  //: Lower triangular matrix
  vnl_matrix<double> L_;

  //: Elements of diagonal matrix
  vnl_vector<double> d_;

  //: 1/(condition number)
  double rcond_;
  long num_dims_rank_def_;
  vnl_vector<double> nullvector_;

 private:
  //: Copy constructor - privatised to avoid it being used
  vnl_ldl_cholesky(vnl_ldl_cholesky const & that);
  //: Assignment operator - privatised to avoid it being used
  vnl_ldl_cholesky& operator=(vnl_ldl_cholesky const & that);

  //: Solve Mx=b, overwriting input vector with the solution.
  //  x points to beginning of an n-element vector containing b
  //  On exit, x[i] filled with solution vector.
  void inplace_solve(double* x) const;
};

#endif // vnl_ldl_cholesky_h_
