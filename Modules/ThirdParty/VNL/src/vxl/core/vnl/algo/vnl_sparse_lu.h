// This is core/vnl/algo/vnl_sparse_lu.h
#ifndef vnl_sparse_lu_h_
#define vnl_sparse_lu_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Solution of the linear system Mx = b for sparse matrices
// \author J. L. Mundy
// \date   27 Dec 2006
//
// \verbatim
//  Modifications
//  None
// \endverbatim

#include <vnl/vnl_vector.h>
#include <vnl/vnl_sparse_matrix.h>
#include <vnl/algo/vnl_algo_export.h>

//: Linear system solver for Mx = b using LU decomposition of a sparse matrix
//  Encapsulating Sparse 1.3 by Kenneth S. Kundert.
//  The matrix is factored before solution. Any number of b vectors can
//  be applied after the matrix is factored with out recomputation of the
//  LU form. It is advised to construct with mode==estimate_condition and
//  check that rcond()>sqrt(machine precision).  If this is not the case
//  the solution may be suspect. An upper bound on error is provided.
//  The solution of M^t x = b is also available.

class VNL_ALGO_EXPORT vnl_sparse_lu
{
 public:
  //: Modes of computation.  See constructor for details.
  enum operation {
    quiet,
    verbose,
    estimate_condition,
    estimate_condition_verbose
  };

  //: Make sparse_lu decomposition of M optionally computing the reciprocal condition number.
  vnl_sparse_lu(vnl_sparse_matrix<double> const& M, operation mode = quiet);
 ~vnl_sparse_lu();

  //: set the relative pivot threshold should be between 0 and 1
  // If set to one then pivoting is complete and slow
  // If near zero then roundoff error may be prohibitive but computation is fast
  // Typical values are between 0.01 and 0.1.
  void set_pivot_thresh(double pivot_thresh){pivot_thresh_=pivot_thresh;}

  //: set the threshold on absolute element magnitude for pivoting
  // Should be either zero or significantly smaller than the absolute
  // value of the smallest diagonal element.
  void set_absolute_thresh(double absolute_thresh){absolute_thresh_=absolute_thresh;}
  //: set diagonal pivoting mode, normally 1 which gives priority to diagonal elements.
  void set_diagonal_pivoting(int diag_pivoting){diag_pivoting_=diag_pivoting;}

  //: Solve problem M x = b
  vnl_vector<double> solve(vnl_vector<double> const& b);

  //: Solve problem M x = b
  void solve(vnl_vector<double> const& b, vnl_vector<double>* x);

  //: Solve problem M^t x = b
  vnl_vector<double> solve_transpose(vnl_vector<double> const& b);

  //: Solve problem M^t x = b
  void solve_transpose(vnl_vector<double> const& b, vnl_vector<double>* x);

  //: Compute determinant
  double determinant();

  //: Return reciprocal condition number (smallest/largest singular values).
  // As long as rcond()>sqrt(precision) the decomposition can be used for
  // solving equations safely.
  // Not calculated unless operation mode at construction includes estimate_condition.
  double rcond();

  //: An estimate of maximum error in solution.
  // Not calculated unless operation mode at construction includes estimate_condition.
  double max_error_bound();

 protected:
  // Internals
  void decompose_matrix();
  bool est_condition();
  // Data Members--------------------------------------------------------------
  vnl_sparse_matrix<double> A_;
  bool factored_;
  bool condition_computed_;
  operation mode_;
  double norm_;
  double rcond_;
  double largest_;
  double pivot_thresh_;
  double absolute_thresh_;
  int diag_pivoting_;
 private:
  //: Copy constructor - privatised to avoid it being used
  vnl_sparse_lu(vnl_sparse_lu const & that);
  //: Assignment operator - privatised to avoid it being used
  vnl_sparse_lu& operator=(vnl_sparse_lu const & that);
  //: The internal matrix representation
  //
  // We don't use the typedef spMatrix directly here to avoid exposing
  // the implementation detail (sparse/spMatrix.h) to the user.
  void* pmatrix_;
};

#endif // vnl_sparse_lu_h_
