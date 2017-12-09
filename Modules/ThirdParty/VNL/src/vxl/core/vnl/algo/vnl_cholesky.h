// This is core/vnl/algo/vnl_cholesky.h
#ifndef vnl_cholesky_h_
#define vnl_cholesky_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Decomposition of symmetric matrix
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   08 Dec 96
//
// \verbatim
//  Modifications
//   Peter Vanroose, Leuven, Apr 1998: added L() (return decomposition matrix)
//   dac (Manchester) 26/03/2001: tidied up documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_algo_export.h>

//: Decomposition of symmetric matrix.
//  A class to hold the Cholesky decomposition of a symmetric matrix and
//  use that to solve linear systems, compute determinants and inverses.
//  The cholesky decomposition decomposes symmetric A = L*L.transpose()
//  where L is lower triangular
//
//  To check that the decomposition can be used safely for solving a linear
//  equation it is wise to construct with mode==estimate_condition and
//  check that rcond()>sqrt(machine precision).  If this is not the case
//  it might be a good idea to use vnl_svd instead.
class VNL_ALGO_EXPORT vnl_cholesky
{
 public:
  //: Modes of computation.  See constructor for details.
  enum Operation {
    quiet,
    verbose,
    estimate_condition
  };

  //: Make cholesky decomposition of M optionally computing the reciprocal condition number.
  vnl_cholesky(vnl_matrix<double> const& M, Operation mode = verbose);
 ~vnl_cholesky() {}

  //: Solve LS problem M x = b
  vnl_vector<double> solve(vnl_vector<double> const& b) const;

  //: Solve LS problem M x = b
  void solve(vnl_vector<double> const& b, vnl_vector<double>* x) const;

  //: Compute determinant
  double determinant() const;

  //   Compute inverse.  Not efficient.
  // It's broken, I don't have time to fix it.
  // Mail awf@robots if you need it and I'll tell you as much as I can
  // to fix it.
  vnl_matrix<double> inverse() const;

  //: Return lower-triangular factor.
  vnl_matrix<double> lower_triangle() const;

  //: Return upper-triangular factor.
  vnl_matrix<double> upper_triangle() const;

  //: Return the decomposition matrix
  vnl_matrix<double> const& L_badly_named_method() const { return A_; }

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
  vnl_matrix<double> A_;
  double rcond_;
  long num_dims_rank_def_;
  vnl_vector<double> nullvector_;

 private:
  //: Copy constructor - privatised to avoid it being used
  vnl_cholesky(vnl_cholesky const & that);
  //: Assignment operator - privatised to avoid it being used
  vnl_cholesky& operator=(vnl_cholesky const & that);
};

#endif // vnl_cholesky_h_
