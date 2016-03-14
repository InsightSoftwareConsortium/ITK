// This is core/vnl/algo/vnl_cholesky.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// vnl_cholesky
// \author Andrew W. Fitzgibbon, Oxford RRG
// Created: 08 Dec 96
//
//-----------------------------------------------------------------------------

#include <cmath>
#include <iostream>
#include "vnl_cholesky.h"
#include <vcl_compiler.h>
#include <vcl_cassert.h>
#include <vnl/algo/vnl_netlib.h> // dpofa_(), dposl_(), dpoco_(), dpodi_()

//: Cholesky decomposition.
// Make cholesky decomposition of M optionally computing
// the reciprocal condition number.  If mode is estimate_condition, the
// condition number and an approximate nullspace are estimated, at a cost
// of a factor of (1 + 18/n).  Here's a table of 1 + 18/n:
// \verbatim
// n:              3      5     10     50    100    500   1000
// slowdown:     7.0    4.6    2.8    1.4   1.18   1.04   1.02
// \endverbatim

vnl_cholesky::vnl_cholesky(vnl_matrix<double> const & M, Operation mode):
  A_(M)
{
  long n = M.columns();
  assert(n == (int)(M.rows()));
  num_dims_rank_def_ = -1;
  if (std::fabs(M(0,n-1) - M(n-1,0)) > 1e-8) {
    std::cerr << "vnl_cholesky: WARNING: non-symmetric: " << M << std::endl;
  }

  if (mode != estimate_condition) {
    // Quick factorization
    v3p_netlib_dpofa_(A_.data_block(), &n, &n, &num_dims_rank_def_);
    if (mode == verbose && num_dims_rank_def_ != 0)
      std::cerr << "vnl_cholesky: " << num_dims_rank_def_ << " dimensions of non-posdeffness\n";
  }
  else {
    vnl_vector<double> nullvec(n);
    v3p_netlib_dpoco_(A_.data_block(), &n, &n, &rcond_, nullvec.data_block(), &num_dims_rank_def_);
    if (num_dims_rank_def_ != 0)
      std::cerr << "vnl_cholesky: rcond=" << rcond_ << " so " << num_dims_rank_def_ << " dimensions of non-posdeffness\n";
  }
}

//: Solve least squares problem M x = b.
//  The right-hand-side std::vector x may be b,
//  which will give a fractional increase in speed.
void vnl_cholesky::solve(vnl_vector<double> const& b, vnl_vector<double>* x) const
{
  assert(b.size() == A_.columns());

  *x = b;
  long n = A_.columns();
  v3p_netlib_dposl_(A_.data_block(), &n, &n, x->data_block());
}

//: Solve least squares problem M x = b.
vnl_vector<double> vnl_cholesky::solve(vnl_vector<double> const& b) const
{
  assert(b.size() == A_.columns());

  long n = A_.columns();
  vnl_vector<double> ret = b;
  v3p_netlib_dposl_(A_.data_block(), &n, &n, ret.data_block());
  return ret;
}

//: Compute determinant.
double vnl_cholesky::determinant() const
{
  long n = A_.columns();
  vnl_matrix<double> I = A_;
  double det[2];
  long job = 10;
  v3p_netlib_dpodi_(I.data_block(), &n, &n, det, &job);
  return det[0] * std::pow(10.0, det[1]);
}

// : Compute inverse.  Not efficient.
vnl_matrix<double> vnl_cholesky::inverse() const
{
  if (num_dims_rank_def_) {
    std::cerr << "vnl_cholesky: Calling inverse() on rank-deficient matrix\n";
    return vnl_matrix<double>();
  }

  long n = A_.columns();
  vnl_matrix<double> I = A_;
  long job = 01;
  v3p_netlib_dpodi_(I.data_block(), &n, &n, VXL_NULLPTR, &job);

  // Copy lower triangle into upper
  for (int i = 0; i < n; ++i)
    for (int j = i+1; j < n; ++j)
      I(i,j) = I(j,i);

  return I;
}

//: Return lower-triangular factor.
vnl_matrix<double> vnl_cholesky::lower_triangle() const
{
  unsigned n = A_.columns();
  vnl_matrix<double> L(n,n);
  // Zap upper triangle and transpose
  for (unsigned i = 0; i < n; ++i) {
    L(i,i) = A_(i,i);
    for (unsigned j = i+1; j < n; ++j) {
      L(j,i) = A_(j,i);
      L(i,j) = 0;
    }
  }
  return L;
}


//: Return upper-triangular factor.
vnl_matrix<double> vnl_cholesky::upper_triangle() const
{
  unsigned n = A_.columns();
  vnl_matrix<double> U(n,n);
  // Zap lower triangle and transpose
  for (unsigned i = 0; i < n; ++i) {
    U(i,i) = A_(i,i);
    for (unsigned j = i+1; j < n; ++j) {
      U(i,j) = A_(j,i);
      U(j,i) = 0;
    }
  }
  return U;
}

