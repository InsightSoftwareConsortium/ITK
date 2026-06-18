// This is core/vnl/algo/vnl_cholesky.cxx
//:
// \file
// vnl_cholesky
// \author Andrew W. Fitzgibbon, Oxford RRG
// Created: 08 Dec 96
//
//-----------------------------------------------------------------------------

#include <cmath>
#include <cassert>
#include <iostream>
#include "vnl_cholesky.h"

// Native lower-Cholesky engine. The lower triangle of A_ holds L with A = L L';
// the strict upper triangle keeps the original M -- the layout the
// lower_triangle()/upper_triangle()/L_badly_named_method() accessors read.
namespace
{
//: In-place lower Cholesky factor; returns 0 on success or the 1-based order of
//  the first leading minor that is not positive definite.
long
cholesky_factor(vnl_matrix<double> & A, long n)
{
  for (long j = 0; j < n; ++j)
  {
    double diag = A(j, j);
    for (long k = 0; k < j; ++k)
      diag -= A(j, k) * A(j, k);
    if (diag <= 0.0)
      return j + 1;
    const double Ljj = std::sqrt(diag);
    A(j, j) = Ljj;
    for (long i = j + 1; i < n; ++i)
    {
      double off = A(i, j);
      for (long k = 0; k < j; ++k)
        off -= A(i, k) * A(j, k);
      A(i, j) = off / Ljj;
    }
  }
  return 0;
}

//: Solve A x = b in place (x holds b on entry) using the factor A = L L'.
void
cholesky_solve(const vnl_matrix<double> & A, long n, double * x)
{
  for (long i = 0; i < n; ++i) // forward: L y = b
  {
    double s = x[i];
    for (long k = 0; k < i; ++k)
      s -= A(i, k) * x[k];
    x[i] = s / A(i, i);
  }
  for (long i = n - 1; i >= 0; --i) // back: L' x = y
  {
    double s = x[i];
    for (long k = i + 1; k < n; ++k)
      s -= A(k, i) * x[k];
    x[i] = s / A(i, i);
  }
}

//: Higham/Hager 1-norm estimate of ||A^{-1}||_1 (A symmetric => = ||A^{-1}||_inf),
//  returning the amplifying direction in nullvec. anorm is ||A||_1 of the
//  original matrix. Sets rcond = 1/(anorm * est), clamped to [0,1].
void
estimate_rcond(const vnl_matrix<double> & A, long n, double anorm, double & rcond, vnl_vector<double> & nullvec)
{
  vnl_vector<double> x(n, 1.0 / static_cast<double>(n));
  vnl_vector<double> y(n);
  vnl_vector<double> xi(n);
  double             est = 0.0;
  for (int iter = 0; iter < 5; ++iter)
  {
    y = x;
    cholesky_solve(A, n, y.data_block()); // y = A^{-1} x
    est = y.one_norm();
    for (long i = 0; i < n; ++i)
      xi[i] = (y[i] >= 0.0) ? 1.0 : -1.0;
    vnl_vector<double> z = xi;
    cholesky_solve(A, n, z.data_block()); // z = A^{-1} xi (A symmetric)
    long   jmax = 0;
    double zmax = std::fabs(z[0]);
    for (long i = 1; i < n; ++i)
      if (std::fabs(z[i]) > zmax)
      {
        zmax = std::fabs(z[i]);
        jmax = i;
      }
    if (zmax <= dot_product(z, x))
      break;
    x.fill(0.0);
    x[jmax] = 1.0;
  }
  nullvec = y;
  if (const double ynorm = y.two_norm(); ynorm > 0.0)
    nullvec /= ynorm;
  const double denom = anorm * est;
  rcond = (denom > 0.0) ? 1.0 / denom : 0.0;
  if (rcond > 1.0)
    rcond = 1.0;
}
} // namespace

//: Cholesky decomposition.
// Make cholesky decomposition of M optionally computing
// the reciprocal condition number.  If mode is estimate_condition, the
// condition number and an approximate nullspace are estimated.

vnl_cholesky::vnl_cholesky(const vnl_matrix<double> & M, Operation mode)
  : A_(M)
{
  long n = M.columns();
  assert(n == (int)(M.rows()));
  num_dims_rank_def_ = -1;
  if (std::fabs(M(0, n - 1) - M(n - 1, 0)) > 1e-8)
  {
    std::cerr << "vnl_cholesky: WARNING: non-symmetric: " << M << std::endl;
  }

  if (mode != estimate_condition)
  {
    // Quick factorization
    num_dims_rank_def_ = cholesky_factor(A_, n);
    if (mode == verbose && num_dims_rank_def_ != 0)
      std::cerr << "vnl_cholesky: " << num_dims_rank_def_ << " dimensions of non-posdeffness\n";
  }
  else
  {
    double anorm = 0.0; // 1-norm of the symmetric M, computed before factoring
    for (long j = 0; j < n; ++j)
    {
      double colsum = 0.0;
      for (long i = 0; i < n; ++i)
        colsum += std::fabs(M(i, j));
      if (colsum > anorm)
        anorm = colsum;
    }
    nullvector_.set_size(n);
    num_dims_rank_def_ = cholesky_factor(A_, n);
    if (num_dims_rank_def_ != 0)
    {
      rcond_ = 0.0;
      nullvector_.fill(0.0);
      std::cerr << "vnl_cholesky: rcond=" << rcond_ << " so " << num_dims_rank_def_
                << " dimensions of non-posdeffness\n";
    }
    else
    {
      estimate_rcond(A_, n, anorm, rcond_, nullvector_);
    }
  }
}

//: Solve least squares problem M x = b.
//  The right-hand-side std::vector x may be b,
//  which will give a fractional increase in speed.
void
vnl_cholesky::solve(const vnl_vector<double> & b, vnl_vector<double> * x) const
{
  assert(b.size() == A_.columns());

  *x = b;
  const long n = A_.columns();
  cholesky_solve(A_, n, x->data_block());
}

//: Solve least squares problem M x = b.
vnl_vector<double>
vnl_cholesky::solve(const vnl_vector<double> & b) const
{
  assert(b.size() == A_.columns());

  const long         n = A_.columns();
  vnl_vector<double> ret = b;
  cholesky_solve(A_, n, ret.data_block());
  return ret;
}

//: Compute determinant.
double
vnl_cholesky::determinant() const
{
  // det(M) = prod(L_ii)^2; carry a normalized mantissa/exponent so a transient
  // partial product cannot overflow when the final value is representable.
  const long n = A_.columns();
  double     mantissa = 1.0;
  long       exponent = 0;
  for (long i = 0; i < n; ++i)
  {
    mantissa *= A_(i, i);
    while (std::fabs(mantissa) >= 10.0)
    {
      mantissa /= 10.0;
      ++exponent;
    }
    while (mantissa != 0.0 && std::fabs(mantissa) < 1.0)
    {
      mantissa *= 10.0;
      --exponent;
    }
  }
  return mantissa * mantissa * std::pow(10.0, 2.0 * static_cast<double>(exponent));
}

// : Compute inverse.  Not efficient.
vnl_matrix<double>
vnl_cholesky::inverse() const
{
  if (num_dims_rank_def_)
  {
    std::cerr << "vnl_cholesky: Calling inverse() on rank-deficient matrix\n";
    return {};
  }

  const long         n = A_.columns();
  vnl_matrix<double> I(n, n);
  vnl_vector<double> col(n);
  for (long j = 0; j < n; ++j)
  {
    col.fill(0.0);
    col[j] = 1.0;
    cholesky_solve(A_, n, col.data_block());
    for (long i = 0; i < n; ++i)
      I(i, j) = col[i];
  }
  return I;
}

//: Return lower-triangular factor.
vnl_matrix<double>
vnl_cholesky::lower_triangle() const
{
  const unsigned n = A_.columns();
  vnl_matrix<double> L(n, n);
  // Zap upper triangle and transpose
  for (unsigned i = 0; i < n; ++i)
  {
    L(i, i) = A_(i, i);
    for (unsigned j = i + 1; j < n; ++j)
    {
      L(j, i) = A_(j, i);
      L(i, j) = 0;
    }
  }
  return L;
}


//: Return upper-triangular factor.
vnl_matrix<double>
vnl_cholesky::upper_triangle() const
{
  const unsigned n = A_.columns();
  vnl_matrix<double> U(n, n);
  // Zap lower triangle and transpose
  for (unsigned i = 0; i < n; ++i)
  {
    U(i, i) = A_(i, i);
    for (unsigned j = i + 1; j < n; ++j)
    {
      U(i, j) = A_(j, i);
      U(j, i) = 0;
    }
  }
  return U;
}
