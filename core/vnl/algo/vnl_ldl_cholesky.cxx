// This is core/vnl/algo/vnl_ldl_cholesky.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \brief Updateable Cholesky decomposition: A=LDL'
// \author Tim Cootes
// \date   29 Mar 2006
//
//-----------------------------------------------------------------------------

#include <cmath>
#include <iostream>
#include "vnl_ldl_cholesky.h"
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

vnl_ldl_cholesky::vnl_ldl_cholesky(vnl_matrix<double> const & M, Operation mode):
  L_(M)
{
  long n = M.columns();
  assert(n == (int)(M.rows()));
  num_dims_rank_def_ = -1;
  if (std::fabs(M(0,n-1) - M(n-1,0)) > 1e-8) {
    std::cerr << "vnl_ldl_cholesky: WARNING: non-symmetric: " << M << std::endl;
  }

  if (mode != estimate_condition) {
    // Quick factorization
    v3p_netlib_dpofa_(L_.data_block(), &n, &n, &num_dims_rank_def_);
    if (mode == verbose && num_dims_rank_def_ != 0)
      std::cerr << "vnl_ldl_cholesky: " << num_dims_rank_def_ << " dimensions of non-posdeffness\n";
  }
  else {
    vnl_vector<double> nullvec(n);
    v3p_netlib_dpoco_(L_.data_block(), &n, &n, &rcond_, nullvec.data_block(), &num_dims_rank_def_);
    if (num_dims_rank_def_ != 0)
      std::cerr << "vnl_ldl_cholesky: rcond=" << rcond_ << " so " << num_dims_rank_def_ << " dimensions of non-posdeffness\n";
  }

  // L_ is currently part of plain decomposition, M=L_ * L_.transpose()
  // Extract diagonal and tweak L_
  d_.set_size(n);

    //: Sqrt of elements of diagonal matrix
  vnl_vector<double> sqrt_d(n);

  for (int i=0; i<n; ++i)
  {
    sqrt_d[i]=L_(i,i);
    d_[i]=sqrt_d[i]*sqrt_d[i];
  }

  // Scale column j by 1/sqrt_d_[i] and set upper triangular elements to zero
  for (int i=0; i<n; ++i)
  {
    double *row = L_[i];
    for (int j=0; j<i; ++j) row[j]/=sqrt_d[j];
    row[i]=1.0;
    for (int j=i+1; j<n; ++j) row[j]=0.0;   // Zero upper triangle
  }
}

//: Sum of v1[i]*v2[i]  (i=0..n-1)
inline double dot(const double* v1, const double* v2, unsigned n)
{
  double sum=0.0;
  for (unsigned i=0;i<n;++i) sum+= v1[i]*v2[i];
  return sum;
}
//: Sum of v1[i*s]*v2[i]  (i=0..n-1)
inline double dot(const double* v1, unsigned s, const double* v2, unsigned n)
{
  double sum=0.0;
  for (unsigned i=0;i<n;++i,v1+=s) sum+= (*v1)*v2[i];
  return sum;
}

//: Solve Lx=y (in-place)
//  x is overwritten with solution
void vnl_ldl_cholesky::solve_lx(vnl_vector<double>& x)
{
  unsigned n = d_.size();
  for (unsigned i=1;i<n;++i)
    x[i] -= dot(L_[i],x.data_block(),i);
}

//: Solve Mx=b, overwriting input vector with the solution.
//  x points to beginning of an n-element vector containing b
//  On exit, x[i] filled with solution vector.
void vnl_ldl_cholesky::inplace_solve(double* x) const
{
  unsigned n = d_.size();
  // Solve Ly=b for y
  for (unsigned i=1;i<n;++i)
    x[i] -= dot(L_[i],x,i);

  // Scale by inverse of D
  for (unsigned i=0;i<n;++i) x[i]/=d_[i];

  // Solve L'x=y for x
  const double* L_data = &L_(n-1,n-2);
  const double* x_data = &x[n-1];
  unsigned c=1;
  for (int i=n-2;i>=0;--i,L_data-=(n+1),--x_data,++c)
  {
    x[i] -= dot(L_data,n,x_data,c);
  }
}

//: Efficient computation of x' * inv(M) * x
//  Useful when M is a covariance matrix!
//  Solves Ly=x for y, then returns sum y[i]*y[i]/d[i]
double vnl_ldl_cholesky::xt_m_inv_x(const vnl_vector<double>& x) const
{
  unsigned n=d_.size();
  assert(x.size()==n);
  vnl_vector<double> y=x;
  // Solve Ly=x for y and compute sum as we go
  double sum = y[0]*y[0]/d_[0];
  for (unsigned i=1;i<n;++i)
  {
    y[i] -= dot(L_[i],y.data_block(),i);
    sum += y[i]*y[i]/d_[i];
  }
  return sum;
}

//: Efficient computation of x' * M * x
//  Twice as fast as explicitly computing x' * M * x
double vnl_ldl_cholesky::xt_m_x(const vnl_vector<double>& x) const
{
  unsigned n=d_.size();
  assert(x.size()==n);
  double sum=0.0;
  const double* xd = x.data_block();
  const double* L_col = L_.data_block();
  unsigned c=n;
  for (unsigned i=0;i<n;++i,++xd,L_col+=(n+1),--c)
  {
    double xLi = dot(L_col,n,xd,c);  // x * i-th column
    sum+= xLi*xLi*d_[i];
  }
  return sum;
}


//: Solve least squares problem M x = b.
//  The right-hand-side std::vector x may be b,
//  which will give a fractional increase in speed.
void vnl_ldl_cholesky::solve(vnl_vector<double> const& b,
                             vnl_vector<double>* xp) const
{
  assert(b.size() == d_.size());
  *xp = b;
  inplace_solve(xp->data_block());
}

//: Solve least squares problem M x = b.
vnl_vector<double> vnl_ldl_cholesky::solve(vnl_vector<double> const& b) const
{
  assert(b.size() == L_.columns());

  vnl_vector<double> ret = b;
  solve(b,&ret);
  return ret;
}

//: Compute determinant.
double vnl_ldl_cholesky::determinant() const
{
  unsigned n=d_.size();
  double det=1.0;
  for (unsigned i=0;i<n;++i) det*=d_[i];
  return det;
}

//: Compute rank-1 update, ie the decomposition of (M+v.v')
//  If the initial state is the decomposition of M, then
//  L and D are updated so that on exit  LDL'=M+v.v'
//
//  Uses the algorithm given by Davis and Hager in
//  "Multiple-Rank Modifications of a Sparse Cholesky Factorization",2001.
void vnl_ldl_cholesky::rank1_update(const vnl_vector<double>& v)
{
  unsigned n = d_.size();
  assert(v.size()==n);
  double a = 1.0;
  vnl_vector<double> w=v;  // Workspace, modified as algorithm goes along
  for (unsigned j=0;j<n;++j)
  {
    double a2=a+w[j]*w[j]/d_[j];
    d_[j]*=a2;
    double gamma = w[j]/d_[j];
    d_[j]/=a;
    a=a2;

    for (unsigned p=j+1;p<n;++p)
    {
      w[p]-=w[j]*L_(p,j);
      L_(p,j)+=gamma*w[p];
    }
  }
}

//: Multi-rank update, ie the decomposition of (M+W.W')
//  If the initial state is the decomposition of M, then
//  L and D are updated so that on exit  LDL'=M+W.W'
void vnl_ldl_cholesky::update(const vnl_matrix<double>& W0)
{
  unsigned n = d_.size();
  assert(W0.rows()==n);
  unsigned r = W0.columns();

  vnl_matrix<double> W(W0);  // Workspace
  vnl_vector<double> a(r,1.0),gamma(r);  // Workspace
  for (unsigned j=0;j<n;++j)
  {
    double* Wj = W[j];
    for (unsigned i=0;i<r;++i)
    {
      double a2=a[i]+Wj[i]*Wj[i]/d_[j];
      d_[j]*=a2;
      gamma[i]=Wj[i]/d_[j];
      d_[j]/=a[i];
      a[i]=a2;
    }
    for (unsigned p=j+1;p<n;++p)
    {
      double *Wp = W[p];
      double *Lp = L_[p];
      for (unsigned i=0;i<r;++i)
      {
        Wp[i]-=Wj[i]*Lp[j];
        Lp[j]+=gamma[i]*Wp[i];
      }
    }
  }
}

// : Compute inverse.  Not efficient.
vnl_matrix<double> vnl_ldl_cholesky::inverse() const
{
  if (num_dims_rank_def_) {
    std::cerr << "vnl_ldl_cholesky: Calling inverse() on rank-deficient matrix\n";
    return vnl_matrix<double>();
  }

  unsigned int n = d_.size();
  vnl_matrix<double> R(n,n);
  R.set_identity();

  // Set each row to solution of Mx=(unit)
  // Since result should be symmetric, this is OK
  for (unsigned int i=0; i<n; ++i)
    inplace_solve(R[i]);

  return R;
}

