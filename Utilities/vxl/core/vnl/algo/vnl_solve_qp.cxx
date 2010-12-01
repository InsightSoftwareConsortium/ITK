#include "vnl_solve_qp.h"
//:
// \file
// \brief Functions to solve various forms of constrained quadratic programming
// \author  Tim Cootes

#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_cholesky.h>
#include <vcl_vector.h>
#include <vcl_cassert.h>
#include <vcl_iostream.h>

//: Solve Sx=b for symmetric S
static void vnl_solve_symmetric_le(const vnl_matrix<double>& S,
                                   const vnl_vector<double>& b,
                                   vnl_vector<double>& x)
{
  vnl_cholesky chol(S,vnl_cholesky::estimate_condition);
  if (chol.rcond()>1e-8) x=chol.solve(b);
  else
  {
    vnl_svd<double> svd(S);
    x=svd.solve(b);
  }
}

//: Solve quadratic programming problem with linear constraints
//  Minimise F(x)=0.5x'Hx + g'x  subject to Ax=b
//  \param H Hessian of F(x) - must be symmetric
//  \retval True if successful
bool vnl_solve_qp_with_equality_constraints(const vnl_matrix<double>& H,
                                            const vnl_vector<double>& g,
                                            const vnl_matrix<double>& A,
                                            const vnl_vector<double>& b,
                                            vnl_vector<double>& x)
{
  // Test inputs
  // unsigned n=H.rows();   // Number of unknowns
  unsigned nc=A.rows();  // Number of equality constraints
  assert(H.cols()==H.rows());
  assert(g.size()==H.rows());
  assert(A.cols()==H.rows());
  assert(b.size()==nc);

  vnl_matrix<double> H_inv;
  vnl_cholesky Hchol(H,vnl_cholesky::estimate_condition);
  if (Hchol.rcond()>1e-8) H_inv=Hchol.inverse();
  else
  {
    vnl_svd<double> Hsvd(H);
    H_inv=Hsvd.inverse();
  }

  if (nc==0)
  {
    // Unconstrained case
    x=-1.0*H_inv*g;
    return true;
  }

  vnl_vector<double> b1=(b+A*H_inv*g)*-1.0;

  // Solve for lagrange multipliers, lambda
  vnl_vector<double> lambda;  // Lagrange multipliers
  vnl_matrix<double> AHA = A*H_inv*A.transpose();
  vnl_solve_symmetric_le(AHA,b1,lambda);

  x=(H_inv*(g+A.transpose()*lambda))*-1.0;
  return true;
}

//: Solve quadratic programming problem with constraint sum(x)=0
//  Minimise F(x)=0.5x'Hx + g'x  subject to sum(x)=0
//  Special case of quadratic programming  (Equality constraint: x.1=0)
//  \param H Hessian of F(x) - must be symmetric
//  \retval True if successful
bool vnl_solve_qp_zero_sum(const vnl_matrix<double>& H,
                           const vnl_vector<double>& g,
                           vnl_vector<double>& x)
{
  // Test inputs
  // unsigned n=H.rows();   // Number of unknowns
  assert(H.cols()==H.rows());
  assert(g.size()==H.rows());

  vnl_matrix<double> H_inv;
  vnl_cholesky Hchol(H,vnl_cholesky::estimate_condition);
  if (Hchol.rcond()>1e-8) H_inv=Hchol.inverse();
  else
  {
    vnl_svd<double> Hsvd(H);
    H_inv=Hsvd.inverse();
  }

  double b1=-1.0*(H_inv*g).sum();

  // Sum of elements in H_inv  (= 1'*H_inv*1)
  double H_inv_sum = vnl_c_vector<double>::sum(H_inv.begin(),H_inv.size());

  if (vcl_fabs(H_inv_sum)<1e-8)
  {
    vcl_cerr<<"Uh-oh. H_inv.sum()="<<H_inv_sum<<vcl_endl
            <<"H="<<H<<vcl_endl
            <<"H_inv="<<H_inv<<vcl_endl;
  }

  // Solve for lagrange multiplier, lambda
  double lambda = b1/H_inv_sum;

  vnl_vector<double> g1(g);
  g1+=lambda;

  x=(H_inv*g1);
  x*=-1.0;

  return true;
}

//: Update x, checking inequality constraints and modifying valid where necessary
static bool vnl_solve_qp_update_x(vnl_vector<double>& x,
                                  const vnl_vector<double>& x1,
                                  vnl_vector<double>& dx,
                                  vcl_vector<bool>& valid,
                                  unsigned& n_valid)
{
  unsigned n=x.size();
  // Check non-negativity constraints
  int worst_i=-1;
  double min_alpha=1.0;
  for (unsigned i=0;i<n_valid;++i)
  {
    if (dx[i]<0.0)
    {
      double alpha = -1.0*x1[i]/dx[i];
      if (alpha<min_alpha)
      {
        min_alpha=alpha; worst_i=i;
      }
    }
  }

  // Update x and apply constraints
  unsigned i1=0;
  for (unsigned i=0;i<n;++i)
  {
    if (valid[i])
    {
      x[i]+=min_alpha*dx[i1];
      if (i1==(unsigned int)worst_i)
      {
        // Set this variable to zero and indicate it cannot change
        x[i]=0.0;
        valid[i]=false;
        n_valid--;
      }
      ++i1;
    }
  }

  return worst_i<0;
}

//: Solve unconstrained problem and apply one extra constraint if necessary
//  Used by vnl_non_neg_constrained_qp
//  Returns true if valid minimum found
bool vnl_solve_qp_non_neg_step(const vnl_matrix<double>& H,
                               const vnl_vector<double>& g,
                               const vnl_matrix<double>& A,
                               const vnl_vector<double>& b,
                               vnl_vector<double>& x,
                               vcl_vector<bool>& valid,
                               unsigned& n_valid)
{
  // Find solution to H1(x+dx)+g1=0, subject to A1(x1+dx)=b
  // H1,A1,g1,x1 contain subsets defined by valid array
  // ie solve H1.dx+(H1.x+g1)=0 subject to A1.dx=(b-A1.x1)

  unsigned n=H.rows();   // Full number of unknowns
  unsigned nc=A.rows();  // Number of equality constraints

  vnl_matrix<double> H1(n_valid,n_valid);
  vnl_matrix<double> A1(nc,n_valid);
  unsigned j1=0;
  for (unsigned j=0;j<n;++j)
  {
    if (valid[j])
    {
      // Fill column j1 of H with elements from column j of H
      // First from H:
      unsigned i1=0;
      for (unsigned i=0;i<n;++i)
      {
        if (valid[i]) { H1(i1,j1)=H(i,j); ++i1; }
      }

      // Now fill column of A1
      for (unsigned i=0;i<nc;++i,++i1) A1(i,j1)=A(i,j);

      ++j1;  // Move to next column in M
    }
  }

  vnl_vector<double> x1(n_valid);  // Will contain non-zero elements of x
  vnl_vector<double> g1(n_valid);
  unsigned i1=0;
  for (unsigned i=0;i<n;++i)
  {
    if (valid[i]) { g1[i1]=g[i]; x1[i1]=x[i]; ++i1; }
  }
  g1 += H1*x1;

  vnl_vector<double> b1(b);
  b1-= A1*x1;

  vnl_vector<double> dx(n_valid,0.0);

  vnl_solve_qp_with_equality_constraints(H1,g1,A1,b1,dx);

  // Update x, checking inequality constraints and modifying valid where necessary
  return vnl_solve_qp_update_x(x,x1,dx,valid,n_valid);
}


//: Solve unconstrained problem and apply one extra constraint if necessary
//  Returns true if valid minimum found
bool vnl_solve_qp_non_neg_sum_one_step(const vnl_matrix<double>& H,
                                       const vnl_vector<double>& g,
                                       vnl_vector<double>& x,
                                       vcl_vector<bool>& valid,
                                       unsigned& n_valid)
{
  // Find solution to H1(x+dx)+g1=0, subject to sum(dx)=0.0
  // H1,g1,x1 contain subsets defined by valid array
  // ie solve H1.dx+(H1.x+g1)=0 subject to sum(dx)=0

  unsigned n=H.rows();   // Full number of unknowns

  vnl_matrix<double> H1(n_valid,n_valid);
  unsigned j1=0;
  for (unsigned j=0;j<n;++j)
  {
    if (valid[j])
    {
      // Fill column j1 of H with elements from column j of H
      // First from H:
      unsigned i1=0;
      for (unsigned i=0;i<n;++i)
      {
        if (valid[i]) { H1(i1,j1)=H(i,j); ++i1; }
      }
      ++j1;  // Move to next column in M
    }
  }

  vnl_vector<double> x1(n_valid);  // Will contain non-zero elements of x
  vnl_vector<double> g1(n_valid);
  unsigned i1=0;
  for (unsigned i=0;i<n;++i)
  {
    if (valid[i]) { g1[i1]=g[i]; x1[i1]=x[i]; ++i1; }
  }
  g1 += H1*x1;

  vnl_vector<double> dx(n_valid,0.0);

  vnl_solve_qp_zero_sum(H1,g1,dx);

  // Update x, checking inequality constraints and modifying valid where necessary
  return vnl_solve_qp_update_x(x,x1,dx,valid,n_valid);
}


//: Find non-negative solution to a constrained quadratic programming problem
//  Minimise F(x)=0.5x'Hx + g'x  subject to Ax=b and x(i)>=0 for all i
//
//  Uses a variant of the active set strategy to solve the problem.
//  This performs a sequence of unconstrained solutions.  If the inequality
//  constraints are violated, the most violated x(i) is set to zero
//  and a slightly smaller problem is solved.
//  \param H Hessian of F(x) - must be symmetric
//  \param x On input, it must satisfy all constraints (Ax=b, x(i)>=0)
//  \param con_tol Tolerance for testing constraints:   |Ax-b|^2<con_tol
//  \param verbose When true, output error messages to cerr if failed
//  \retval True if successful
bool vnl_solve_qp_with_non_neg_constraints(const vnl_matrix<double>& H,
                                           const vnl_vector<double>& g,
                                           const vnl_matrix<double>& A,
                                           const vnl_vector<double>& b,
                                           vnl_vector<double>& x,
                                           double con_tol,
                                           bool verbose)
{
  // Test inputs
  unsigned n=H.rows();   // Number of unknowns
  //unsigned nc=A.rows();  // Number of equality constraints
  assert(H.cols()==n);
  assert(g.size()==n);
  assert(A.cols()==n);
  assert(b.size()==A.rows());

  if (vnl_vector_ssd(A*x,b)>con_tol)
  {
    if (verbose)
      vcl_cerr<<"Supplied x does not satisfy equality constraints\n";
    return false;
  }
  for (unsigned i=0;i<n;++i)
  {
    if (x[i]<0)
    {
      if (verbose)
        vcl_cerr<<"Element "<<i<<" of x is negative.  Must be >=0 on input.\n";
      return false;
    }
  }

  // Indicate which elements of x are non-zero and to be optimised
  vcl_vector<bool> valid(n,true);
  unsigned n_valid=n;

  while (!vnl_solve_qp_non_neg_step(H,g,A,b,x,valid,n_valid)) {}

  if (vnl_vector_ssd(A*x,b)>con_tol)
  {
    if (verbose)
      vcl_cerr<<"Oops: Final x does not satisfy equality constraints\n";
    return false;
  }
  else
    return true;
}

//: Find non-negative solution to a constrained quadratic programming problem
//  Minimise F(x)=0.5x'Hx + g'x  subject to sum(x)=1 and x(i)>=0 for all i
//
//  Uses a variant of the active set strategy to solve the problem.
//  This performs a sequence of unconstrained solutions.  If the inequality
//  constraints are violated, the most violated x(i) is set to zero
//  and a slightly smaller problem is solved.
//  \param H Hessian of F(x) - must be symmetric
//  \param x On input, it must satisfy all constraints (sum(x)=1, x(i)>=0)
//  \param verbose When true, output error messages to cerr if failed
//  \retval True if successful
bool vnl_solve_qp_non_neg_sum_one(const vnl_matrix<double>& H,
                                  const vnl_vector<double>& g,
                                  vnl_vector<double>& x,
                                  bool verbose)
{
  // Test inputs
  unsigned n=H.rows();   // Number of unknowns
  assert(H.cols()==n);
  assert(g.size()==n);

  if (vcl_fabs(x.sum()-1.0)>1e-8)
  {
    if (verbose)
      vcl_cerr<<"Supplied x does not sum to unity.\n";
    return false;
  }
  for (unsigned i=0;i<n;++i)
  {
    if (x[i]<0)
    {
      if (verbose)
        vcl_cerr<<"Element "<<i<<" of x is negative.  Must be >=0 on input.\n";
      return false;
    }
  }

  // Indicate which elements of x are non-zero and to be optimised
  vcl_vector<bool> valid(n,true);
  unsigned n_valid=n;

  while (!vnl_solve_qp_non_neg_sum_one_step(H,g,x,valid,n_valid)) {}

  if (vcl_fabs(x.sum()-1.0)>1e-8)
  {
    if (verbose)
      vcl_cerr<<"Oops. Final x does not sum to unity.\n";
    return false;
  }
  else
    return true;
}
