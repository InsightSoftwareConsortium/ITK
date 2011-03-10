// This is core/vnl/vnl_sparse_lst_sqr_function.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Matt Leotta (Brown)
// \date   April 13, 2005

#include "vnl_sparse_lst_sqr_function.h"
#include <vcl_iostream.h>
#include <vcl_cassert.h>
#include <vnl/vnl_vector_ref.h>

void vnl_sparse_lst_sqr_function::dim_warning(unsigned int nr_of_unknowns,
                                              unsigned int nr_of_residuals)
{
  if (nr_of_unknowns > nr_of_residuals)
    vcl_cerr << "vnl_sparse_lst_sqr_function: WARNING: "
             << "unknowns(" << nr_of_unknowns << ") > "
             << "residuals("<< nr_of_residuals << ")\n";
}

//: Construct vnl_sparse_lst_sqr_function.
// Assumes A consists of \p num_a parameters each of size \p num_params_per_a
// Assumes B consists of \p num_b parameters each of size \p num_params_per_b
// Assumes there is a residual x_ij for all i and j, each of size \p num_residuals_per_e
// The optional argument should be no_gradient if the gradf function has not
// been implemented.  Default is use_gradient.
vnl_sparse_lst_sqr_function::vnl_sparse_lst_sqr_function(
                                 unsigned int num_a,
                                 unsigned int num_params_per_a,
                                 unsigned int num_b,
                                 unsigned int num_params_per_b,
                                 unsigned int num_residuals_per_e,
                                 UseGradient g)
 : failure(false),
   residual_indices_(),
   indices_a_(num_a+1,0),
   indices_b_(num_b+1,0),
   indices_e_(num_a*num_b+1,0),
   use_gradient_(g == use_gradient)
{
  unsigned int k = num_params_per_a;
  for (unsigned int i=1; i<indices_a_.size(); ++i, k+=num_params_per_a)
    indices_a_[i] = k;

  k = num_params_per_b;
  for (unsigned int i=1; i<indices_b_.size(); ++i, k+=num_params_per_b)
    indices_b_[i] = k;

  k = num_residuals_per_e;
  for (unsigned int i=1; i<indices_e_.size(); ++i, k+=num_residuals_per_e)
    indices_e_[i] = k;
}

//: Construct vnl_sparse_lst_sqr_function.
// Assumes A consists of \p num_a parameters each of size \p num_params_per_a
// Assumes B consists of \p num_b parameters each of size \p num_params_per_b
// \p xmask is a mask for residual availability.  residual e_ij exists only if mask[i][j]==true
// Assumes each available residual has size \p num_residuals_per_e
// The optional argument should be no_gradient if the gradf function has not
// been implemented.  Default is use_gradient.
vnl_sparse_lst_sqr_function::vnl_sparse_lst_sqr_function(
                                 unsigned int num_a,
                                 unsigned int num_params_per_a,
                                 unsigned int num_b,
                                 unsigned int num_params_per_b,
                                 const vcl_vector<vcl_vector<bool> >& xmask,
                                 unsigned int num_residuals_per_e,
                                 UseGradient g)
 : failure(false),
   residual_indices_(xmask),
   indices_a_(num_a+1,0),
   indices_b_(num_b+1,0),
   indices_e_(residual_indices_.num_non_zero()+1,0),
   use_gradient_(g == use_gradient)
{
  unsigned int k = num_params_per_a;
  for (unsigned int i=1; i<indices_a_.size(); ++i, k+=num_params_per_a)
    indices_a_[i] = k;

  k = num_params_per_b;
  for (unsigned int i=1; i<indices_b_.size(); ++i, k+=num_params_per_b)
    indices_b_[i] = k;

  k = num_residuals_per_e;
  for (unsigned int i=1; i<indices_e_.size(); ++i, k+=num_residuals_per_e)
    indices_e_[i] = k;

  dim_warning(num_a*num_params_per_a + num_b*num_params_per_b, k);
}


//: Construct vnl_sparse_lst_sqr_function.
// This constructor is the most general
// \param a_sizes is a vector describing the number of parameters for each a_i
// \param b_sizes is a vector describing the number of parameters for each b_j
// \param e_sizes is a vector describing the number of parameters for each residual e_ij
// \param xmask is a mask for residual availability.  residual e_ij exists only if mask[i][j]==true
// xmask must be a_sizes.size() by b_sizes.size() and contain e_sizes.size() true entries
// The optional argument should be no_gradient if the gradf function has not
// been implemented.  Default is use_gradient.
vnl_sparse_lst_sqr_function::vnl_sparse_lst_sqr_function(
                                 const vcl_vector<unsigned int>& a_sizes,
                                 const vcl_vector<unsigned int>& b_sizes,
                                 const vcl_vector<unsigned int>& e_sizes,
                                 const vcl_vector<vcl_vector<bool> >& xmask,
                                 UseGradient g)
 : failure(false),
   residual_indices_(xmask),
   indices_a_(a_sizes.size()+1,0),
   indices_b_(b_sizes.size()+1,0),
   indices_e_(e_sizes.size()+1,0),
   use_gradient_(g == use_gradient)
{
  assert(residual_indices_.num_non_zero() == (int)e_sizes.size());
  assert(residual_indices_.num_rows() == (int)a_sizes.size());
  assert(residual_indices_.num_cols() == (int)b_sizes.size());

  for (unsigned int i=0; i<a_sizes.size(); ++i)
    indices_a_[i+1] = indices_a_[i]+a_sizes[i];

  for (unsigned int i=0; i<b_sizes.size(); ++i)
    indices_b_[i+1] = indices_b_[i]+b_sizes[i];

  for (unsigned int i=0; i<e_sizes.size(); ++i)
    indices_e_[i+1] = indices_e_[i]+e_sizes[i];

  dim_warning(indices_a_.back() + indices_b_.back(), indices_e_.back());
}


//: Compute all residuals.
//  Given the parameter vectors a and b, compute the vector of residuals e.
//  e has been sized appropriately before the call.
//  The default implementation computes e by calling fij for each valid
//  pair of i and j.  You do not need to overload this method unless you
//  want to provide a more efficient implementation for your problem.
void
vnl_sparse_lst_sqr_function::f(vnl_vector<double> const& a, vnl_vector<double> const& b,
                               vnl_vector<double>& e)
{
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;
  for (unsigned int i=0; i<number_of_a(); ++i)
  {
    // This is semi const incorrect - there is no vnl_vector_ref_const
    const vnl_vector_ref<double> ai(number_of_params_a(i),
                                    const_cast<double*>(a.data_block())+index_a(i));

    vnl_crs_index::sparse_vector row = residual_indices_.sparse_row(i);
    for (sv_itr r_itr=row.begin(); r_itr!=row.end(); ++r_itr)
    {
      unsigned int j = r_itr->second;
      unsigned int k = r_itr->first;
      // This is semi const incorrect - there is no vnl_vector_ref_const
      const vnl_vector_ref<double> bj(number_of_params_b(j),
                                      const_cast<double*>(b.data_block())+index_b(j));
      vnl_vector_ref<double> eij(number_of_residuals(k), e.data_block()+index_e(k));
      fij(i,j,ai,bj,eij);        // compute residual vector e_ij
    }
  }
}


//: Compute the sparse Jacobian in block form.
//  Given the parameter vectors a and b, compute the set of block Jacobians Aij and Bij.
//  All Aij and Bij have been sized appropriately before the call.
//  The default implementation computes A and B by calling jac_Aij and jac_Bij
//  for each valid pair of i and j.  You do not need to overload this method
//  unless you want to provide a more efficient implementation for your problem.
void
vnl_sparse_lst_sqr_function::jac_blocks(vnl_vector<double> const& a, vnl_vector<double> const& b,
                                        vcl_vector<vnl_matrix<double> >& A,
                                        vcl_vector<vnl_matrix<double> >& B)
{
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;
  for (unsigned int i=0; i<number_of_a(); ++i)
  {
    // This is semi const incorrect - there is no vnl_vector_ref_const
    const vnl_vector_ref<double> ai(number_of_params_a(i),
                                    const_cast<double*>(a.data_block())+index_a(i));

    vnl_crs_index::sparse_vector row = residual_indices_.sparse_row(i);
    for (sv_itr r_itr=row.begin(); r_itr!=row.end(); ++r_itr)
    {
      unsigned int j = r_itr->second;
      unsigned int k = r_itr->first;
      // This is semi const incorrect - there is no vnl_vector_ref_const
      const vnl_vector_ref<double> bj(number_of_params_b(j),
                                      const_cast<double*>(b.data_block())+index_b(j));

      jac_Aij(i,j,ai,bj,A[k]);  // compute Jacobian A_ij
      jac_Bij(i,j,ai,bj,B[k]);  // compute Jacobian B_ij
    }
  }
}


//: Compute the sparse Jacobian in block form using a finite difference approximation.
//  Given the parameter vectors a and b, compute the set of block Jacobians Aij and Bij.
//  The finite difference approximation is done independently at each block
//  All Aij and Bij have been sized appropriately before the call.
//  The default implementation computes A and B by calling jac_Aij and jac_Bij
//  for each valid pair of i and j.  You do not need to overload this method
//  unless you want to provide a more efficient implementation for your problem.
void
vnl_sparse_lst_sqr_function::fd_jac_blocks(vnl_vector<double> const& a, vnl_vector<double> const& b,
                                           vcl_vector<vnl_matrix<double> >& A,
                                           vcl_vector<vnl_matrix<double> >& B,
                                           double stepsize)
{
  typedef vnl_crs_index::sparse_vector::iterator sv_itr;
  for (unsigned int i=0; i<number_of_a(); ++i)
  {
    // This is semi const incorrect - there is no vnl_vector_ref_const
    const vnl_vector_ref<double> ai(number_of_params_a(i),
                                    const_cast<double*>(a.data_block())+index_a(i));

    vnl_crs_index::sparse_vector row = residual_indices_.sparse_row(i);
    for (sv_itr r_itr=row.begin(); r_itr!=row.end(); ++r_itr)
    {
      unsigned int j = r_itr->second;
      unsigned int k = r_itr->first;
      // This is semi const incorrect - there is no vnl_vector_ref_const
      const vnl_vector_ref<double> bj(number_of_params_b(j),
                                      const_cast<double*>(b.data_block())+index_b(j));

      fd_jac_Aij(i,j,ai,bj,A[k],stepsize);  // compute Jacobian A_ij with finite differences
      fd_jac_Bij(i,j,ai,bj,B[k],stepsize);  // compute Jacobian B_ij with finite differences
    }
  }
}


//: Compute the residuals from the ith component of a and the jth component of b.
//  Given the parameter vectors ai and bj, compute the vector of residuals fij.
//  fij has been sized appropriately before the call.
void
vnl_sparse_lst_sqr_function::fij(int /*i*/, int /*j*/, vnl_vector<double> const& /*ai*/,
                                 vnl_vector<double> const& /*bj*/, vnl_vector<double>& /*f_i_j*/)
{
  // This default implementation is empty; overloaded in derived class.
  vcl_cerr << "Warning: fij() called but not implemented in derived class\n";
}

//: Calculate the Jacobian A_ij, given the parameter vectors a_j and b_i.
void
vnl_sparse_lst_sqr_function::jac_Aij(int /*i*/, int /*j*/, vnl_vector<double> const& /*ai*/,
                                     vnl_vector<double> const& /*bj*/, vnl_matrix<double>& /*Aij*/)
{
  // This default implementation is empty; overloaded in derived class.
  vcl_cerr << "Warning: jac_Aij() called but not implemented in derived class\n";
}

//: Calculate the Jacobian B_ij, given the parameter vectors a_j and b_i.
void
vnl_sparse_lst_sqr_function::jac_Bij(int /*i*/, int /*j*/, vnl_vector<double> const& /*ai*/,
                                     vnl_vector<double> const& /*bj*/, vnl_matrix<double>& /*Bij*/)
{
  // This default implementation is empty; overloaded in derived class.
  vcl_cerr << "Warning: jac_Bij() called but not implemented in derived class\n";
}

//: Use this to compute a finite-difference Jacobian A_ij
void
vnl_sparse_lst_sqr_function::fd_jac_Aij(int i, int j, vnl_vector<double> const& ai,
                                        vnl_vector<double> const& bj, vnl_matrix<double>& Aij,
                                        double stepsize)
{
  unsigned int dim = ai.size();
  unsigned int n = Aij.rows();
  assert(dim == number_of_params_a(i));
  assert(n == number_of_residuals(i,j));
  assert(dim == Aij.columns());

  vnl_vector<double> tai = ai;
  vnl_vector<double> fplus(n);
  vnl_vector<double> fminus(n);
  // note: i and j are indices for the macro problem
  // while ii and jj are indices for subproblem jacobian Aij
  for (unsigned int ii = 0; ii < dim; ++ii)
  {
    // calculate f just to the right of ai[ii]
    double tplus = tai[ii] = ai[ii] + stepsize;
    this->fij(i,j,tai,bj,fplus);

    // calculate f just to the left of ai[ii]
    double tminus = tai[ii] = ai[ii] - stepsize;
    this->fij(i,j,tai,bj,fminus);

    double h = 1.0 / (tplus - tminus);
    for (unsigned int jj = 0; jj < n; ++jj)
      Aij(jj,ii) = (fplus[jj] - fminus[jj]) * h;

    // restore tai
    tai[ii] = ai[ii];
  }
}


//: Use this to compute a finite-difference Jacobian B_ij
void
vnl_sparse_lst_sqr_function::fd_jac_Bij(int i, int j, vnl_vector<double> const& ai,
                                        vnl_vector<double> const& bj, vnl_matrix<double>& Bij,
                                        double stepsize)
{
  unsigned int dim = bj.size();
  unsigned int n = Bij.rows();
  assert(dim == number_of_params_b(j));
  assert(n == number_of_residuals(i,j));
  assert(dim == Bij.columns());

  vnl_vector<double> tbj = bj;
  vnl_vector<double> fplus(n);
  vnl_vector<double> fminus(n);
  // note: i and j are indices for the macro problem
  // while ii and jj are indices for subproblem jacobian Bij
  for (unsigned int ii = 0; ii < dim; ++ii)
  {
    // calculate f just to the right of bj[ii]
    double tplus = tbj[ii] = bj[ii] + stepsize;
    this->fij(i,j,ai,tbj,fplus);

    // calculate f just to the left of bj[ii]
    double tminus = tbj[ii] = bj[ii] - stepsize;
    this->fij(i,j,ai,tbj,fminus);

    double h = 1.0 / (tplus - tminus);
    for (unsigned int jj = 0; jj < n; ++jj)
      Bij(jj,ii) = (fplus[jj] - fminus[jj]) * h;

    // restore tbj
    tbj[ii] = bj[ii];
  }
}


void vnl_sparse_lst_sqr_function::trace(int /* iteration */,
                                        vcl_vector<vnl_vector<double> > const& /*a*/,
                                        vcl_vector<vnl_vector<double> > const& /*b*/,
                                        vcl_vector<vnl_vector<double> > const& /*e*/)
{
  // This default implementation is empty; overloaded in derived class.
  vcl_cerr << "Warning: trace() called but not implemented in derived class\n";
}

