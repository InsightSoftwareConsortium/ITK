// This is core/vnl/vnl_sparse_lst_sqr_function.h
#ifndef vnl_sparse_lst_sqr_function_h_
#define vnl_sparse_lst_sqr_function_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Abstract base for sparse least squares functions
// \author Matt Leotta (Brown)
// \date   April 13, 2005
//
// \verbatim
//  Modifications
//   Apr 13, 2005  MJL - Modified from vnl_least_squares_function
// \endverbatim
//
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_crs_index.h>

//: Abstract base for sparse least squares functions.
//    vnl_sparse_lst_sqr_function is an abstract base for functions to be minimized
//    by an optimizer.  To define your own function to be minimized, subclass
//    from vnl_sparse_lst_sqr_function, and implement the pure virtual f (and
//    optionally grad_f).
//
//    This differs from a vnl_least_squares_function in that many entries in the
//    Jacobian are known to be zero, and we don't want to compute them.  The particular
//    sparse structure is that described in Hartley and Zisserman section A4.3.  It
//    is assumed that the parameter vector can be partitioned into sets A and B.
//    These are further partitioned into subsets {a_1, a_2, ... a_m} and
//    {b_1, b_2, ... b_n}.  Likewise, the residual vector X is partitioned into
//    {x_11, x_12, ... x_mn} (not all x_ij are required).  We further assume that
//    dx_ij/da_k = 0 for all i != k and dx_ij/db_k = 0 for all j != k.  An example
//    of this is bundle adjustment where each a_i is the parameters for one of m cameras,
//    each b_j is the parameters of a 3D point, and x_ij is the projection error of
//    the jth point by the ith camera.
class vnl_sparse_lst_sqr_function
{
 public:
  enum  UseGradient {
    no_gradient,
    use_gradient
  };
  bool failure;

  //: Construct vnl_sparse_lst_sqr_function.
  // Assumes A consists of \p num_a parameters each of size \p num_params_per_a
  // Assumes B consists of \p num_b parameters each of size \p num_params_per_b
  // Assumes there is a residual x_ij for all i and j, each of size \p num_residuals_per_e
  // The optional argument should be no_gradient if the gradf function has not
  // been implemented.  Default is use_gradient.
  vnl_sparse_lst_sqr_function(unsigned int num_a,
                              unsigned int num_params_per_a,
                              unsigned int num_b,
                              unsigned int num_params_per_b,
                              unsigned int num_residuals_per_e,
                              UseGradient g = use_gradient);

  //: Construct vnl_sparse_lst_sqr_function.
  // Assumes A consists of \p num_a parameters each of size \p num_params_per_a
  // Assumes B consists of \p num_b parameters each of size \p num_params_per_b
  // \p xmask is a mask for residual availability.  residual e_ij exists only if mask[i][j]==true
  // Assumes each available residual has size \p num_residuals_per_e
  // The optional argument should be no_gradient if the gradf function has not
  // been implemented.  Default is use_gradient.
  vnl_sparse_lst_sqr_function(unsigned int num_a,
                              unsigned int num_params_per_a,
                              unsigned int num_b,
                              unsigned int num_params_per_b,
                              const vcl_vector<vcl_vector<bool> >& xmask,
                              unsigned int num_residuals_per_e,
                              UseGradient g = use_gradient);

  //: Construct vnl_sparse_lst_sqr_function.
  // This constructor is the most general
  // \param a_sizes is a vector describing the number of parameters for each a_i
  // \param b_sizes is a vector describing the number of parameters for each b_j
  // \param e_sizes is a vector describing the number of parameters for each residual e_ij
  // \param xmask is a mask for residual availability.  residual e_ij exists only if mask[i][j]==true
  // xmask must be a_sizes.size() by b_sizes.size() and contain e_sizes.size() true entries
  // The optional argument should be no_gradient if the gradf function has not
  // been implemented.  Default is use_gradient.
  vnl_sparse_lst_sqr_function(const vcl_vector<unsigned int>& a_sizes,
                              const vcl_vector<unsigned int>& b_sizes,
                              const vcl_vector<unsigned int>& e_sizes,
                              const vcl_vector<vcl_vector<bool> >& xmask,
                              UseGradient g = use_gradient);

  virtual ~vnl_sparse_lst_sqr_function() {}

  // the virtuals may call this to signal a failure.
  void throw_failure() { failure = true; }
  void clear_failure() { failure = false; }

  //: Compute all residuals.
  //  Given the parameter vectors a and b, compute the vector of residuals f.
  //  f has been sized appropriately before the call.
  //  The default implementation computes f by calling fij for each valid
  //  pair of i and j.  You do not need to overload this method unless you
  //  want to provide a more efficient implementation for your problem.
  virtual void f(vnl_vector<double> const& a, vnl_vector<double> const& b,
                 vnl_vector<double>& f);

  //: Compute the sparse Jacobian in block form.
  //  Given the parameter vectors a and b, compute the set of block Jacobians Aij and Bij.
  //  All Aij and Bij have been sized appropriately before the call.
  //  The default implementation computes A and B by calling jac_Aij and jac_Bij
  //  for each valid pair of i and j.  You do not need to overload this method
  //  unless you want to provide a more efficient implementation for your problem.
  virtual void jac_blocks(vnl_vector<double> const& a, vnl_vector<double> const& b,
                          vcl_vector<vnl_matrix<double> >& A,
                          vcl_vector<vnl_matrix<double> >& B);

  //: Compute the sparse Jacobian in block form using a finite difference approximation.
  //  Given the parameter vectors a and b, compute the set of block Jacobians Aij and Bij.
  //  The finite difference approximation is done independently at each block
  //  All Aij and Bij have been sized appropriately before the call.
  //  The default implementation computes A and B by calling jac_Aij and jac_Bij
  //  for each valid pair of i and j.  You do not need to overload this method
  //  unless you want to provide a more efficient implementation for your problem.
  virtual void fd_jac_blocks(vnl_vector<double> const& a, vnl_vector<double> const& b,
                             vcl_vector<vnl_matrix<double> >& A,
                             vcl_vector<vnl_matrix<double> >& B,
                             double stepsize);

  //: Compute the residuals from the ith component of a and the jth component of b.
  //  Given the parameter vectors ai and bj, compute the vector of residuals fij.
  //  fij has been sized appropriately before the call.
  virtual void fij(int i, int j, vnl_vector<double> const& ai,
                   vnl_vector<double> const& bj, vnl_vector<double>& fij);

  //: Calculate the Jacobian A_ij, given the parameter vectors a_i and b_j.
  virtual void jac_Aij(int i, int j, vnl_vector<double> const& ai,
                       vnl_vector<double> const& bj, vnl_matrix<double>& Aij);

  //: Calculate the Jacobian B_ij, given the parameter vectors a_i and b_j.
  virtual void jac_Bij(int i, int j, vnl_vector<double> const& ai,
                       vnl_vector<double> const& bj, vnl_matrix<double>& Bij);

  //: Use this to compute a finite-difference Jacobian A_ij
  void fd_jac_Aij(int i, int j, vnl_vector<double> const& ai,
                  vnl_vector<double> const& bj, vnl_matrix<double>& Aij,
                  double stepsize);

  //: Use this to compute a finite-difference Jacobian B_ij
  void fd_jac_Bij(int i, int j, vnl_vector<double> const& ai,
                  vnl_vector<double> const& bj, vnl_matrix<double>& Bij,
                  double stepsize);

  //: Called after each LM iteration to print debugging etc.
  virtual void trace(int iteration,
                     vcl_vector<vnl_vector<double> > const& a,
                     vcl_vector<vnl_vector<double> > const& b,
                     vcl_vector<vnl_vector<double> > const& e);

  //: Return the number of parameters of a_j
  unsigned int number_of_params_a(int i) const { return indices_a_[i+1]-indices_a_[i]; }

  //: Return the number of parameters of b_i
  unsigned int number_of_params_b(int j) const { return indices_b_[j+1]-indices_b_[j]; }

  //: Return the number of residuals in the kth residual vector.
  unsigned int number_of_residuals(int k) const { return indices_e_[k+1]-indices_e_[k]; }

  //: Return the number of residuals for x_ij.
  unsigned int number_of_residuals(int i, int j) const
  { int k = residual_indices_(i,j);
    if (k<0) return 0;
    else return number_of_residuals(k); }

  //: return the index of aj in a
  unsigned int index_a(int i) const { return indices_a_[i]; }

  //: return the index of bj in b
  unsigned int index_b(int j) const { return indices_b_[j]; }

  //: return the index of bj in b
  unsigned int index_e(int k) const { return indices_e_[k]; }

  //: Return the number of subsets in \p a
  unsigned int number_of_a() const { return indices_a_.size()-1; }

  //: Return the number of subsets in \p b
  unsigned int number_of_b() const { return indices_b_.size()-1; }

  //: Return the number of residual vectors
  unsigned int number_of_e() const { return indices_e_.size()-1; }

  //: Return true if the derived class has indicated that gradf has been implemented
  bool has_gradient() const { return use_gradient_; }

  //: Return a const reference to the residual indexer
  const vnl_crs_index& residual_indices() const { return residual_indices_; }

 protected:
  vnl_crs_index residual_indices_;
  vcl_vector<unsigned int> indices_a_;
  vcl_vector<unsigned int> indices_b_;
  vcl_vector<unsigned int> indices_e_;

  bool use_gradient_;

 private:
  void dim_warning(unsigned int n_unknowns, unsigned int n_residuals);
};

#endif // vnl_sparse_lst_sqr_function_h_
