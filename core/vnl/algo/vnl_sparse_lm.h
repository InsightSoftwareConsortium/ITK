// This is core/vnl/algo/vnl_sparse_lm.h
#ifndef vnl_sparse_lm_h_
#define vnl_sparse_lm_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Sparse Levenberg Marquardt nonlinear least squares
// \author Matt Leotta (Brown)
// \date   April 14, 2005
//
// \verbatim
//  Modifications
//   Mar 15, 2010  MJL - Modified to handle 'c' parameters (globals)
// \endverbatim
//

#include <iosfwd>
#include <vector>
#include <vcl_compiler.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_nonlinear_minimizer.h>

#include <vnl/algo/vnl_algo_export.h>

class vnl_sparse_lst_sqr_function;

//: Sparse Levenberg Marquardt nonlinear least squares
//  Unlike vnl_levenberg_marquardt this does not use the MINPACK routines.
//  This class implements sparse Levenberg Marquardt as described in
//  the Hartley and Zisserman "Multiple View Geometry" book and further
//  described in a technical report on sparse bundle adjustment available
//  at http://www.ics.forth.gr/~lourakis/sba
class VNL_ALGO_EXPORT vnl_sparse_lm : public vnl_nonlinear_minimizer
{
 public:

  //: Initialize with the function object that is to be minimized.
  vnl_sparse_lm(vnl_sparse_lst_sqr_function& f);

  //: Destructor
  ~vnl_sparse_lm();

  //: Minimize the function supplied in the constructor until convergence or failure.
  //  On return, a, b, and c are such that f(a,b,c) is the lowest value achieved.
  //  Returns true for convergence, false for failure.
  //  If use_gradient is set to false, a finite difference approximation will be used,
  //  even if the Jacobian functions have been provided.
  //  If use_weights is set to false, weights will not be computed even if a
  //  weighting function has been provided.
  bool minimize(vnl_vector<double>& a,
                vnl_vector<double>& b,
                vnl_vector<double>& c,
                bool use_gradient = true,
                bool use_weights = true);

  // Coping with failure-------------------------------------------------------

  //: Provide an ASCII diagnosis of the last minimization on std::ostream.
  void diagnose_outcome(/*std::cerr*/) const;
  void diagnose_outcome(std::ostream&) const;

  //: Return J'*J computed at last minimum.
  //  it is an approximation of inverse of covariance
  vnl_matrix<double> const& get_JtJ();

  //: Access the final weights after optimization
  const vnl_vector<double>& get_weights() const { return weights_; }

protected:

  //: used to compute the initial damping
  double tau_;
  //: the function to minimize
  vnl_sparse_lst_sqr_function* f_;

  vnl_matrix<double> inv_covar_;
  bool set_covariance_; // Set if covariance_ holds J'*J

  void init(vnl_sparse_lst_sqr_function* f);

private:

  //: allocate matrix memory by setting all the matrix sizes
  void allocate_matrices();

  //: check vector sizes and verify that they match the problem size
  bool check_vector_sizes(vnl_vector<double> const& a,
                          vnl_vector<double> const& b,
                          vnl_vector<double> const& c);

  //: compute the blocks making up the the normal equations: Jt J d = Jt e
  void compute_normal_equations();

  //: extract the vector on the diagonal of Jt J
  vnl_vector<double> extract_diagonal() const;

  //: set the vector on the diagonal of Jt J
  void set_diagonal(const vnl_vector<double>& diag);

  //: compute all inv(Vi) and Yij
  void compute_invV_Y();

  //: compute Z and Sa
  void compute_Z_Sa(vnl_matrix<double>& Sa);

  //: compute Ma
  void compute_Ma(const vnl_matrix<double>& H);

  //: compute Mb
  void compute_Mb();

  //: solve for dc
  void solve_dc(vnl_vector<double>& dc);

  //: compute sea using ea, Z, dc, Y, and eb
  void compute_sea(vnl_vector<double> const& dc,
                   vnl_vector<double>& sea);

  //: compute Sa and sea
  // only used when size_c_ == 0
  void compute_Sa_sea(vnl_matrix<double>& Sa, vnl_vector<double>& sea);

  //: back solve to find db using da and dc
  void backsolve_db(vnl_vector<double> const& da,
                    vnl_vector<double> const& dc,
                    vnl_vector<double>& db);

  const int num_a_;
  const int num_b_;
  const int num_e_;
  const int num_nz_;

  const int size_a_;
  const int size_b_;
  const int size_c_;
  const int size_e_;

  //: Storage for each of the Jacobians A_ij, B_ij, and C_ij
  std::vector<vnl_matrix<double> > A_;
  std::vector<vnl_matrix<double> > B_;
  std::vector<vnl_matrix<double> > C_;

  //: Storage for normal equation blocks
  // diagonals of JtJ
  std::vector<vnl_matrix<double> > U_;
  std::vector<vnl_matrix<double> > V_;
  vnl_matrix<double>              T_;
  // off-diagonals of JtJ
  std::vector<vnl_matrix<double> > W_;
  std::vector<vnl_matrix<double> > R_;
  std::vector<vnl_matrix<double> > Q_;
  // vectors Jte
  vnl_vector<double> ea_;
  vnl_vector<double> eb_;
  vnl_vector<double> ec_;

  // Storage for residual vector
  vnl_vector<double> e_;

  // Storage for weight vector
  vnl_vector<double> weights_;

  // Storage for intermediate results
  std::vector<vnl_matrix<double> > inv_V_;
  std::vector<vnl_matrix<double> > Y_;
  std::vector<vnl_matrix<double> > Z_;
  std::vector<vnl_matrix<double> > Ma_;
  std::vector<vnl_matrix<double> > Mb_;

};


#endif // vnl_sparse_lm_h_
