// This is core/vnl/vnl_linear_system.h
#ifndef vnl_linear_system_h_
#define vnl_linear_system_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Abstraction for a linear system of equations.
// \author David Capel, capes@robots
// \date   July 2000
//
// \verbatim
//  Modifications
//   LSB (Manchester) 23/1/01 Documentation tidied
// \endverbatim

#include <vnl/vnl_vector.h>

//: Abstraction for a linear system of equations.
//    vnl_linear_system provides an abstraction for a linear system
//    of equations, Ax = b, to be solved by one of the iterative linear
//    solvers. Access to the systems is via the pure virtual methods
//    multiply() and transpose_multiply(). This procedural access scheme
//    makes it possible to solve very large, sparse systems which it would
//    be inefficient to store in matrix form.
//
//    To solve the system, use an algorithm like vnl_lsqr.
class vnl_linear_system
{
 public:

  vnl_linear_system(unsigned int number_of_unknowns, unsigned int number_of_residuals) :
    p_(number_of_unknowns), n_(number_of_residuals) {}

  virtual ~vnl_linear_system();

  //: Compute A*x,  putting result in y
  virtual void multiply(vnl_vector<double> const& x, vnl_vector<double>& y) const = 0;

  //: Compute A_transpose * y, putting result in x
  virtual void transpose_multiply(vnl_vector<double> const& y, vnl_vector<double>& x) const = 0;

  //; Put the right-hand side of the system Ax = b into b
  virtual void get_rhs(vnl_vector<double>& b) const = 0;

  //; (Optional) Apply a suitable preconditioner to x.
  // A preconditioner is an approximation of the inverse of A.
  // Common choices are Jacobi (1/diag(A'A)), Gauss-Seidel,
  // and incomplete LU or Cholesky decompositions.
  // The default implementation applies the identity.
  virtual void apply_preconditioner(vnl_vector<double> const& x, vnl_vector<double>& px) const;

  //: Return the number of unknowns
  unsigned int get_number_of_unknowns() const { return p_; }

  //: Return the number of residuals.
  unsigned int get_number_of_residuals() const { return n_; }

  //: Compute rms error for parameter vector x
  double get_rms_error(vnl_vector<double> const& x) const;

  //: Compute relative residual (|Ax - b| / |b| )for parameter vector x
  double get_relative_residual(vnl_vector<double> const& x) const;

 protected:
  unsigned int p_;
  unsigned int n_;
};

#endif // vnl_linear_system_h_
