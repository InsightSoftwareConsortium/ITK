// This is core/vnl/vnl_least_squares_function.h
#ifndef vnl_least_squares_function_h_
#define vnl_least_squares_function_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Abstract base for minimising functions
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   31 Aug 96
//
// \verbatim
//  Modifications
//   280697 AWF Changed return type of f from double to void, as it wasn't used, and
//              people were going to extra trouble to compute it.
//   20 Apr 1999 FSM Added failure flag so that f() and grad() may signal failure to the caller.
//   23/3/01 LSB (Manchester) Tidied documentation
//   Feb.2002 - Peter Vanroose - brief doxygen comment placed on single line
// \endverbatim
//
#include <vcl_string.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

//:  Abstract base for minimising functions.
//    vnl_least_squares_function is an abstract base for functions to be minimized
//    by an optimizer.  To define your own function to be minimized, subclass
//    from vnl_least_squares_function, and implement the pure virtual f (and
//    optionally grad_f).
//
//    Whether or not f ought to be const is a problem.  Clients might well
//    want to cache some information during the call, and if they're compute
//    objects, will almost certainly be writing to members during the
//    computation.  For the moment it's non-const, but we'll see...
class vnl_least_squares_function
{
 public:
  enum  UseGradient {
    no_gradient,
    use_gradient
  };
  bool failure;

  //: Construct vnl_least_squares_function.
  // Passing number of parameters (unknowns, domain dimension) and number of
  // residuals (range dimension).
  // The optional argument should be no_gradient if the gradf function has not
  // been implemented.  Default is use_gradient.
  vnl_least_squares_function(unsigned int number_of_unknowns,
                             unsigned int number_of_residuals,
                             UseGradient g = use_gradient)
  : failure(false), p_(number_of_unknowns), n_(number_of_residuals),
    use_gradient_(g == use_gradient)
  { dim_warning(p_,n_); }

  virtual ~vnl_least_squares_function() {}

  // the virtuals may call this to signal a failure.
  void throw_failure() { failure = true; }
  void clear_failure() { failure = false; }

  //: The main function.
  //  Given the parameter vector x, compute the vector of residuals fx.
  //  Fx has been sized appropriately before the call.
  virtual void f(vnl_vector<double> const& x, vnl_vector<double>& fx) = 0;

  //: Calculate the Jacobian, given the parameter vector x.
  virtual void gradf(vnl_vector<double> const& x, vnl_matrix<double>& jacobian);

  //: Use this to compute a finite-difference gradient other than lmdif
  void fdgradf(vnl_vector<double> const& x, vnl_matrix<double>& jacobian,
               double stepsize);

  //: Called after each LM iteration to print debugging etc.
  virtual void trace(int iteration,
                     vnl_vector<double> const& x,
                     vnl_vector<double> const& fx);

  //: Compute the rms error at x by calling f and returning the norm of the residual vector.
  double rms(vnl_vector<double> const& x);

  //: Return the number of unknowns
  unsigned int get_number_of_unknowns() const { return p_; }

  //: Return the number of residuals.
  unsigned int get_number_of_residuals() const { return n_; }

  //: Return true if the derived class has indicated that gradf has been implemented
  bool has_gradient() const { return use_gradient_; }

 protected:
  unsigned int p_;
  unsigned int n_;
  bool use_gradient_;

  void init(unsigned int number_of_unknowns, unsigned int number_of_residuals)
  { p_ = number_of_unknowns; n_ = number_of_residuals; dim_warning(p_,n_); }
 private:
  void dim_warning(unsigned int n_unknowns, unsigned int n_residuals);
};

#endif // vnl_least_squares_function_h_
