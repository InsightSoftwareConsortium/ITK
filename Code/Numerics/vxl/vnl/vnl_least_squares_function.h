#ifndef vnl_least_squares_function_h_
#define vnl_least_squares_function_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_least_squares_function - Many-to-many function
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_least_squares_function.h
// .FILE	vnl_least_squares_function.cxx
//
// .SECTION Description
//    vnl_least_squares_function is an abstract base for functions to be minimized
//    by an optimizer.  To define your own function to be minimized, subclass
//    from vnl_least_squares_function, and implement the pure virtual f (and
//    optionally grad_f).
//
//    Whether or not f ought to be const is a problem.  Clients might well
//    want to cache some information during the call, and if they're compute
//    objects, will almost certainly be writing to members during the
//    computation.  For the moment it's non-const, but we'll see...
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 31 Aug 96
//
// .SECTION Modifications:
//     280697 AWF Changed return type of f from double to void, as it wasn't used, and
//                people were going to extra trouble to compute it.
//     20 Apr 1999 FSM
//            Added failure flag so that f() and grad() may signal failure to the caller.
//
#include <vcl_string.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

class vnl_least_squares_function {
public:
  // Constants-----------------------------------------------------------------
  enum  UseGradient {
    no_gradient,
    use_gradient
  };
  bool failure;

// -- Construct vnl_least_squares_function, passing number of parameters
// (unknowns, domain dimension) and number of residuals (range dimension).
// The optional argument should be no_gradient if the gradf function has not
// been implemented.
  vnl_least_squares_function(int number_of_unknowns, int number_of_residuals, UseGradient = use_gradient);

  virtual ~vnl_least_squares_function();

  // Operations----------------------------------------------------------------
  void throw_failure(); // the virtuals may call this to signal a failure.
  void clear_failure(); //

  // Computations--------------------------------------------------------------

// -- The main function.  Given the parameter vector x, compute the vector
// of residuals fx.  Fx has been sized appropriately before the call.
  virtual void f(vnl_vector<double> const & x, vnl_vector<double>& fx) = 0;

// -- Calculate the Jacobian, given the parameter vector x.
  virtual void gradf(vnl_vector<double> const & x, vnl_matrix<double>& jacobian);

// -- Called after each LM iteration to print debugging etc.
  virtual void trace(int iteration, vnl_vector<double> const & x, vnl_vector<double> const & fx);

// -- Compute the rms error at x by calling f and returning the norm of the residual
// vector.
  double rms(vnl_vector<double> const & x);

  // Data Access---------------------------------------------------------------

// -- Return the number of unknowns
  int get_number_of_unknowns() const { return p_; }

// -- Return the number of residuals.
  int get_number_of_residuals() const { return n_; }

// -- Return true if the derived class has indicated that gradf has been implemented
  bool has_gradient() const { return use_gradient_; }

protected:
  // Data Members--------------------------------------------------------------
  int p_;
  int n_;
  bool use_gradient_;
  vcl_string print_x_fmt_;
  vcl_string print_f_fmt_;

  void init(int number_of_unknowns, int number_of_residuals);
};

#endif // vnl_least_squares_function_h_
