#ifndef vnl_cost_function_h_
#define vnl_cost_function_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_cost_function.h

//: \file
//  \brief Vector->Real function
//  \author Andrew W. Fitzgibbon, Oxford RRG, 23 Oct 97


//    Modifications
//     971023 AWF Initial version.
//     LSB (Manchester) 26/3/01 Tidied documnetation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_matops.h>
#include <vnl/vnl_unary_function.h>

//:   An object that represents a function from R^n -> R.
//    It is commonly used to express the
//    interface of a minimizer. 
class vnl_cost_function : public vnl_unary_function<double, vnl_vector<double> > {
public:
  vnl_cost_function(int number_of_unknowns):dim(number_of_unknowns) {}

  virtual ~vnl_cost_function() {}

//:  The main function.  Given the parameter vector x, compute the
// value of f(x).
  virtual double f(const vnl_vector<double>& x);

//:  Calculate the Jacobian, given the parameter vector x.
  virtual void gradf(const vnl_vector<double>& x, vnl_vector<double>& gradient);

//:  Compute one or both of f and g.  Normally implemented in terms of the above two,
//  but may be faster if specialized. f != 0 => compute f
  virtual void compute(const vnl_vector<double>& x, double *f, vnl_vector<double>* g);

//:  Return the number of unknowns
  int get_number_of_unknowns() const { return dim; }

//:  Compute finite-difference gradient
  void fdgradf(const vnl_vector<double>& x, vnl_vector<double>& gradient, double stepsize = 1e-5);

//:  Called when error is printed for user.
  virtual double reported_error(double f_value) { return f_value; }

//:  Conveniences for printing grad, fdgrad
  vnl_vector<double> gradf(const vnl_vector<double>& x);
  vnl_vector<double> fdgradf(const vnl_vector<double>& x);

public:
  int dim;
};


#endif // vnl_cost_function_h_
