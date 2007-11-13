// This is core/vnl/algo/vnl_lbfgsb.h
#ifndef vnl_lbfgsb_h_
#define vnl_lbfgsb_h_
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma interface
#endif
//:
// \file
// \brief Limited memory Broyden Fletcher Goldfarb Shannon constrained opt
// \author Brad King, Kitware Inc.
// \date   28 Aug 07
//
// \verbatim
// Modifications
//  070828 BJK Initial version.
// \endverbatim
//

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_nonlinear_minimizer.h>
#include <vnl/vnl_vector.h>

//: Limited memory Broyden Fletcher Goldfarb Shannon minimization with constraints.
//  Lower and upper bounds may be specified for the variables to be optimized.
//  The algorithm miminizes a nonlinear function f(x) of n variables
//  subject to simple bound constraints of l <= x <= u.

class vnl_lbfgsb : public vnl_nonlinear_minimizer
{
 public:
  vnl_lbfgsb();
  vnl_lbfgsb(vnl_cost_function& f);

  //: Find a minimum in the feasible region given an initial guess.
  // Returns true if a minimum is found and false for failure.
  bool minimize(vnl_vector<double>& x);

  //: Set the bounds to be enforced on each variable.
  // The argument should have one entry per unknown.
  // Each entry may have one of these values:
  //   0 - variable is not constrainted
  //   1 - variable has only a lower bound
  //   2 - variable has both lower and upper bounds
  //   3 - variable has only an upper bound
  void set_bound_selection(vnl_vector<long> const& nbd)
    { this->bound_selection_ = nbd; }

  //: Get the bounds currently enforced on each variable.
  void get_bound_selection(vnl_vector<long>& nbd) const
    { nbd = this->bound_selection_; }

  //: Set the lower bounds for all variables.
  // The argument should have one entry per unknown.
  // The lower bound is used only if the corresponding entry
  // in the bound selection vector is set to 1 or 2.
  void set_lower_bound(vnl_vector<double> const& l)
    { this->lower_bound_ = l; }

  //: Get the lower bounds for all variables.
  void get_lower_bound(vnl_vector<double>& l) const
    { l = this->lower_bound_; }

  //: Set the upper bounds for all variables.
  // The argument should have one entry per unknown.
  // The upper bound is used only if the corresponding entry
  // in the bound selection vector is set to 2 or 3.
  void set_upper_bound(vnl_vector<double> const& u)
    { this->upper_bound_ = u; }

  //: Get the upper bounds for all variables.
  void get_upper_bound(vnl_vector<double>& u) const
    { u = this->upper_bound_; }

  //: Set the maximum number of variable metric corrections.
  // This is used to determine the size of the limited-memory matrix.
  // The default value is 5.
  void set_max_variable_metric_corrections(long m)
    { this->max_corrections_ = m; }

  //: Get the maximum number of variable metric corrections.
  long get_max_variable_metric_corrections() const
    { return this->max_corrections_; }

  //: Set the cost function convergence factor.
  // When an iteration changes the function value by an amount smaller than
  // this factor times the machine epsilon (scaled by function magnitude)
  // convergence is assumed.  The default value is 1e+7.
  void set_cost_function_convergence_factor(double factor)
    { this->convergence_factor_ = factor; }

  //: Get the cost function convergence factor.
  double get_cost_function_convergence_factor() const
    { return this->convergence_factor_; }

  //: Set the projected gradient tolerance.
  // When the projected gradient vector has no component larger than
  // the given value convergence is assumed.  The default value is
  // 1e-5.
  void set_projected_gradient_tolerance(double tol)
    { this->projected_gradient_tolerance_ = tol; }

  //: Get the projected gradient tolerance.
  double get_projected_gradient_tolerance() const
    { return this->projected_gradient_tolerance_; }

  //: Get the current infinity norm of the projected gradient.
  double get_inf_norm_projected_gradient() const
    { return this->inf_norm_projected_gradient_; }

 protected:

  vnl_vector<double> lower_bound_;
  vnl_vector<double> upper_bound_;
  vnl_vector<long> bound_selection_;
  long max_corrections_;
  double convergence_factor_;
  double projected_gradient_tolerance_;
  double inf_norm_projected_gradient_;

 private:
  void init_parameters();
  vnl_cost_function* f_;
};

#endif // vnl_lbfgsb_h_
