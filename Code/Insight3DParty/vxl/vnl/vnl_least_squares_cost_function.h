#ifndef vnl_least_squares_cost_function_h_
#define vnl_least_squares_cost_function_h_
#ifdef __GNUC__
#pragma interface
#endif
//
// .NAME	vnl_least_squares_cost_function - vnl_least_squares_function -> vnl_cost_function adaptor
// .LIBRARY	vnl
// .HEADER	vxl package
// .INCLUDE	vnl/vnl_least_squares_cost_function.h
// .FILE	vnl_least_squares_cost_function.cxx
//
// .SECTION Description
//    An adaptor that converts a vnl_least_squares_function to a vnl_cost_function
//
// .SECTION Author
//     Andrew W. Fitzgibbon, Oxford RRG, 20 Aug 99
//
// .SECTION Modifications
//     990820 AWF Initial version.
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_least_squares_function.h>

class vnl_least_squares_cost_function : public vnl_cost_function {
public:
  vnl_least_squares_cost_function(vnl_least_squares_function* f);

  double f(const vnl_vector<double>& x);

  virtual void gradf(const vnl_vector<double>& x, vnl_vector<double>& gradient);

protected:
  vnl_vector<double> storage_;
  vnl_matrix<double> jacobian_;
  vnl_least_squares_function* f_;
};

#endif // vnl_least_squares_cost_function_h_
