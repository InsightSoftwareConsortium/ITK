#ifndef vnl_least_squares_cost_function_h_
#define vnl_least_squares_cost_function_h_
#ifdef __GNUC__
#pragma interface
#endif
// This is vxl/vnl/vnl_least_squares_cost_function.h

//: \file
//  \brief vnl_least_squares_function -> vnl_cost_function adaptor
//  \author Andrew W. Fitzgibbon, Oxford RRG, 20 Aug 99

//     Modifications:
//     990820 AWF Initial version.
//     LSB (Manchester) 23/3/01 Tidied documentation
//
//-----------------------------------------------------------------------------

#include <vnl/vnl_cost_function.h>
#include <vnl/vnl_least_squares_function.h>

//: An adaptor that converts a vnl_least_squares_function to a vnl_cost_function
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
