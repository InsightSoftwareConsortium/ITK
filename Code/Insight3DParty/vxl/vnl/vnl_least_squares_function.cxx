#ifdef __GNUC__
#pragma implementation
#endif
// Class: vnl_least_squares_function
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 31 Aug 96

#include <vcl_iostream.h>
#include "vnl_least_squares_function.h"
#include <vcl_cassert.h>

vnl_least_squares_function::vnl_least_squares_function(int number_of_unknowns,
						       int number_of_residuals,
						       UseGradient g)
  : failure(false)
{
  n_ = number_of_residuals;
  p_ = number_of_unknowns;
  assert(n_>=0);
  assert(p_>=0);
  use_gradient_ = (g == use_gradient);
}

void vnl_least_squares_function::init(int number_of_unknowns,
				      int number_of_residuals)
{
  n_ = number_of_residuals;
  p_ = number_of_unknowns;
  assert(n_>=0);
  assert(p_>=0);
}

vnl_least_squares_function::~vnl_least_squares_function()
{
}

void vnl_least_squares_function::throw_failure()
{
  //cerr << "throw_failure()" << endl;
  failure = true;
}

void vnl_least_squares_function::clear_failure()
{
  //cerr << "clear_failure()" << endl;
  failure = false;
}

void vnl_least_squares_function::gradf(vnl_vector<double> const & /*x*/,
				       vnl_matrix<double>       & /*jacobian*/)
{
}

void vnl_least_squares_function::trace(int /* iteration */,
				       vnl_vector<double> const & /*x*/,
				 vnl_vector<double> const & /*fx*/)
{
}

double vnl_least_squares_function::rms(vnl_vector<double> const & x)
{
  vnl_vector<double> fx(n_);
  f(x, fx);
  return fx.rms();
}
