// This is vxl/vnl/vnl_least_squares_function.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   31 Aug 96

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

  if (number_of_unknowns > number_of_residuals) {
    vcl_cerr << "vnl_least_squares_function: WARNING: "
      "unknowns(" << number_of_unknowns << ")"
      " > "
      "residuals("<< number_of_residuals << ")"
      "\n";
  }
}

void vnl_least_squares_function::init(int number_of_unknowns,
                                      int number_of_residuals)
{
  n_ = number_of_residuals;
  p_ = number_of_unknowns;
  assert(n_>=0);
  assert(p_>=0);

  if (number_of_unknowns > number_of_residuals) {
    vcl_cerr << "vnl_least_squares_function: WARNING: "
      "unknowns(" << number_of_unknowns << ")"
      " > "
      "residuals("<< number_of_residuals << ")"
      "\n";
  }
}

vnl_least_squares_function::~vnl_least_squares_function()
{
}

void vnl_least_squares_function::throw_failure()
{
  //vcl_cerr << "throw_failure()\n";
  failure = true;
}

void vnl_least_squares_function::clear_failure()
{
  //vcl_cerr << "clear_failure()\n";
  failure = false;
}

void vnl_least_squares_function::gradf(vnl_vector<double> const& /*x*/,
                                       vnl_matrix<double>& /*jacobian*/)
{
}

//: Compute fd gradient
void vnl_least_squares_function::fdgradf(vnl_vector<double> const& x,
                                         vnl_matrix<double>& jacobian,
                                         double stepsize)
{
  int dim = x.size();
  int n = jacobian.rows();
  assert(dim == get_number_of_unknowns());
  assert(n == get_number_of_residuals());
  assert(dim == int(jacobian.columns()));

  vnl_vector<double> tx = x;
  vnl_vector<double> fplus(n);
  vnl_vector<double> fminus(n);
  double h = stepsize;
  for (int i = 0; i < dim; ++i)
  {
    double tplus = x[i] + h;
    tx[i] = tplus;
    this->f(tx, fplus);

    double tminus = x[i] - h;
    tx[i] = tminus;
    this->f(tx, fminus);

    double h = (tplus - tminus);
    for (int j = 0; j < n; ++j)
      jacobian(j,i) = (fplus[j] - fminus[j]) / h;

    tx[i] = x[i];
  }
}

void vnl_least_squares_function::trace(int /* iteration */,
                                       vnl_vector<double> const& /*x*/,
                                       vnl_vector<double> const& /*fx*/)
{
}

double vnl_least_squares_function::rms(vnl_vector<double> const& x)
{
  vnl_vector<double> fx(n_);
  f(x, fx);
  return fx.rms();
}
