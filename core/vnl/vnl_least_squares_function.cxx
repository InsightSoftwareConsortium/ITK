// This is core/vnl/vnl_least_squares_function.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author Andrew W. Fitzgibbon, Oxford RRG
// \date   31 Aug 96

#include <iostream>
#include "vnl_least_squares_function.h"
#include <vcl_compiler.h>
#include <vcl_cassert.h>

void vnl_least_squares_function::dim_warning(unsigned int number_of_unknowns,
                                             unsigned int number_of_residuals)
{
  if (number_of_unknowns > number_of_residuals)
    std::cerr << "vnl_least_squares_function: WARNING: "
             << "unknowns(" << number_of_unknowns << ") > "
             << "residuals("<< number_of_residuals << ")\n";
}

void vnl_least_squares_function::gradf(vnl_vector<double> const& /*x*/,
                                       vnl_matrix<double>& /*jacobian*/)
{
  std::cerr << "Warning: gradf() called but not implemented in derived class\n";
}

//: Compute finite differences gradient using central differences.
void vnl_least_squares_function::fdgradf(vnl_vector<double> const& x,
                                         vnl_matrix<double>& jacobian,
                                         double stepsize)
{
  unsigned int dim = x.size();
  unsigned int n = jacobian.rows();
  assert(dim == get_number_of_unknowns());
  assert(n == get_number_of_residuals());
  assert(dim == jacobian.columns());

  vnl_vector<double> tx = x;
  vnl_vector<double> fplus(n);
  vnl_vector<double> fminus(n);
  for (unsigned int i = 0; i < dim; ++i)
  {
    // calculate f just to the right of x[i]
    double tplus = tx[i] = x[i] + stepsize;
    this->f(tx, fplus);

    // calculate f just to the left of x[i]
    double tminus = tx[i] = x[i] - stepsize;
    this->f(tx, fminus);

    double h = 1.0 / (tplus - tminus);
    for (unsigned int j = 0; j < n; ++j)
      jacobian(j,i) = (fplus[j] - fminus[j]) * h;

    // restore tx
    tx[i] = x[i];
  }
}


//: Compute finite differences gradient using forward differences.
void vnl_least_squares_function::ffdgradf(vnl_vector<double> const& x,
                                          vnl_matrix<double>& jacobian,
                                          double stepsize)
{
  unsigned int dim = x.size();
  unsigned int n = jacobian.rows();
  assert(dim == get_number_of_unknowns());
  assert(n == get_number_of_residuals());
  assert(dim == jacobian.columns());

  vnl_vector<double> tx = x;
  vnl_vector<double> fplus(n);
  vnl_vector<double> fcentre(n);
  this->f(x, fcentre);
  for (unsigned int i = 0; i < dim; ++i)
  {
    // calculate f just to the right of x[i]
    double tplus = tx[i] = x[i] + stepsize;
    this->f(tx, fplus);

    double h = 1.0 / (tplus - x[i]);
    for (unsigned int j = 0; j < n; ++j)
      jacobian(j,i) = (fplus[j] - fcentre[j]) * h;

    // restore tx
    tx[i] = x[i];
  }
}

void vnl_least_squares_function::trace(int /* iteration */,
                                       vnl_vector<double> const& /*x*/,
                                       vnl_vector<double> const& /*fx*/)
{
  // This default implementation is empty; overloaded in derived class.
}

double vnl_least_squares_function::rms(vnl_vector<double> const& x)
{
  vnl_vector<double> fx(n_);
  f(x, fx);
  return fx.rms();
}
