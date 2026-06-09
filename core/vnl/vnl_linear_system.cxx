// This is core/vnl/vnl_linear_system.cxx
//:
// \file
// \author David Capel, capes@robots
// \date   July 2000

#include "vnl_linear_system.h"
#include <cassert>

vnl_linear_system::~vnl_linear_system() = default;

void
vnl_linear_system::apply_preconditioner(const vnl_vector<double> & x, vnl_vector<double> & px) const
{
  assert(px.size() == x.size());

  px = x;
}

double
vnl_linear_system::get_rms_error(const vnl_vector<double> & x) const
{
  vnl_vector<double> resid(n_);
  vnl_vector<double> b(n_);

  multiply(x, resid);
  get_rhs(b);

  resid -= b;

  return resid.rms();
}

double
vnl_linear_system::get_relative_residual(const vnl_vector<double> & x) const
{
  vnl_vector<double> resid(n_);
  vnl_vector<double> b(n_);

  multiply(x, resid);
  get_rhs(b);

  resid -= b;

  return resid.rms() / b.rms();
}
