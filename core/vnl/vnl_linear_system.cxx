// This is core/vnl/vnl_linear_system.cxx
#ifdef VCL_NEEDS_PRAGMA_INTERFACE
#pragma implementation
#endif
//:
// \file
// \author David Capel, capes@robots
// \date   July 2000

#include "vnl_linear_system.h"
#include <vcl_cassert.h>

vnl_linear_system::~vnl_linear_system()
{
}

void vnl_linear_system::apply_preconditioner(vnl_vector<double> const& x, vnl_vector<double> & px) const
{
  assert(px.size() == x.size());

  px = x;
}

double vnl_linear_system::get_rms_error(vnl_vector<double> const& x) const
{
  vnl_vector<double> resid(n_);
  vnl_vector<double> b(n_);

  multiply(x, resid);
  get_rhs(b);

  resid -= b;

  return resid.rms();
}

double vnl_linear_system::get_relative_residual(vnl_vector<double> const& x) const
{
  vnl_vector<double> resid(n_);
  vnl_vector<double> b(n_);

  multiply(x, resid);
  get_rhs(b);

  resid -= b;

  return resid.rms() / b.rms();
}
