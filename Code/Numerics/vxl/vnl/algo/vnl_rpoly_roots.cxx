#ifdef __GNUC__
#pragma implementation
#endif
//
// Class: vnl_rpoly_roots
// Author: Andrew W. Fitzgibbon, Oxford RRG
// Created: 06 Aug 96
//
//-----------------------------------------------------------------------------

#include "vnl_rpoly_roots.h"

#include <vcl_cstdlib.h>
#include <vcl_iostream.h>
#include <vnl/vnl_complex.h>
#include <vnl/vnl_math.h>
#include <vnl/algo/vnl_netlib.h> // rpoly_()

// - The constructor calculates the roots.  This is the most efficient interface
// as all the result variables are initialized to the correct size.
// The polynomial is @{$ a[0] x^d + a[1] x^{d-1} + \cdots + a[d] = 0 $@}.
// Note that if the routine fails, not all roots will be found.  In this case,
// the "realroots" and "roots" functions will return fewer than n roots.
vnl_rpoly_roots::vnl_rpoly_roots(const vnl_vector<double>& a)
  : coeffs_(a), r_(coeffs_.size()-1), i_(coeffs_.size()-1)
{
  // fsm : if the coefficients are NaNs then rpoly_ gets stuck in an 
  // infinite loop. of course, the caller shouldn't pass in NaNs, but
  // it would be nice to get an error message instead of hanging.
  bool has_nans=false;
  for (unsigned i=0;i<a.size();i++)
    if (vnl_math_isnan(a(i)))
      has_nans=true;

  if (has_nans) {
    vcl_cerr << __FILE__ " these coefficients are invalid :" << vcl_endl;
    for (unsigned i=0;i<a.size();i++)
      vcl_cerr << i << ' ' << a(i) << vcl_endl;
    abort();
  }

  compute();
}

// - Complex vector of all roots.
vnl_vector<vnl_double_complex> vnl_rpoly_roots::roots() const
{
  vnl_vector<vnl_double_complex> ret(num_roots_found_);
  for(int i = 0; i < num_roots_found_; ++i)
    ret[i] = vnl_double_complex(r_[i], i_[i]);
  return ret;
}

// - Return real roots only.  Roots are real if the absolute value
// of their imaginary part is less than the optional argument TOL.
// TOL defaults to 1e-12 [untested]
vnl_vector<double> vnl_rpoly_roots::realroots(double tol) const
{
  int c = 0;
  for(int i = 0; i < num_roots_found_; ++i)
    if (vnl_math_abs(i_[i]) < tol)
      ++c;

  vnl_vector<double> ret(c);
  c = 0;
  {for(int i = 0; i < num_roots_found_; ++i)
    if (vnl_math_abs(i_[i]) < tol)
      ret[c++] = r_[i];}

  return ret;
}

// -- Compute roots using Jenkins-Traub algorithm.
// Calls rpoly and interprets failure codes.
bool vnl_rpoly_roots::compute()
{
  int fail = 0;
  int n = coeffs_.size() - 1;

  rpoly_(coeffs_.data_block(), &n, r_.data_block(), i_.data_block(), &fail);

  if (!fail) {
    num_roots_found_ = n;
    return true;
  }
  
  num_roots_found_ = n;

  if (coeffs_[0] == 0.0)
    vcl_cerr << "vnl_rpoly_roots: Leading coefficient is zero.  Not allowed.\n";
  else
    vcl_cerr << "vnl_rpoly_roots: Calculation failed, only " << n << " roots found\n";

  return false;
}
