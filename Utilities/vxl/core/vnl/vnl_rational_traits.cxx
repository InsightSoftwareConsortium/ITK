//:
// \file
// \author Peter Vanroose
// \date   6 September 2002
//
//-----------------------------------------------------------------------------

#include "vnl_rational_traits.h"
#include <vcl_complex.h>
#include <vcl_cmath.h>

const vnl_rational vnl_numeric_traits<vnl_rational>::zero = vnl_rational(0L,1L);
const vnl_rational vnl_numeric_traits<vnl_rational>::one = vnl_rational(1L,1L);
const vnl_rational vnl_numeric_traits<vnl_rational>::maxval = vnl_rational(vnl_numeric_traits<long>::maxval,1L);

const vcl_complex<vnl_rational> vnl_numeric_traits<vcl_complex<vnl_rational> >::zero
  = vcl_complex<vnl_rational>(vnl_rational(0L,1L),vnl_rational(0L,1L));
const vcl_complex<vnl_rational> vnl_numeric_traits<vcl_complex<vnl_rational> >::one
  = vcl_complex<vnl_rational>(vnl_rational(1L,1L),vnl_rational(0L,1L));

vnl_rational vnl_math_squared_magnitude(vcl_complex<vnl_rational> const& x)
{
  return x.real()*x.real()+x.imag()*x.imag();
}

vnl_rational vnl_math_abs(vcl_complex<vnl_rational> const& x)
{
  return vnl_rational(vcl_sqrt(double(x.real()*x.real()+x.imag()*x.imag())));
}

vcl_ostream& operator<<(vcl_ostream& os, vcl_complex<vnl_rational> x)
{
  return os << x.real() << '+' << x.imag() << 'j';
}
