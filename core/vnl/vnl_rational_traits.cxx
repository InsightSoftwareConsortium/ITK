//:
// \file
// \author Peter Vanroose
// \date   6 September 2002
//
//-----------------------------------------------------------------------------

#include <complex>
#include <cmath>
#include "vnl_rational_traits.h"
#include <vcl_compiler.h>

const vnl_rational vnl_numeric_traits<vnl_rational>::zero = vnl_rational(0L,1L);
const vnl_rational vnl_numeric_traits<vnl_rational>::one = vnl_rational(1L,1L);
const vnl_rational vnl_numeric_traits<vnl_rational>::maxval = vnl_rational(vnl_numeric_traits<long>::maxval,1L);

const std::complex<vnl_rational> vnl_numeric_traits<std::complex<vnl_rational> >::zero
  = std::complex<vnl_rational>(vnl_rational(0L,1L),vnl_rational(0L,1L));
const std::complex<vnl_rational> vnl_numeric_traits<std::complex<vnl_rational> >::one
  = std::complex<vnl_rational>(vnl_rational(1L,1L),vnl_rational(0L,1L));

namespace vnl_math
{
  vnl_rational squared_magnitude(std::complex<vnl_rational> const& x)
  {
    return x.real()*x.real()+x.imag()*x.imag();
  }

  vnl_rational abs(std::complex<vnl_rational> const& x)
  {
    return vnl_rational(std::sqrt(double(x.real()*x.real()+x.imag()*x.imag())));
  }
}

std::ostream& operator<<(std::ostream& os, std::complex<vnl_rational> x)
{
  return os << x.real() << '+' << x.imag() << 'j';
}
