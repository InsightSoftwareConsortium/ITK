// This is core/vnl/vnl_complex_traits.h
#ifndef vnl_complex_traits_h_
#define vnl_complex_traits_h_
//:
// \file
// \brief To allow templated algorithms to determine appropriate actions of conjugation, complexification etc.
// \author fsm, Oxford RRG, 26 Mar 1999
//
// \verbatim
//  Modifications
//   LSB (Manchester) 26/3/01 Documentation tidied
// \endverbatim
//-----------------------------------------------------------------------------

#include <complex>
#include <stdexcept>
#include <type_traits>
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include "vnl/vnl_export.h"

template <class T, class = void>
struct vnl_complex_traits;

template <class T>
struct VNL_EXPORT vnl_complex_traits<T, std::enable_if_t<std::is_integral_v<T>>>
{
  enum
  {
    isreal = true
  };
  static T
  conjugate(T x)
  {
    return x;
  }
  static std::complex<float>
  complexify(T)
  {
    throw std::runtime_error("Can not call complexify on non floating point data type");
  }
};

template <class T>
struct VNL_EXPORT vnl_complex_traits<T, std::enable_if_t<std::is_floating_point_v<T>>>
{
  enum
  {
    isreal = true
  };
  static T
  conjugate(T x)
  {
    return x;
  }
  static std::complex<T>
  complexify(T x)
  {
    return { x, T{ 0 } };
  }
};

template <class T>
struct VNL_EXPORT vnl_complex_traits<std::complex<T>, std::enable_if_t<std::is_floating_point_v<T>>>
{
  enum
  {
    isreal = false
  };
  static std::complex<T>
  conjugate(std::complex<T> x)
  {
    return std::conj(x);
  }
  static std::complex<T>
  complexify(T x)
  {
    return x;
  }
};

#include "vnl_bignum.h"

template <>
struct VNL_EXPORT vnl_complex_traits<vnl_bignum>
{
  enum
  {
    isreal = true
  };
  static vnl_bignum
  conjugate(vnl_bignum x)
  {
    return x;
  }
  static std::complex<float>
  complexify(vnl_bignum /* x */)
  {
    throw std::runtime_error("Can not call complexify on non floating point data type");
    return std::complex<float>(0., 0.);
  }
};

template <>
struct VNL_EXPORT vnl_complex_traits<std::complex<vnl_bignum>>
{
  enum
  {
    isreal = false
  };
  static std::complex<float>
  conjugate(std::complex<vnl_bignum> /* x */)
  {
    throw std::runtime_error("Can not call complexify on non floating point data type");
    return std::complex<float>(0., 0.);
  }
  static std::complex<float>
  complexify(std::complex<float> /* x */)
  {
    throw std::runtime_error("Can not call complexify on non floating point data type");
    return std::complex<float>(0., 0.);
  }
};

#include "vnl_rational.h"

template <>
struct VNL_EXPORT vnl_complex_traits<vnl_rational>
{
  enum
  {
    isreal = true
  };
  static vnl_rational
  conjugate(vnl_rational x)
  {
    return x;
  }
  static std::complex<float>
  complexify(vnl_rational /* x */)
  {
    throw std::runtime_error("Can not call complexify on non floating point data type");
    return std::complex<float>(0., 0.);
  }
};

template <>
struct VNL_EXPORT vnl_complex_traits<std::complex<vnl_rational>>
{
  enum
  {
    isreal = false
  };
  static std::complex<float>
  conjugate(std::complex<float> /* x */)
  {
    throw std::runtime_error("Can not call complexify on non floating point data type");
    return std::complex<float>(0., 0.);
  }
  static std::complex<vnl_rational>
  complexify(std::complex<vnl_rational> x)
  {
    return x;
  }
};

#endif // vnl_complex_traits_h_
