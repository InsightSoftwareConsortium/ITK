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

#endif // vnl_complex_traits_h_
