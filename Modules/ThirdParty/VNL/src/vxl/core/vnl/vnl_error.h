// This is core/vnl/vnl_error.h
#ifndef vnl_error_h_
#define vnl_error_h_
//:
//  \file
//  \author fsm
#include "vnl/vnl_export.h"

//: Raise exception for invalid index.
extern VNL_EXPORT void
vnl_error_vector_index(const char * fcn, int index);

//: Raise exception for invalid dimension.
extern VNL_EXPORT void
vnl_error_vector_dimension(const char * fcn, int l1, int l2);

//: Raise exception for using class objects, or chars in (...).
extern VNL_EXPORT void
vnl_error_vector_va_arg(int n);

//: Raise exception for invalid row index.
extern VNL_EXPORT void
vnl_error_matrix_row_index(const char * fcn, unsigned r);

//: Raise exception for invalid col index.
extern VNL_EXPORT void
vnl_error_matrix_col_index(const char * fcn, unsigned c);

//: Raise exception for invalid dimensions.
extern VNL_EXPORT void
vnl_error_matrix_dimension(const char * fcn, int r1, int c1, int r2, int c2);

//: Raise exception for a nonsquare matrix.
extern VNL_EXPORT void
vnl_error_matrix_nonsquare(const char * fcn);

//: Raise exception for using class objects, or chars in (...).
extern VNL_EXPORT void
vnl_error_matrix_va_arg(int n);

//: Template overloads accepting non-int integral types to avoid narrowing casts at call sites.
//  These forward to the original int-based functions after asserting values are in range.
#include <cassert>
#include <limits>
#include <type_traits>

template <typename T>
inline void
vnl_error_assert_int_range(T v)
{
  static_assert(std::is_integral_v<T>);
  if constexpr (sizeof(T) > sizeof(int) || (std::is_unsigned_v<T> && sizeof(T) == sizeof(int)))
  {
    assert(static_cast<long long>(v) >= static_cast<long long>(std::numeric_limits<int>::min()));
    assert(static_cast<long long>(v) <= static_cast<long long>(std::numeric_limits<int>::max()));
  }
}

template <typename T, std::enable_if_t<std::is_integral_v<T> && !std::is_same_v<T, int>, int> = 0>
inline void
vnl_error_vector_index(const char * fcn, T index)
{
  vnl_error_assert_int_range(index);
  vnl_error_vector_index(fcn, static_cast<int>(index));
}

template <typename T1,
          typename T2,
          std::enable_if_t<std::is_integral_v<T1> && std::is_integral_v<T2> &&
                             !(std::is_same_v<T1, int> && std::is_same_v<T2, int>),
                           int> = 0>
inline void
vnl_error_vector_dimension(const char * fcn, T1 l1, T2 l2)
{
  vnl_error_assert_int_range(l1);
  vnl_error_assert_int_range(l2);
  vnl_error_vector_dimension(fcn, static_cast<int>(l1), static_cast<int>(l2));
}

template <
  typename T1,
  typename T2,
  typename T3,
  typename T4,
  std::enable_if_t<
    std::is_integral_v<T1> && std::is_integral_v<T2> && std::is_integral_v<T3> && std::is_integral_v<T4> &&
      !(std::is_same_v<T1, int> && std::is_same_v<T2, int> && std::is_same_v<T3, int> && std::is_same_v<T4, int>),
    int> = 0>
inline void
vnl_error_matrix_dimension(const char * fcn, T1 r1, T2 c1, T3 r2, T4 c2)
{
  vnl_error_assert_int_range(r1);
  vnl_error_assert_int_range(c1);
  vnl_error_assert_int_range(r2);
  vnl_error_assert_int_range(c2);
  vnl_error_matrix_dimension(
    fcn, static_cast<int>(r1), static_cast<int>(c1), static_cast<int>(r2), static_cast<int>(c2));
}

#endif // vnl_error_h_
