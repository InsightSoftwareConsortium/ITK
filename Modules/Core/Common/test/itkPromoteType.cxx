/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkPromoteType.h"
#include <complex>
#include <type_traits>

namespace itk
{
namespace mpl
{

template <typename TA, typename TB>
struct PromoteType<std::complex<TA>, std::complex<TB>>
{
  using Type = std::complex<typename PromoteType<TA, TB>::Type>;
};
} // namespace mpl
} // namespace itk

int
itkPromoteType(int, char *[])
{
  using namespace itk::mpl;

  // Prvalues of small integral types (such as char) may be converted to prvalues
  // of larger integral types (such as int). In particular, arithmetic operators
  // do not accept types smaller than int as arguments, and integral promotions
  // are automatically applied after lvalue-to-rvalue conversion, if applicable.
  // This conversion always preserves the value.
  //
  // The following implicit conversions are classified as integral promotions:
  // * signed char or short can be converted to int.
  // * unsigned char or unsigned short can be converted to int if it can hold
  //   its entire value range, and unsigned int otherwise.
  // * char can be converted to int or unsigned int depending on the underlying
  //   type: signed char or unsigned char (see above)
  // * wchar_t, char16_t, and char32_t can be converted to the first type from
  //   the following list able to hold their entire value range: int, unsigned
  //   int, long, unsigned long, long long, unsigned long long.

  static_assert(std::is_same<PromoteType<signed char, int>::Type, int>::value, "test failed");
  static_assert(std::is_same<PromoteType<signed char, short>::Type, int>::value, "test failed");
  static_assert(std::is_same<PromoteType<unsigned char, int>::Type, int>::value, "test failed");
  static_assert(std::is_same<PromoteType<unsigned char, unsigned int>::Type, unsigned int>::value, "test failed");
  static_assert(std::is_same<PromoteType<int, int>::Type, int>::value, "test failed");
  static_assert(std::is_same<PromoteType<short, int>::Type, int>::value, "test failed");
  static_assert(std::is_same<PromoteType<double, int>::Type, double>::value, "test failed");
  static_assert(std::is_same<PromoteType<float, int>::Type, float>::value, "test failed");
  static_assert(std::is_same<PromoteType<long, int>::Type, long>::value, "test failed");
  static_assert(std::is_same<PromoteType<long long, int>::Type, long long>::value, "test failed");
  static_assert(std::is_same<PromoteType<int, long long>::Type, long long>::value, "test failed");
  static_assert(std::is_same<PromoteType<long, long double>::Type, long double>::value, "test failed");
  static_assert(std::is_same<PromoteType<double, std::complex<double>>::Type, std::complex<double>>::value,
                "test failed");

  static_assert(std::is_same<PromoteType<std::complex<int>, std::complex<double>>::Type, std::complex<double>>::value,
                "test failed");
  return EXIT_SUCCESS;
}
