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

#ifndef itkBitCast_h
#define itkBitCast_h

#if __cplusplus < 202002L

#  include <cstring>     // For memcpy.
#  include <type_traits> // For is_trivially_copyable and remove_const_t.

namespace itk
{

/** Rudimentary `bit_cast` implementation for C++14/C++17. From C++20, `std::bit_cast` is preferred.
 */
template <typename TDestination, class TSource>
TDestination
bit_cast(const TSource & source)
{
  static_assert(sizeof(TDestination) == sizeof(TSource),
                "The destination type should have the same size as the source type.");
  static_assert(std::is_trivially_copyable<TDestination>::value, "The destination type should be trivially copyable");
  static_assert(std::is_trivially_copyable<TSource>::value, "The source type should be trivially copyable.");

  std::remove_const_t<TDestination> result;
  std::memcpy(&result, &source, sizeof(TSource));
  return result;
}

} // namespace itk

#else

// From C++20, std::bit_cast is included with the C++ Standard Library.
#  include <bit>

namespace itk
{
using ::std::bit_cast;
}

#endif

#endif // itkBitCast_h
