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

#ifndef itkMakeUniqueForOverwrite_h
#define itkMakeUniqueForOverwrite_h

#include <memory> // For unique_ptr.

#if __cplusplus < 202002L

#  include <type_traits> // For remove_extent_t, false_type, etc.

namespace itk
{

// Namespace of implementation details (not part of the public ITK interface).
namespace MakeUniqueForOverwriteImplDetail
{

// `is_unbounded_array` implementation for C++14/C++17. From C++20, `std::is_unbounded_array` is preferred.
template <typename>
struct is_unbounded_array : std::false_type
{};

template <typename T>
struct is_unbounded_array<T[]> : std::true_type
{};

} // namespace MakeUniqueForOverwriteImplDetail


/** `make_unique_for_overwrite` implementation for C++14/C++17, specifically for dynamically sized ("unbounded")
 * arrays. From C++20, `std::make_unique_for_overwrite` is preferred.
 *
 * Example (mind the square brackets):
   \code
     using ElementType = int;
     size_t numberOfElements{ 42 };

     // Create a buffer that may be used to store the specified number of elements.
     auto buffer = make_unique_for_overwrite<ElementType[]>(numberOfElements);
   \endcode
 *
 */
template <typename TUnboundedArray>
auto
make_unique_for_overwrite(const size_t numberOfElements)
{
  // The template argument must be something like `ElementType[]`, having an empty pair of square brackets.
  static_assert(MakeUniqueForOverwriteImplDetail::is_unbounded_array<TUnboundedArray>::value,
                "The specified template argument must be an unbounded array type!");
  return std::unique_ptr<TUnboundedArray>(new std::remove_extent_t<TUnboundedArray>[numberOfElements]);
}

} // namespace itk

#else

namespace itk
{
using ::std::make_unique_for_overwrite;
}

#endif

#endif // itkMakeUniqueForOverwrite_h
