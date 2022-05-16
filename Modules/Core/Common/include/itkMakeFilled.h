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
#ifndef itkMakeFilled_h
#define itkMakeFilled_h

namespace itk
{
/** Yields a container of the specified type, filled by the specified value, assigned to each of its elements.
 * Only intended for container types of a compile-time fixed size, including instances of FixedArray (and derived
 * classes), Index, Offset, Size, and std::array. Moreover, their size (number of elements) should be greater than zero.
 */
template <typename TContainer>
constexpr TContainer
MakeFilled(typename TContainer::const_reference value)
{
  // Note: The seemingly redundant {} initialization is required for C++14 and C++17, in order to declare MakeFilled
  // constexpr. (C++20 does allow uninitialized variables in a constexpr function.)
  TContainer container{};

  static_assert(container.size() > 0, "MakeFilled requires TContainer with constant non-zero size!");

  // Note that `std::fill` and `std::fill_n` are not `constexpr` until C++20.
  for (auto & element : container)
  {
    element = value;
  }
  return container;
}
} // namespace itk

#endif // itkMakeFilled_h
