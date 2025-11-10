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

#ifndef itkPrintHelper_h
#define itkPrintHelper_h

#include <iostream>
#include <iterator>
#include <list>
#include <type_traits>

// Workaround for https://gcc.gnu.org/bugzilla/show_bug.cgi?id=112467
#if defined(ITK_WRAPPING_PARSER) && defined(__GNUC__) && !defined(__clang__)
#  define __clang__
#  define ITK_CASTXML_GCC_VECTOR_WORKAROUND
#endif
#include <vector>
#if defined(ITK_CASTXML_GCC_VECTOR_WORKAROUND)
#  undef __clang__
#  undef ITK_CASTXML_GCC_VECTOR_WORKAROUND
#endif

namespace itk::print_helper
{

template <typename T>
std::ostream &
operator<<(std::ostream & os, const std::vector<T> & v)
{
  if (v.empty())
  {
    return os << "[]";
  }

  os << '[';
  std::copy(v.begin(), v.end() - 1, std::ostream_iterator<T>(os, ", "));
  return os << v.back() << ']';
}

template <typename T>
std::ostream &
operator<<(std::ostream & os, const std::list<T> & l)
{
  if (l.empty())
  {
    return os << "[]";
  }

  os << '[';
  std::copy(l.begin(), std::prev(l.end()), std::ostream_iterator<T>(os, ", "));
  return os << l.back() << ']';
}

// Stream insertion operator for C-style arrays, excluding character arrays (strings)
template <typename T, size_t VLength, typename = std::enable_if_t<!std::is_same_v<T, char>>>
std::ostream &
operator<<(std::ostream & os, const T (&arr)[VLength])
{
  if constexpr (VLength == 0)
  {
    return os << "()";
  }

  os << '(';
  for (size_t i = 0; i < VLength - 1; ++i)
  {
    os << arr[i] << ", ";
  }
  return os << arr[VLength - 1] << ')';
}

} // namespace itk::print_helper

#endif // itkPrintHelper_h
