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

#include <array>
#include <iostream>
#include <iterator>
#include <vector>
#include <list>
#include <type_traits>


namespace itk::print_helper
{

// Forward declarations so the per-container bodies below see all overloads at
// definition time.  Required for nested cases like vector<list<T>>, where the
// recursive `os << *it` is parsed before the list overload would otherwise be
// declared, and ADL on std container types never reaches itk::print_helper.
template <typename T>
std::ostream &
operator<<(std::ostream & os, const std::vector<T> & v);

template <typename T>
std::ostream &
operator<<(std::ostream & os, const std::list<T> & l);

template <typename T, size_t VLength>
std::ostream &
operator<<(std::ostream & os, const std::array<T, VLength> & container);

template <typename T, size_t VLength, typename = std::enable_if_t<!std::is_same_v<T, char>>>
std::ostream &
operator<<(std::ostream & os, const T (&arr)[VLength]);

template <typename T>
std::ostream &
operator<<(std::ostream & os, const std::vector<T> & v)
{
  if (v.empty())
  {
    return os << "[]";
  }

  os << '[';
  // Manual loop so that an unqualified `os << *it` resolves overloads in
  // itk::print_helper for nested containers; std::ostream_iterator inserts
  // from inside namespace std and would not see them.
  for (auto it = v.begin(); it != std::prev(v.end()); ++it)
  {
    os << *it << ", ";
  }
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
  for (auto it = l.begin(); it != std::prev(l.end()); ++it)
  {
    os << *it << ", ";
  }
  return os << l.back() << ']';
}

template <typename T, size_t VLength>
std::ostream &
operator<<(std::ostream & os, [[maybe_unused]] const std::array<T, VLength> & container)
{
  if constexpr (VLength == 0)
  {
    return os << "()";
  }
  else
  {
    os << '(';
    for (auto it = container.cbegin(); it != std::prev(container.cend()); ++it)
    {
      os << *it << ", ";
    }
    return os << container.back() << ')';
  }
}

// Stream insertion operator for C-style arrays, excluding character arrays (strings)
template <typename T, size_t VLength, typename>
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
