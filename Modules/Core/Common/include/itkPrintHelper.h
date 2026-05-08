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

#include "itkIndent.h"

#include <array>
#include <iostream>
#include <iterator>
#include <vector>
#include <list>
#include <type_traits>


namespace itk
{
// Forward declaration so itkPrintHelper.h can be safely included from
// itkMacro.h without re-entering itkNumericTraits.h (which itself uses
// macros defined later in itkMacro.h).  Every PrintNumericTrait() call
// site needs the full NumericTraits<T> specialization in scope, but those
// sites already #include "itkNumericTraits.h" directly or transitively.
template <typename T>
class NumericTraits;
} // namespace itk

namespace itk::print_helper
{

// Forward declarations so the per-container bodies below see all overloads at
// definition time.  Required for nested cases like vector<list<T>>, where the
// recursive `os << *it` is parsed before the list overload would otherwise be
// declared, and ADL on std container types never reaches itk::print_helper.
// PrintNumericTrait further down also dispatches through `os << value` and
// must see these forward declarations to instantiate against std container
// member types at the call site.
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

/** \brief Print "<name>: <value>\n" indented, matching ITK's PrintSelf style.
 *
 * Formats and writes a single named member to \a os preceded by \a indent,
 * followed by a newline.  When \c NumericTraits<T>::PrintType differs from
 * \a T (the relevant case being the \c char family, whose \c PrintType is
 * \c int, so values render numerically rather than as ASCII characters) the
 * value is forwarded through a \c static_cast.  When the two types coincide
 * (the common case, including all built-in scalars wider than \c char and
 * \c std::complex specialisations whose \c PrintType is \c Self) the cast
 * step is skipped entirely so the value's own stream insertion overload is
 * selected directly.
 *
 * Equivalent to the boilerplate
 * \code
 *   os << indent << "Name: "
 *      << static_cast<typename NumericTraits<T>::PrintType>(m_Name)
 *      << std::endl;
 * \endcode
 * but with explicit \a os and \a indent parameters and no preprocessor
 * macro expansion.
 *
 * Typical use inside a \c PrintSelf override:
 * \code
 *   print_helper::PrintNumericTrait(os, indent, "Threshold", m_Threshold);
 * \endcode
 */
template <typename T>
inline void
PrintNumericTrait(std::ostream & os, const Indent & indent, const char * name, const T & value)
{
  os << indent << name << ": ";
  if constexpr (std::is_same_v<T, typename NumericTraits<T>::PrintType>)
  {
    os << value;
  }
  else
  {
    os << static_cast<typename NumericTraits<T>::PrintType>(value);
  }
  os << std::endl;
}

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
