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

#include "itkMacro.h"

#include <cstdlib>
#include <type_traits>

namespace
{
struct Comparable
{
  bool
  operator==(const Comparable &) const
  {
    return true;
  }
  ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Comparable);
};

template <typename T, typename = void>
struct HasMemberUnequalOperator : std::false_type
{};

template <typename T>
struct HasMemberUnequalOperator<T, std::void_t<decltype(&T::operator!=)>> : std::true_type
{};

#if defined(ITK_ALPHA_FEATURE_CXX20)
static_assert(!HasMemberUnequalOperator<Comparable>::value);
#else
static_assert(HasMemberUnequalOperator<Comparable>::value);
#endif
} // namespace

int
main()
{
  return EXIT_SUCCESS;
}
