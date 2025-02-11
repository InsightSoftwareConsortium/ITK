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

// First include the header file to be tested:
#include "itkArray.h"
#include <gtest/gtest.h>

namespace
{
// Checks that `itk::Array` supports class template argument deduction (CTAD).
template <typename TValue>
constexpr bool
CheckClassTemplateArgumentDeduction()
{
  using ExpectedType = itk::Array<TValue>;

  const TValue constValue{};
  static_assert(std::is_same_v<decltype(itk::Array(&constValue, 1)), ExpectedType>,
                "The `Array(const ValueType *, ...)` constructor should support CTAD!");

  TValue nonConstValue{};
  static_assert(std::is_same_v<decltype(itk::Array(&nonConstValue, 1)), ExpectedType>,
                "The `Array(ValueType *, ...)` constructor should support CTAD!");
  return true;
}
} // namespace


static_assert(CheckClassTemplateArgumentDeduction<int>() && CheckClassTemplateArgumentDeduction<float>());
