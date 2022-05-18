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
#include "itkBooleanStdVector.h"

#include <type_traits>

namespace
{
using ValueType = itk::BooleanStdVectorType::value_type;

static_assert(sizeof(ValueType) <= sizeof(bool),
              "The value type of BooleanStdVectorType should not be larger than the built-in C++ bool type.");
static_assert(
  ValueType{ false } == false,
  "The value type of BooleanStdVectorType should be allowed as a conditional expression, evaluating to false.");
static_assert(
  ValueType{ true } == true,
  "The value type of BooleanStdVectorType should be allowed as a conditional expression, evaluating to true.");
static_assert(bool{ ValueType{ false } } == false,
              "The value type of BooleanStdVectorType should support a lossless round-trip from bool value false.");
static_assert(bool{ ValueType{ true } } == true,
              "The value type of BooleanStdVectorType should support a lossless round-trip from bool value true.");
static_assert(std::is_same<decltype(*itk::BooleanStdVectorType{}.data()), ValueType &>::value,
              "BooleanStdVectorType should have a valid data() member function (unlike std::vector<bool>).");

} // namespace
