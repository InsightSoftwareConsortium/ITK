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
#ifndef itkNumberToString_h
#define itkNumberToString_h

#include "itkMacro.h"
#include <string>

namespace itk
{
/** \class NumberToString
 * \brief Convert floating and fixed point numbers to strings
 *
 * This class uses the double-conversion library to floating point and
 * fixed point numbers to ASCII versions that are represented without
 * numerical precision errors.
 *
 * Typical use:
 *  \#include "itkNumberToString.h"
 *  NumberToString<float> convert;
 *  float a = 1.0f/3.0f;
 *  std::cout << convert(a) << std::endl;
 *
 * The specialization `NumberToString<>` allows conversion from any type of number:
 *
 *  NumberToString<> convert;
 *  float a = 1.0f/3.0f;
 *  auto b = std::numeric_limits<int>::max();
 *  std::cout << convert(a) << convert(b) << std::endl;
 *
 * \ingroup ITKCommon
 */
template <typename TValue = void>
class ITK_TEMPLATE_EXPORT NumberToString
{
public:
  std::string
  operator()(TValue val) const;
};

// declaration of specialization
template <>
ITKCommon_EXPORT std::string
NumberToString<double>::operator()(double val) const;
template <>
ITKCommon_EXPORT std::string
NumberToString<float>::operator()(float val) const;

template <>
class NumberToString<void>
{
public:
  template <typename TValue>
  std::string
  operator()(const TValue val) const
  {
    constexpr NumberToString<TValue> convert{};
    return convert(val);
  }
};


/** Converts the specified numeric value to a string, using a `NumberToString` function object. Produces a full
 * precision (lossless) string representation, also for `float`and `double`. Otherwise it is similar to `std::to_string`
 */
template <typename TValue>
std::string
ConvertNumberToString(const TValue val)
{
  constexpr NumberToString<TValue> convert{};
  return convert(val);
}

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNumberToString.hxx"
#endif
#endif
