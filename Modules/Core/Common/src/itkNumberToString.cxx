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
#include "itkNumberToString.h"
#include "double-conversion/double-to-string.h"

#include <sstream>

namespace
{

bool
ConvertToShortest(const double_conversion::DoubleToStringConverter & converter,
                  const double                                       val,
                  double_conversion::StringBuilder &                 builder)
{
  return converter.ToShortest(val, &builder);
}

bool
ConvertToShortest(const double_conversion::DoubleToStringConverter & converter,
                  const float                                        val,
                  double_conversion::StringBuilder &                 builder)
{
  // Call the converter member function that is specific for single-precision `float`.
  return converter.ToShortestSingle(val, &builder);
}

template <typename TValue>
std::string
FloatingPointNumberToString(const TValue val)
{
  // Declare a buffer, large enough for strings like:
  // "-100000000000000000000" (-1e20, either float or double, 23 chars)
  // "-1.7976931348623157e+308" (-DBL_MAX, 25 chars)
  // "-0.0000033333333333333333" (-3e-005/9.0, 26 chars)
  char buf[32];

  double_conversion::StringBuilder builder(buf, sizeof(buf));

  if (!ConvertToShortest(double_conversion::DoubleToStringConverter::EcmaScriptConverter(), val, builder))
  {
    itkGenericExceptionMacro(<< "Conversion failed for " << val);
  }
  return std::string(builder.Finalize());
}

} // namespace

namespace itk
{

template <>
std::string
NumberToString<double>::operator()(double val) const
{
  return FloatingPointNumberToString(val);
}

template <>
std::string
NumberToString<float>::operator()(float val) const
{
  return FloatingPointNumberToString(val);
}

} // namespace itk
