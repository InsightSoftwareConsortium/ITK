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
#include "itkSampleToHistogramFilter.h"

#include <gtest/gtest.h>
#include <sstream>
#include <string>
#include <type_traits>


namespace
{
bool
Contains(const std::string & text, const std::string & part)
{
  return text.find(part) != std::string::npos;
}


// Checks an exception declared by itkDeclareExceptionMacro.
template <typename TException, typename TExpectedBaseClass>
void
CheckDeclaredException(const std::string & expectedName, const std::string & expectedDefaultMessage)
{
  static_assert(std::is_base_of_v<TExpectedBaseClass, TException>);
  static_assert(std::is_base_of_v<itk::ExceptionObject, TException>);
  static_assert(std::is_nothrow_default_constructible_v<TException>);

  EXPECT_EQ(TException::default_exception_message, expectedDefaultMessage);

  const TException defaultConstructed{};
  EXPECT_EQ(defaultConstructed.GetNameOfClass(), expectedName);

  const TException exception(__FILE__, __LINE__, TException::default_exception_message, ITK_LOCATION);
  EXPECT_EQ(exception.GetNameOfClass(), expectedName);
  EXPECT_EQ(exception.GetDescription(), expectedDefaultMessage);

  std::ostringstream stream;
  stream << exception;
  EXPECT_TRUE(Contains(stream.str(), "itk::" + expectedName));
  EXPECT_TRUE(Contains(stream.str(), expectedDefaultMessage));

  const TException copy(exception);
  EXPECT_EQ(copy, exception);

  const TExpectedBaseClass & base = exception;
  EXPECT_EQ(base.GetNameOfClass(), expectedName);
}
} // namespace


TEST(SampleToHistogramFilterExceptions, DeclaredExceptions)
{
  CheckDeclaredException<itk::SampleToHistogramFilterException, itk::ExceptionObject>(
    "SampleToHistogramFilterException", "Histogram-related Exception");
  CheckDeclaredException<itk::MissingHistogramSizeInput, itk::SampleToHistogramFilterException>(
    "MissingHistogramSizeInput", "Histogram Size input is missing");
  CheckDeclaredException<itk::MissingHistogramMarginalScaleInput, itk::SampleToHistogramFilterException>(
    "MissingHistogramMarginalScaleInput", "Histogram marginal scale input is missing");
  CheckDeclaredException<itk::NullSizeHistogramInputMeasurementVectorSize, itk::SampleToHistogramFilterException>(
    "NullSizeHistogramInputMeasurementVectorSize", "Input sample MeasurementVectorSize is zero");
  CheckDeclaredException<itk::MissingHistogramBinMaximumInput, itk::SampleToHistogramFilterException>(
    "MissingHistogramBinMaximumInput", "Histogram Bin Maximum input is missing");
  CheckDeclaredException<itk::MissingHistogramBinMinimumInput, itk::SampleToHistogramFilterException>(
    "MissingHistogramBinMinimumInput", "Histogram Bin Minimum input is missing");
  CheckDeclaredException<itk::HistogramWrongNumberOfComponents, itk::SampleToHistogramFilterException>(
    "HistogramWrongNumberOfComponents", "Histogram has wrong number of components");
}
