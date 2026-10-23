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

// First include the header files to be tested:
#include "itkDataObject.h"
#include "itkDeref.h"
#include "itkExceptionObject.h"

#include "itkImage.h"
#include <gtest/gtest.h>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>


namespace
{
template <typename TException>
std::string
PrintToString(const TException & exception)
{
  std::ostringstream stream;
  stream << exception;
  return stream.str();
}


bool
Contains(const std::string & text, const std::string & part)
{
  return text.find(part) != std::string::npos;
}


// Checks an exception type that inherits the constructors of ExceptionObject.
template <typename TException>
void
CheckExceptionWithInheritedConstructors(const std::string & expectedName)
{
  static_assert(std::is_base_of_v<itk::ExceptionObject, TException>);
  static_assert(std::is_nothrow_default_constructible_v<TException>);
  static_assert(std::is_nothrow_copy_constructible_v<TException>);
  static_assert(std::is_nothrow_copy_assignable_v<TException>);

  const TException defaultConstructed{};
  EXPECT_EQ(defaultConstructed.GetNameOfClass(), expectedName);
  EXPECT_STREQ(defaultConstructed.GetDescription(), "");
  EXPECT_TRUE(Contains(PrintToString(defaultConstructed), "itk::" + expectedName));

  const std::string testDescription = "test description";
  const std::string testLocation = "test location";
  const TException  exception(__FILE__, __LINE__, testDescription, testLocation);
  EXPECT_EQ(exception.GetNameOfClass(), expectedName);
  EXPECT_EQ(exception.GetDescription(), testDescription);
  EXPECT_EQ(exception.GetLocation(), testLocation);
  EXPECT_TRUE(Contains(exception.what(), testDescription));

  const std::string printed = PrintToString(exception);
  EXPECT_TRUE(Contains(printed, "itk::" + expectedName));
  EXPECT_TRUE(Contains(printed, "Description: " + testDescription));

  const TException copy(exception);
  EXPECT_EQ(copy, exception);

  TException assigned;
  assigned = exception;
  EXPECT_EQ(assigned, exception);

  const itk::ExceptionObject & base = exception;
  EXPECT_EQ(base.GetNameOfClass(), expectedName);
}


// Checks a DataObjectError type, both without and with a data object attached.
template <typename TException>
void
CheckDataObjectError(const std::string & expectedName)
{
  static_assert(std::is_base_of_v<itk::DataObjectError, TException>);

  TException defaultConstructed;
  EXPECT_EQ(defaultConstructed.GetNameOfClass(), expectedName);
  EXPECT_EQ(defaultConstructed.GetDataObject(), nullptr);
  EXPECT_EQ(std::as_const(defaultConstructed).GetDataObject(), nullptr);

  const std::string printedWithoutDataObject = PrintToString(defaultConstructed);
  EXPECT_TRUE(Contains(printedWithoutDataObject, "itk::" + expectedName));
  EXPECT_TRUE(Contains(printedWithoutDataObject, "Data object: (None)"));

  const auto image = itk::Image<float, 2>::New();
  TException exception(__FILE__, __LINE__);
  exception.SetDataObject(image);
  EXPECT_EQ(exception.GetDataObject(), image.get());
  EXPECT_EQ(std::as_const(exception).GetDataObject(), image.get());

  const std::string printedWithDataObject = PrintToString(exception);
  EXPECT_TRUE(Contains(printedWithDataObject, "itk::" + expectedName));
  EXPECT_TRUE(Contains(printedWithDataObject, "Data object:"));
  EXPECT_FALSE(Contains(printedWithDataObject, "(None)"));
  EXPECT_TRUE(Contains(printedWithDataObject, "LargestPossibleRegion"));

  const TException copy(exception);
  EXPECT_EQ(copy.GetDataObject(), image.get());

  TException assigned;
  assigned = exception;
  EXPECT_EQ(assigned.GetDataObject(), image.get());

  const itk::DataObjectError & base = exception;
  EXPECT_EQ(base.GetNameOfClass(), expectedName);
  EXPECT_EQ(base.GetDataObject(), image.get());
}
} // namespace


TEST(CommonExceptions, InheritedConstructors)
{
  CheckExceptionWithInheritedConstructors<itk::MemoryAllocationError>("MemoryAllocationError");
  CheckExceptionWithInheritedConstructors<itk::RangeError>("RangeError");
  CheckExceptionWithInheritedConstructors<itk::InvalidArgumentError>("InvalidArgumentError");
  CheckExceptionWithInheritedConstructors<itk::IncompatibleOperandsError>("IncompatibleOperandsError");
  CheckExceptionWithInheritedConstructors<itk::DerefError>("DerefError");
}


TEST(CommonExceptions, ProcessAborted)
{
  constexpr const char * expectedDescription = "Filter execution was aborted by an external request";

  const itk::ProcessAborted defaultConstructed;
  EXPECT_STREQ(defaultConstructed.GetNameOfClass(), "ProcessAborted");
  EXPECT_STREQ(defaultConstructed.GetDescription(), expectedDescription);

  const unsigned int        line = __LINE__;
  const itk::ProcessAborted exception(__FILE__, line);
  EXPECT_STREQ(exception.GetDescription(), expectedDescription);
  EXPECT_STREQ(exception.GetFile(), __FILE__);
  EXPECT_EQ(exception.GetLine(), line);

  const std::string printed = PrintToString(exception);
  EXPECT_TRUE(Contains(printed, "itk::ProcessAborted"));
  EXPECT_TRUE(Contains(printed, expectedDescription));
}


TEST(CommonExceptions, DataObjectErrors)
{
  CheckDataObjectError<itk::DataObjectError>("DataObjectError");
  CheckDataObjectError<itk::InvalidRequestedRegionError>("InvalidRequestedRegionError");
}
