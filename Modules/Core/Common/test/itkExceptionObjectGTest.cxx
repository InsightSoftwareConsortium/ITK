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
#include "itkMacro.h"
#include <gtest/gtest.h>
#include <sstream>
#include <string>


itkDeclareExceptionMacro(GTest_SpecializedException,
                         ExceptionObject,
                         "GTest specific specialized exception (for unit test purposes only)");


// Tests the description of an exception object thrown by `itkExceptionMacro(x)`.
TEST(ExceptionObject, TestDescriptionFromExceptionMacro)
{
  class TestClass
  {
  public:
    ITK_DISALLOW_COPY_AND_MOVE(TestClass);
    TestClass() = default;
    virtual ~TestClass() = default;

    itkTypeMacroNoParent(TestClass);

    void
    CallExceptionMacro(const std::string & message) const
    {
      itkExceptionMacro(<< message);
    }
  };

  const std::string message = "test message";
  const TestClass   testObject{};

  try
  {
    testObject.CallExceptionMacro(message);
  }
  catch (const itk::ExceptionObject & exceptionObject)
  {
    const char * const actualDescription = exceptionObject.GetDescription();
    ASSERT_NE(actualDescription, nullptr);

    std::ostringstream expectedDescription;
    expectedDescription << "ITK ERROR: " << testObject.GetNameOfClass() << "(" << &testObject << "): " << message;

    EXPECT_EQ(actualDescription, expectedDescription.str());
  }
}


// Tests the description of an exception object thrown by `itkSpecializedExceptionMacro(x)`.
TEST(ExceptionObject, TestDescriptionFromSpecializedExceptionMacro)
{
  try
  {
    itkSpecializedExceptionMacro(GTest_SpecializedException);
  }
  catch (const itk::ExceptionObject & exceptionObject)
  {
    const char * const description = exceptionObject.GetDescription();
    ASSERT_NE(description, nullptr);
    EXPECT_EQ(description, std::string("ITK ERROR: ") + itk::GTest_SpecializedException::default_exception_message);
  }
}


// Tests that `ExceptionObject::what()` returns the expected concatenation of
// file name, line number, and description.
TEST(ExceptionObject, TestWhat)
{
  const itk::ExceptionObject exceptionObject(__FILE__, __LINE__, "test description");

  const char * const what = exceptionObject.what();
  const char * const file = exceptionObject.GetFile();
  const char * const description = exceptionObject.GetDescription();

  ASSERT_NE(what, nullptr);
  ASSERT_NE(file, nullptr);
  ASSERT_NE(description, nullptr);

  EXPECT_EQ(what, file + (":" + std::to_string(exceptionObject.GetLine()) + ":\n") + description);
}
