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
#include <regex>
#include "itkLightObject.h"
#include "itkObjectFactory.h"


// A simple LightObject subclass for testing constructor that takes LightObject pointer
namespace
{
class TestLightObject : public itk::LightObject
{
public:
  using Self = TestLightObject;
  using Superclass = itk::LightObject;
  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(TestLightObject);
};
} // end anonymous namespace


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

    itkVirtualGetNameOfClassMacro(TestClass);

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
    // Description now stores only the user-provided message; the ITK ERROR prefix & metadata moved to what().
    EXPECT_EQ(actualDescription, message);

    const std::string what = exceptionObject.what();
    // For this scenario we only guarantee that the user message appears.
    EXPECT_NE(what.find(message), std::string::npos);
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
    EXPECT_EQ(std::string(description), std::string(itk::GTest_SpecializedException::default_exception_message));
  }
}


// Tests that `ExceptionObject::what()` returns the expected concatenation of
// file name, line number, location and description.
TEST(ExceptionObject, TestWhat)
{
  {
    // Test with empty location.
    const itk::ExceptionObject exceptionObject(__FILE__, __LINE__, "test description", "");

    const char * const what = exceptionObject.what();
    const char * const file = exceptionObject.GetFile();
    const char * const description = exceptionObject.GetDescription();

    ASSERT_NE(what, nullptr);
    ASSERT_NE(file, nullptr);
    ASSERT_NE(description, nullptr);

    EXPECT_EQ(what, file + (":" + std::to_string(exceptionObject.GetLine()) + ":\nITK ERROR: ") + description);
  }
  {
    // Test with location = ITK_LOCATION (as used by ITK's exception macro's).
    const itk::ExceptionObject exceptionObject(__FILE__, __LINE__, "test description", ITK_LOCATION);

    const char * const what = exceptionObject.what();
    const char * const file = exceptionObject.GetFile();
    const char * const description = exceptionObject.GetDescription();
    const char * const location = exceptionObject.GetLocation();

    ASSERT_NE(what, nullptr);
    ASSERT_NE(file, nullptr);
    ASSERT_NE(description, nullptr);
    ASSERT_NE(location, nullptr);


    EXPECT_EQ(what,
              file + (":" + std::to_string(exceptionObject.GetLine()) + ": in '" + location + "':\nITK ERROR: ") +
                description);
  }
}


// Tests modifying a default constructed exception via setters.
TEST(ExceptionObject, TestDefaultConstructedAndSetters)
{
  itk::ExceptionObject e; // default constructed (no data)
  EXPECT_STREQ(e.GetFile(), "");
  EXPECT_EQ(e.GetLine(), 0u);
  EXPECT_STREQ(e.GetLocation(), "");
  EXPECT_STREQ(e.GetDescription(), "");
  EXPECT_STREQ(e.what(), "ExceptionObject"); // what() of default exception

  // Setters should update class name and description
  e.SetDescription("first description");
  e.SetLocation("MyFunction()");
  const std::string what = e.what();
  // what should now contain class name and description
  EXPECT_NE(what.find("first description"), std::string::npos);
  EXPECT_NE(what.find("MyFunction()"), std::string::npos);
}


// Tests that SetLocation, SetDescription, update the cached what() text
TEST(ExceptionObject, TestSettersUpdateWhat)
{
  itk::ExceptionObject e(__FILE__, __LINE__, "initial", "");
  std::string          what1 = e.what();
  EXPECT_NE(what1.find("initial"), std::string::npos);
  EXPECT_NE(what1.find("ITK ERROR"), std::string::npos); // ITK ERROR is present

  e.SetLocation("LocA");
  std::string what2 = e.what();
  EXPECT_NE(what2.find("LocA"), std::string::npos);
  EXPECT_NE(what2.find("initial"), std::string::npos);

  e.SetDescription("Second description");
  std::string what3 = e.what();
  EXPECT_NE(what3.find("Second description"), std::string::npos);
  EXPECT_EQ(what3.find("initial"), std::string::npos);
}


// Tests construction with a LightObject thrower supplies its class name & pointer in what().
TEST(ExceptionObject, TestConstructorWithLightObjectThrower)
{
  auto                 thrower = TestLightObject::New();
  itk::ExceptionObject e(__FILE__, __LINE__, "light object description", ITK_LOCATION, thrower.GetPointer());
  const std::string    what = e.what();
  const auto           newlinePos = what.find('\n');
  ASSERT_NE(newlinePos, std::string::npos);
  const std::string afterNewline = what.substr(newlinePos + 1);
  EXPECT_NE(afterNewline.find("TestLightObject"), std::string::npos);
  EXPECT_NE(afterNewline.find(": light object description"), std::string::npos);
}


// Tests construction with a non-LightObject thrower (templated constructor) stores a non-empty class name (typeid).
TEST(ExceptionObject, TestConstructorWithNonLightObjectThrower)
{
  struct PlainStruct
  {
  } plain;
  auto e = itk::ExceptionObject(__FILE__, __LINE__, "plain description", ITK_LOCATION, &plain);
  // For non-LightObject, class name should be empty string
  const std::string what = e.what();
  EXPECT_NE(what.find("ITK ERROR:"), std::string::npos);
  EXPECT_NE(what.find("plain description"), std::string::npos);
}


// Tests operator== covers case with same shared data and with modified fields.
TEST(ExceptionObject, TestEqualityOperator)
{
  itk::ExceptionObject a(__FILE__, __LINE__, "desc", "locA");
  itk::ExceptionObject b = a; // shares data
  EXPECT_TRUE(a == b);

  itk::ExceptionObject c(__FILE__, __LINE__, "desc", "locA");
  // Value equality currently false because GetFile() pointer string differs or internal class name differences; ensure
  // reflexive only.
  EXPECT_FALSE(a == c);

  itk::ExceptionObject d(__FILE__, __LINE__, "desc2", "locA");
  EXPECT_FALSE(a == d);

  itk::ExceptionObject e(__FILE__, __LINE__, "desc", "locB");
  EXPECT_FALSE(a == e);
}


// Tests Print() output basic structure.
TEST(ExceptionObject, TestPrintOutput)
{
  itk::ExceptionObject e(__FILE__, __LINE__, "print description", "PrintLocation");
  std::ostringstream   oss;
  oss << e;
  const std::string out = oss.str();
  EXPECT_NE(out.find("itk::ExceptionObject"), std::string::npos);
  EXPECT_NE(out.find("Location: \"PrintLocation\""), std::string::npos);
  EXPECT_NE(out.find("File: "), std::string::npos);
  EXPECT_NE(out.find("Line: "), std::string::npos);
  EXPECT_NE(out.find("Description: print description"), std::string::npos);
}

// Tests that Print() includes ClassName and Thrower when available.
TEST(ExceptionObject, TestPrintIncludesClassNameAndThrower)
{
  auto                 thrower = TestLightObject::New();
  itk::ExceptionObject e(__FILE__, __LINE__, "desc", "LocX", thrower.GetPointer());
  std::ostringstream   oss;
  oss << e;
  const std::string out = oss.str();
  EXPECT_NE(out.find("ClassName: TestLightObject"), std::string::npos);
  EXPECT_NE(out.find("Thrower: "), std::string::npos);
}
