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

#ifndef itkTestingMacros_h
#define itkTestingMacros_h

#include "itkMacro.h"

#include <cstring>

/** \namespace itk
 * \brief The "itk" namespace contains all Insight Segmentation and
 * Registration Toolkit (ITK) classes. There are several nested namespaces
 * within the itk:: namespace. */
namespace itk
{
// end namespace itk - this is here for documentation purposes
}

// DEPRECATED: These macros are left here for compatibility.
// In the future, they will be removed in favor of the "ITK_" prefixed
// versions.
#define EXERCISE_BASIC_OBJECT_METHODS \
  static_assert(false, "Replace EXERCISE_BASIC_OBJECT_METHODS with ITK_EXERCISE_BASIC_OBJECT_METHODS")
#define TRY_EXPECT_EXCEPTION static_assert(false, "Replace TRY_EXPECT_EXCEPTION with ITK_TRY_EXPECT_EXCEPTION")
#define TRY_EXPECT_NO_EXCEPTION static_assert(fasle, "Replace TRY_EXPECT_NO_EXCEPTION with ITK_TRY_EXPECT_NO_EXCEPTION")
#define TEST_EXPECT_TRUE_STATUS_VALUE \
  static_assert(false, "Replace TEST_EXPECT_TRUE_STATUS_VALUE with ITK_TEST_EXPECT_TRUE_STATUS_VALUE")
#define TEST_EXPECT_TRUE static_assert(false, "Replace TEST_EXPECT_TRUE with ITK_TEST_EXPECT_TRUE")
#define TEST_EXPECT_EQUAL_STATUS_VALUE "Replace TEST_EXPECT_EQUAL_STATUS_VALUE with ITK_TEST_EXPECT_EQUAL_STATUS_VALUE"
#define TEST_EXPECT_EQUAL static_assert(false, "Replace TEST_EXPECT_EQUAL with ITK_TEST_EXPECT_EQUAL")
#define TEST_SET_GET static_assert(false, "Replace TEST_SET_GET with ITK_TEST_SET_GET")
#define TEST_SET_GET_VALUE static_assert(false, "Replace TEST_SET_GET_VALUE with ITK_TEST_SET_GET_VALUE")
#define TEST_SET_GET_NULL_VALUE static_assert(false, "Replace TEST_SET_GET_NULL_VALUE with ITK_TEST_SET_GET_NULL_VALUE")
#define TEST_SET_GET_BOOLEAN static_assert(false, "Replace TEST_SET_GET_BOOLEAN with ITK_TEST_SET_GET_BOOLEAN")


/* clang-format off */
#if defined(__GNUC__)
#define ITK_EXERCISE_BASIC_OBJECT_METHODS(object, ClassName, SuperclassName)                                           \
  object->Print(std::cout);                                                                                            \
  std::cout << "Name of Class = " << object->GetNameOfClass() << '\n';                                            \
  ITK_MACROEND_NOOP_STATEMENT
#else // not GCC
#define ITK_EXERCISE_BASIC_OBJECT_METHODS(object, ClassName, SuperclassName)                                           \
  object->Print(std::cout);                                                                                            \
  std::cout << "Name of Class = " << object->Self::GetNameOfClass() << '\n';                                            \
  std::cout << "Name of Superclass = " << object->Superclass::GetNameOfClass() << '\n';                           \
  if (!std::strcmp(object->Self::GetNameOfClass(), #ClassName))                                                              \
  {                                                                                                                    \
    std::cout << "Class name is correct" << '\n';                                                                 \
  }                                                                                                                    \
  else                                                                                                                 \
  {                                                                                                                    \
    std::cerr << "Class name provided does not match object's NameOfClass" << '\n';                               \
    return EXIT_FAILURE;                                                                                               \
  }                                                                                                                    \
  if (!std::strcmp(object->Superclass::GetNameOfClass(), #SuperclassName))                                             \
  {                                                                                                                    \
    std::cout << "Superclass name is correct" << '\n';                                                            \
  }                                                                                                                    \
  else                                                                                                                 \
  {                                                                                                                    \
    std::cerr << "Superclass name provided does not match object's Superclass::NameOfClass" << '\n';              \
    return EXIT_FAILURE;                                                                                               \
  }                                                                                                                    \
  ITK_MACROEND_NOOP_STATEMENT
#endif // GCC
/* clang-format on */

#define ITK_TRY_EXPECT_EXCEPTION(command)                        \
  try                                                            \
  {                                                              \
    std::cout << "Trying " << #command << '\n';                  \
    command;                                                     \
    std::cerr << "Failed to catch expected exception" << '\n';   \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << '\n'; \
    return EXIT_FAILURE;                                         \
  }                                                              \
  catch (const itk::ExceptionObject & excp)                      \
  {                                                              \
    std::cout << "Caught expected exception" << '\n';            \
    std::cout << excp << '\n';                                   \
  }                                                              \
  ITK_MACROEND_NOOP_STATEMENT


#define ITK_TRY_EXPECT_NO_EXCEPTION(command)                     \
  try                                                            \
  {                                                              \
    std::cout << "Trying " << #command << '\n';                  \
    command;                                                     \
  }                                                              \
  catch (const itk::ExceptionObject & excp)                      \
  {                                                              \
    std::cerr << excp << '\n';                                   \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << '\n'; \
    return EXIT_FAILURE;                                         \
  }                                                              \
  ITK_MACROEND_NOOP_STATEMENT

#define ITK_TEST_EXPECT_TRUE_STATUS_VALUE(command, statusVal)              \
  {                                                                        \
    ITK_GCC_PRAGMA_PUSH                                                    \
    ITK_GCC_SUPPRESS_Wfloat_equal                                          \
    bool _ITK_TEST_EXPECT_TRUE_command(command);                           \
    ITK_GCC_PRAGMA_POP                                                     \
    if (!(_ITK_TEST_EXPECT_TRUE_command))                                  \
    {                                                                      \
      std::cerr << "Error in " << #command << '\n';                        \
      std::cerr << "  In " __FILE__ ", line " << __LINE__ << '\n';         \
      std::cerr << "Expected true" << '\n';                                \
      std::cerr << "  but got  " << _ITK_TEST_EXPECT_TRUE_command << '\n'; \
      statusVal = EXIT_FAILURE;                                            \
    }                                                                      \
  }                                                                        \
  ITK_MACROEND_NOOP_STATEMENT

#define ITK_TEST_EXPECT_TRUE(command)                                      \
  {                                                                        \
    ITK_GCC_PRAGMA_PUSH                                                    \
    ITK_GCC_SUPPRESS_Wfloat_equal                                          \
    bool _ITK_TEST_EXPECT_TRUE_command(command);                           \
    ITK_GCC_PRAGMA_POP                                                     \
    if (!(_ITK_TEST_EXPECT_TRUE_command))                                  \
    {                                                                      \
      std::cerr << "Error in " << #command << '\n';                        \
      std::cerr << "  In " __FILE__ ", line " << __LINE__ << '\n';         \
      std::cerr << "Expected true" << '\n';                                \
      std::cerr << "  but got  " << _ITK_TEST_EXPECT_TRUE_command << '\n'; \
      return EXIT_FAILURE;                                                 \
    }                                                                      \
  }                                                                        \
  ITK_MACROEND_NOOP_STATEMENT


#define ITK_TEST_EXPECT_EQUAL_STATUS_VALUE(lh, rh, statusVal)      \
  {                                                                \
    ITK_GCC_PRAGMA_PUSH                                            \
    ITK_GCC_SUPPRESS_Wfloat_equal                                  \
    bool _ITK_TEST_EXPECT_EQUAL_result((lh) == (rh));              \
    ITK_GCC_PRAGMA_POP                                             \
    if (!(_ITK_TEST_EXPECT_EQUAL_result))                          \
    {                                                              \
      std::cerr << "Error in " << #lh << " == " << #rh << '\n';    \
      std::cerr << "\tIn " __FILE__ ", line " << __LINE__ << '\n'; \
      std::cerr << "\tlh: " << (lh) << '\n';                       \
      std::cerr << "\trh: " << (rh) << '\n';                       \
      std::cerr << "Expression is not equal" << '\n';              \
      statusVal = EXIT_FAILURE;                                    \
    }                                                              \
  }                                                                \
  ITK_MACROEND_NOOP_STATEMENT

#define ITK_TEST_EXPECT_EQUAL(lh, rh)                              \
  {                                                                \
    ITK_GCC_PRAGMA_PUSH                                            \
    ITK_GCC_SUPPRESS_Wfloat_equal                                  \
    bool _ITK_TEST_EXPECT_EQUAL_result((lh) == (rh));              \
    ITK_GCC_PRAGMA_POP                                             \
    if (!(_ITK_TEST_EXPECT_EQUAL_result))                          \
    {                                                              \
      std::cerr << "Error in " << #lh << " == " << #rh << '\n';    \
      std::cerr << "\tIn " __FILE__ ", line " << __LINE__ << '\n'; \
      std::cerr << "\tlh: " << (lh) << '\n';                       \
      std::cerr << "\trh: " << (rh) << '\n';                       \
      std::cerr << "Expression is not equal" << '\n';              \
      return EXIT_FAILURE;                                         \
    }                                                              \
  }                                                                \
  ITK_MACROEND_NOOP_STATEMENT


#define ITK_TEST_SET_GET(variable, command)                      \
  if (variable != command)                                       \
  {                                                              \
    std::cerr << "Error in " << #command << '\n';                \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << '\n'; \
    std::cerr << "Expected " << variable.GetPointer() << '\n';   \
    std::cerr << "but got  " << command << '\n';                 \
    return EXIT_FAILURE;                                         \
  }                                                              \
  ITK_MACROEND_NOOP_STATEMENT


#define ITK_TEST_SET_GET_VALUE(variable, command)                \
  ITK_GCC_PRAGMA_PUSH                                            \
  ITK_GCC_SUPPRESS_Wfloat_equal                                  \
  if (variable != command)                                       \
  {                                                              \
    std::cerr << "Error in " << #command << '\n';                \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << '\n'; \
    std::cerr << "Expected " << variable << '\n';                \
    std::cerr << "but got  " << command << '\n';                 \
    return EXIT_FAILURE;                                         \
  }                                                              \
  ITK_GCC_PRAGMA_POP                                             \
  ITK_MACROEND_NOOP_STATEMENT

#define ITK_TEST_SET_GET_NULL_VALUE(command)                     \
  if (nullptr != command)                                        \
  {                                                              \
    std::cerr << "Error in " << #command << '\n';                \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << '\n'; \
    std::cerr << "Expected "                                     \
              << "nullptr" << '\n';                              \
    std::cerr << "but got  " << command << '\n';                 \
    return EXIT_FAILURE;                                         \
  }                                                              \
  ITK_MACROEND_NOOP_STATEMENT

#define ITK_TEST_SET_GET_BOOLEAN(object, variable, value)                                               \
  object->Set##variable(false);                                                                         \
  object->Set##variable(true);                                                                          \
  if (object->Get##variable() != 1)                                                                     \
  {                                                                                                     \
    std::cerr << "Error in Set/Get" #variable << ", Get" #variable << " is " << object->Get##variable() \
              << " instead of 1" << '\n';                                                               \
    return EXIT_FAILURE;                                                                                \
  }                                                                                                     \
  object->Set##variable(false);                                                                         \
  if (object->Get##variable() != 0)                                                                     \
  {                                                                                                     \
    std::cerr << "Error in Set/Get" #variable << ", Get" #variable << " is " << object->Get##variable() \
              << " instead of 0" << '\n';                                                               \
    return EXIT_FAILURE;                                                                                \
  }                                                                                                     \
  object->variable##On();                                                                               \
  if (object->Get##variable() != 1)                                                                     \
  {                                                                                                     \
    std::cerr << "Error in On/Get" #variable << ", Get" #variable << " is " << object->Get##variable()  \
              << " instead of 1" << '\n';                                                               \
    return EXIT_FAILURE;                                                                                \
  }                                                                                                     \
  object->variable##Off();                                                                              \
  if (object->Get##variable() != 0)                                                                     \
  {                                                                                                     \
    std::cerr << "Error in Off/Get" #variable << ", Get" #variable << " is " << object->Get##variable() \
              << " instead of 0" << '\n';                                                               \
    return EXIT_FAILURE;                                                                                \
  }                                                                                                     \
  object->Set##variable(value)

/** The name of the executable, argv[0], is not always available as argv[0] may be null.
 * In that case, use the name of the test function.
 */
#define itkNameOfTestExecutableMacro(argv)                                                                             \
  [argv](const std::string & functionName) {                                                                           \
    return ((argv == nullptr) || (argv[0] == nullptr) || (argv[0][0] == '\0')) ? ("<" + functionName + " executable>") \
                                                                               : argv[0];                              \
  }(__func__)

#endif
