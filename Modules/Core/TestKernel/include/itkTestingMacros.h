/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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


// object's Class must be specified to build on sun studio
#define EXERCISE_BASIC_OBJECT_METHODS( object, Class, SuperClass )        \
    object->Print( std::cout );                                           \
    std::cout << "Name of Class = " << object->GetNameOfClass() << std::endl; \
    std::cout << "Name of Superclass = " << object->Superclass::GetNameOfClass() << std::endl; \
    if( !std::strcmp(object->GetNameOfClass(), #Class) )                  \
      {                                                                   \
      std::cout << "Class name is correct" << std::endl;                  \
      }                                                                   \
    else                                                                  \
      {                                                                   \
      std::cerr << "Class name provided does not match object's NameOfClass" << std::endl; \
      return EXIT_FAILURE;                                                \
      }                                                                   \
    if( !std::strcmp(object->Superclass::GetNameOfClass(), #SuperClass) ) \
      {                                                                   \
      std::cout << "Superclass name is correct" << std::endl;             \
      }                                                                   \
    else                                                                  \
      {                                                                   \
      std::cerr << "Superclass name provided does not match object's Superclass::NameOfClass" << std::endl; \
      return EXIT_FAILURE;                                                \
      }

#define TRY_EXPECT_EXCEPTION( command ) \
  try \
    {  \
    std::cout << "Trying " << #command << std::endl; \
    command;  \
    std::cerr << "Failed to catch expected exception" << std::endl;  \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;\
    return EXIT_FAILURE;  \
    }  \
  catch( itk::ExceptionObject & excp )  \
    {  \
    std::cout << "Caught expected exception" << std::endl;  \
    std::cout << excp << std::endl; \
    }


#define TRY_EXPECT_NO_EXCEPTION( command ) \
  try \
    {  \
    std::cout << "Trying " << #command << std::endl; \
    command;  \
    }  \
  catch( itk::ExceptionObject & excp )  \
    {  \
    std::cerr << excp << std::endl; \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    return EXIT_FAILURE;  \
    }

#define TEST_EXPECT_TRUE_STATUS_VALUE( command, statusVal )                                     \
  {                                                                     \
CLANG_PRAGMA_PUSH    \
CLANG_SUPPRESS_Wfloat_equal   \
  bool _TEST_EXPECT_TRUE_command(command);                              \
CLANG_PRAGMA_POP    \
  if( !(_TEST_EXPECT_TRUE_command) )                                    \
    {                                                                   \
    std::cerr << "Error in " << #command << std::endl;                  \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "Expected true" << std::endl;                          \
    std::cerr << "  but got  " <<  _TEST_EXPECT_TRUE_command << std::endl; \
    statusVal = EXIT_FAILURE;                                                \
    }                                                                   \
  }

#define TEST_EXPECT_TRUE( command )                                     \
  {                                                                     \
CLANG_PRAGMA_PUSH    \
CLANG_SUPPRESS_Wfloat_equal   \
  bool _TEST_EXPECT_TRUE_command(command);                              \
CLANG_PRAGMA_POP    \
  if( !(_TEST_EXPECT_TRUE_command) )                                    \
    {                                                                   \
    std::cerr << "Error in " << #command << std::endl;                  \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "Expected true" << std::endl;                          \
    std::cerr << "  but got  " <<  _TEST_EXPECT_TRUE_command << std::endl; \
    return EXIT_FAILURE;                                                \
    }                                                                   \
  }


#define TEST_EXPECT_EQUAL_STATUS_VALUE( lh, rh, statusVal )                                     \
  {                                                                     \
CLANG_PRAGMA_PUSH    \
CLANG_SUPPRESS_Wfloat_equal   \
    bool _TEST_EXPECT_EQUAL_result((lh) == (rh));                       \
CLANG_PRAGMA_POP    \
    if( !(_TEST_EXPECT_EQUAL_result) )                                  \
    {                                                                   \
    std::cerr << "Error in " << #lh << " == " << #rh << std::endl;      \
    std::cerr << "\tIn " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "\tlh: " << (lh) << std::endl;                         \
    std::cerr << "\trh: " << (rh) << std::endl;                         \
    std::cerr << "Expression is not equal" << std::endl;                \
    statusVal = EXIT_FAILURE;                                                \
    }                                                                   \
  }

#define TEST_EXPECT_EQUAL( lh, rh )                                     \
  {                                                                     \
CLANG_PRAGMA_PUSH    \
CLANG_SUPPRESS_Wfloat_equal   \
    bool _TEST_EXPECT_EQUAL_result((lh) == (rh));                       \
CLANG_PRAGMA_POP    \
    if( !(_TEST_EXPECT_EQUAL_result) )                                  \
    {                                                                   \
    std::cerr << "Error in " << #lh << " == " << #rh << std::endl;      \
    std::cerr << "\tIn " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "\tlh: " << (lh) << std::endl;                         \
    std::cerr << "\trh: " << (rh) << std::endl;                         \
    std::cerr << "Expression is not equal" << std::endl;                \
    return EXIT_FAILURE;                                                \
    }                                                                   \
  }


#define TEST_SET_GET( variable, command ) \
  if( variable.GetPointer() != command )   \
    {   \
    std::cerr << "Error in " << #command << std::endl; \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "Expected " << variable.GetPointer() << std::endl; \
    std::cerr << "but got  " << command << std::endl; \
    return EXIT_FAILURE; \
    }


#define TEST_SET_GET_VALUE( variable, command ) \
CLANG_PRAGMA_PUSH    \
CLANG_SUPPRESS_Wfloat_equal   \
  if( variable != command )   \
CLANG_PRAGMA_POP    \
    {   \
    std::cerr << "Error in " << #command << std::endl; \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "Expected " << variable << std::endl;   \
    std::cerr << "but got  " << command << std::endl; \
    return EXIT_FAILURE; \
    }

#define TEST_SET_GET_NULL_VALUE( command ) \
  if( ITK_NULLPTR != command )   \
    {   \
    std::cerr << "Error in " << #command << std::endl; \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "Expected " << "ITK_NULLPTR" << std::endl;   \
    std::cerr << "but got  " << command << std::endl; \
    return EXIT_FAILURE; \
    }

#define TEST_SET_GET_BOOLEAN( object, variable, value ) \
  object->Set##variable( false ); \
  object->Set##variable( true ); \
  if( object->Get##variable() != 1 ) \
  {   \
    std::cerr << "Error in Set/Get"#variable << ", Get"#variable << " is " << object->Get##variable() << " instead of 1" << std::endl; \
    return EXIT_FAILURE; \
  } \
  object->Set##variable( false ); \
  if( object->Get##variable() != 0 ) \
  {   \
    std::cerr << "Error in Set/Get"#variable << ", Get"#variable << " is " << object->Get##variable() << " instead of 0" << std::endl; \
    return EXIT_FAILURE; \
  } \
  object->variable##On(); \
  if( object->Get##variable() != 1 ) \
  {   \
    std::cerr << "Error in On/Get"#variable << ", Get"#variable << " is " << object->Get##variable() << " instead of 1" << std::endl; \
    return EXIT_FAILURE; \
  } \
  object->variable##Off(); \
  if( object->Get##variable() != 0 ) \
  {   \
    std::cerr << "Error in Off/Get"#variable << ", Get"#variable << " is " << object->Get##variable() << " instead of 0" << std::endl; \
    return EXIT_FAILURE; \
  } \
  object->Set##variable( value );
#endif
