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


// object's Class must be specified to build on sun studio
#define EXERCISE_BASIC_OBJECT_METHODS( object, Class ) \
    object->Print( std::cout );  \
    std::cout << "Name of Class = " << object->GetNameOfClass() << std::endl; \
    std::cout << "Name of Superclass = " << object->Class::Superclass::GetNameOfClass() << std::endl;


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

#define TEST_EXPECT_TRUE( command )                                     \
  {                                                                     \
  bool _TEST_EXPECT_TRUE_command(command);                              \
  if( !(_TEST_EXPECT_TRUE_command) )                                    \
    {                                                                   \
    std::cerr << "Error in " << #command << std::endl;                  \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "Expected true" << std::endl;                          \
    std::cerr << "  but got  " <<  _TEST_EXPECT_TRUE_command << std::endl; \
    return EXIT_FAILURE;                                                \
    }                                                                   \
  }

#define TEST_EXPECT_EQUAL( lh, rh )                                     \
  {                                                                     \
    bool _TEST_EXPECT_EQUAL_result((lh) == (rh));                       \
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
  if( variable != command )   \
    {   \
    std::cerr << "Error in " << #command << std::endl; \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "Expected " << variable << std::endl;   \
    std::cerr << "but got  " << command << std::endl; \
    return EXIT_FAILURE; \
    }

#define TEST_SET_GET_NULL_VALUE( command ) \
  if( NULL != command )   \
    {   \
    std::cerr << "Error in " << #command << std::endl; \
    std::cerr << "  In " __FILE__ ", line " << __LINE__ << std::endl;   \
    std::cerr << "Expected " << "NULL" << std::endl;   \
    std::cerr << "but got  " << command << std::endl; \
    return EXIT_FAILURE; \
    }


#endif
