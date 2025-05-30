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
#include "itkFloatingPointExceptions.h"
#include <iostream>
#include <cfloat>
#include <set>

// constants declared in another compilation unit to prevent
// compilation time errors for divide by zero and overflow
extern const double itkFloatingPointExceptionsTest_double_zero;
extern const double itkFloatingPointExceptionsTest_double_max;


int
itkFloatingPointExceptionsTest(int argc, char * argv[])
{
  itk::FloatingPointExceptions::Enable();
  itk::FloatingPointExceptions::SetExceptionAction(itk::FloatingPointExceptions::ExceptionActionEnum::EXIT);
  if (argc < 2)
  {
    std::cout << "No test specified" << std::endl;
    return 1;
  }
  int          error_return(0);
  const double double_zero = itkFloatingPointExceptionsTest_double_zero;
  const double double_max = itkFloatingPointExceptionsTest_double_max;
  const double test1 = itkFloatingPointExceptionsTest_double_zero;

  const std::string testName(argv[1]);
  if (testName == "DivByZero")
  {
    std::cout << "Testing floating point divide by zero" << std::endl;
    std::cout.flush();
    try
    {
      const double s = 1.0 / double_zero;
      //
      // should never reach here
      std::cout << "Divide by Zero Exception not caught"
                << " result is " << s << std::endl;
      error_return++;
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cout << "Expected divide by zero exception caught" << std::endl;
      std::cout << e;
      std::cout.flush();
    }
  }
  if (testName == "ZeroDivByZero")
  {
    std::cout << "Testing floating point zero divided by zero" << std::endl;
    std::cout.flush();
    try
    {
      const double s = test1 / double_zero;
      //
      // should never reach here
      std::cout << "Zero divide by Zero Exception not caught"
                << " result is " << s << std::endl;
      error_return++;
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cout << "Expected 0.0 / 0.0  exception caught" << std::endl;
      std::cout << e;
      std::cout.flush();
    }
  }
  if (testName == "FPOverflow")
  {
    std::cout << "Testing floating point overflow" << std::endl;
    std::cout.flush();
    try
    {
      double s = double_max;
      s = s * s;
      //
      // should never reach here
      std::cout << "Overflow Exception not caught"
                << " result is " << s << std::endl;
      error_return++;
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cout << "Overflow exception caught" << std::endl;
      std::cout << e;
      std::cout.flush();
    }
  }
  if (testName == "FPUnderflow")
  {
    std::cout << "Testing floating point underflow" << std::endl;
    std::cout.flush();
    // not caught as SIGFPE apparently
    try
    {
      double s = DBL_MIN;
      s = s / double_max;
      //
      // should never reach here
      std::cout << "Underflow Exception not caught"
                << " result is " << s << std::endl;
      error_return++;
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cout << "Underflow exception caught" << std::endl;
      std::cout << e;
    }
  }

  // Test streaming enumeration for FloatingPointExceptionsEnums::ExceptionAction elements
  const std::set<itk::FloatingPointExceptionsEnums::ExceptionAction> allExceptionAction{
    itk::FloatingPointExceptionsEnums::ExceptionAction::ABORT, itk::FloatingPointExceptionsEnums::ExceptionAction::EXIT
  };
  for (const auto & ee : allExceptionAction)
  {
    std::cout << "STREAMED ENUM VALUE FloatingPointExceptionsEnums::ExceptionAction: " << ee << std::endl;
  }

  return error_return;
}
