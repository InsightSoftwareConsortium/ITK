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
#include "itkFloatingPointExceptions.h"
#include <iostream>
#include <float.h>

// constants declared in another compilation unit to prevent
// compilation time errors for divide by zero and overflow
extern const double itkFloatingPointExceptionsTest_double_zero;
extern const double itkFloatingPointExceptionsTest_double_max;
extern const int itkFloatingPointExceptionsTest_int_zero;


int
itkFloatingPointExceptionsTest(int argc, char *argv[] )
{
  itk::FloatingPointExceptions::Enable();
  itk::FloatingPointExceptions::
    SetExceptionAction(itk::FloatingPointExceptions::EXIT);
  if(argc < 2)
    {
    std::cout << "No test specified" << std::endl;
    return 1;
    }
  int error_return(0);
  double double_zero = itkFloatingPointExceptionsTest_double_zero;
  double double_max = itkFloatingPointExceptionsTest_double_max;
  double test1 = itkFloatingPointExceptionsTest_double_zero;
  int int_zero = itkFloatingPointExceptionsTest_int_zero;

  std::string testName(argv[1]);
  if(testName == "DivByZero")
    {
    std::cout << "Testing floating point divide by zero" << std::endl;
    std::cout.flush();
    try
      {
      double s = 1.0 / double_zero;
      //
      // should never reach here
      std::cout << "Divide by Zero Exception not caught"
                << " result is " << s << std::endl;
      error_return++;
      }
    catch (itk::ExceptionObject &e)
      {
      std::cout << "Expected divide by zero exception caught" << std::endl;
      std::cout << e;
      std::cout.flush();
      }
    }
  if(testName == "ZeroDivByZero")
    {
    std::cout << "Testing floating point zero divided by zero" << std::endl;
    std::cout.flush();
    try
      {
      double s = test1 / double_zero;
      //
      // should never reach here
      std::cout << "Zero divide by Zero Exception not caught"
                << " result is " << s << std::endl;
      error_return++;
      }
    catch (itk::ExceptionObject &e)
      {
      std::cout << "Expected 0.0 / 0.0  exception caught" << std::endl;
      std::cout << e;
      std::cout.flush();
      }
    }
  if(testName == "FPOverFlow")
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
    catch (itk::ExceptionObject &e)
      {
      std::cout << "Overflow exception caught" << std::endl;
      std::cout << e;
      std::cout.flush();
      }
    }
  if(testName == "FPUnderFlow")
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
    catch (itk::ExceptionObject &e)
      {
      std::cout << "Underflow exception caught" << std::endl;
      std::cout << e;
      }
    }

  if(testName == "IntDivByZero")
    {
    std::cout << "Testing integer divide by zero" << std::endl;
    std::cout.flush();
    try
      {
      int s = 1 / int_zero;
      //
      // should never reach here
      std::cout << "Integer divide by zero Exception not caught"
                << " result is " << s << std::endl;
      error_return++;
      }
    catch (itk::ExceptionObject &e)
      {
      std::cout << "Integer divide by zero  exception caught" << std::endl;
      std::cout << e;
      std::cout.flush();
      }
    }
  return error_return;
}
