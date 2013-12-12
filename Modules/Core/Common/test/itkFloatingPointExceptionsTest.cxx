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
  double force_zero_denom(0.0),test1(0.0);
  int force_int_zero(0);
  // this will fool the compiler into not complaining at
  // compile time about divide by zero
  if(argc > 32767)
    {
    force_zero_denom = 1.0;
    test1 = 1.0;
    force_int_zero = 1;
    }

  std::string testName(argv[1]);
  if(testName == "DivByZero")
    {
    std::cout << "Testing floating point divide by zero" << std::endl;
    std::cout.flush();
    try
      {
      double s = 1.0 / force_zero_denom;
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
      double s = test1 / force_zero_denom;
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
      double s = DBL_MAX;
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
      s = s / DBL_MAX;
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
      int s = 1 / force_int_zero;
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
