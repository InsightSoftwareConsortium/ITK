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

#include <iostream>
#include <cmath>
#include "itkMath.h"
#include "itkRealTimeClock.h"
#include "itkStdStreamStateSave.h"
#include "itkTestingMacros.h"

int
itkRealTimeClockTest(int, char *[])
{
  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope
  // scope.
  const itk::StdStreamStateSave coutState(std::cout);

  try
  {

    // Create an ITK RealTimeClock
    const itk::RealTimeClock::Pointer clock = itk::RealTimeClock::New();
    ITK_EXERCISE_BASIC_OBJECT_METHODS(clock, RealTimeClock, Object);

    std::cout << "Testing itk::RealTimeClock" << '\n';
    std::cout << "Frequency: " << clock->GetFrequency() << '\n';

    itk::RealTimeStamp timestamps[5];

    std::cout << "Printing timestamps got one by one" << '\n';

    for (unsigned int i = 0; i < 5; ++i)
    {
      std::cout << clock->GetRealTimeStamp() << '\n';
    }

    for (auto & timestamp : timestamps)
    {
      timestamp = clock->GetRealTimeStamp();
    }

    std::cout << "Printing timestamps buffered" << '\n';
    for (const auto & timestamp : timestamps)
    {
      std::cout << timestamp << '\n';
    }

    // Print out several time stamps
    itk::RealTimeStamp realStamp1 = clock->GetRealTimeStamp();
    itk::RealTimeStamp realStamp2 = clock->GetRealTimeStamp();
    std::cout << "Current Time " << realStamp2 << '\n';

    using TimeRepresentationType = itk::RealTimeStamp::TimeRepresentationType;

    const TimeRepresentationType tolerance = 1e6;

    for (unsigned int i = 0; i < 10; ++i)
    {
      realStamp1 = realStamp2;
      realStamp2 = clock->GetRealTimeStamp();
      const itk::RealTimeInterval                      difference = realStamp2 - realStamp1;
      const itk::RealTimeStamp::TimeRepresentationType seconds1 = realStamp1.GetTimeInSeconds();
      const itk::RealTimeStamp::TimeRepresentationType seconds2 = realStamp2.GetTimeInSeconds();
      const itk::RealTimeStamp::TimeRepresentationType secondsD = difference.GetTimeInSeconds();
      const itk::RealTimeStamp::TimeRepresentationType secondsE = seconds2 - seconds1;
      std::cout << realStamp2 << " - " << realStamp1 << " = ";
      std::cout << secondsD << " = " << secondsE << '\n';

      if (itk::Math::abs(secondsD - secondsE) / secondsE > tolerance)
      {
        std::cerr << "Precision error in time difference" << '\n';
        std::cerr << "Expected " << secondsE << " seconds " << '\n';
        std::cerr << "But got  " << secondsD << " seconds " << '\n';
        return EXIT_FAILURE;
      }
    }
  }
  catch (...)
  {
    std::cerr << "Exception caught !!" << '\n';
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << '\n';
  return EXIT_SUCCESS;
}
