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
#if defined(_MSC_VER)
   //Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
#pragma warning( disable : 4786 )
#endif

#include <iostream>
#include "itkRealTimeClock.h"

int itkRealTimeClockTest( int, char * [] )
{
  try
    {

    // Create an ITK RealTimeClock
    itk::RealTimeClock::Pointer clock = itk::RealTimeClock::New();

    std::cout << "Testing itk::RealTimeClock" << std::endl;
    std::cout << "Frequency: " << clock->GetFrequency() << std::endl;

    std::cout.precision(30);

    unsigned int i;

    typedef itk::RealTimeClock::TimeStampType  TimeStampType;

    TimeStampType timestamps[5];

    std::cout << "Printing timestamps got one by one" << std::endl;

    for( i = 0; i < 5; ++i )
      {
      std::cout << clock->GetTimeStamp() << std::endl;
      }

    for( i = 0; i < 5; ++i )
      {
      timestamps[i] = clock->GetTimeStamp();
      }

    std::cout << "Printing timestamps buffered" << std::endl;
    for( i = 0; i < 5; ++i )
      {
      std::cout << timestamps[i] << std::endl;
      }

    }
  catch(...)
    {
    std::cerr << "Exception catched !!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}


