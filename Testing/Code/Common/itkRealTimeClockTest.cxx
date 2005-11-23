/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRealTimeClockTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
      std::cout << clock->GetTimestamp() << std::endl;
      }

    for( i = 0; i < 5; ++i )
      {
      timestamps[i] = clock->GetTimestamp();
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


