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

#include <iostream>
#include "itkTimeProbe.h"
#include "itkMath.h"


int itkTimeProbeTest( int, char * [] )
{
  // Create an ITK time probe
  itk::TimeProbe localTimer;

  // Print the initial values
  std::cout << "Testing itk::TimeProbe" << std::endl;
  std::cout << "Type: " << localTimer.GetType() << std::endl;
  std::cout << "Unit: " << localTimer.GetUnit() << std::endl;

  std::cout << "NumberOfStarts: " << localTimer.GetNumberOfStarts() << std::endl;
  std::cout << "NumberOfStops: " << localTimer.GetNumberOfStops() << std::endl;
  std::cout << "InstantValue: " << localTimer.GetInstantValue() << std::endl;
  std::cout << "Total: " << localTimer.GetTotal() << std::endl;
  std::cout << "Mean: " << localTimer.GetMean() << std::endl;

  // time a task
  localTimer.Start();
  //usleep( 1000000 );//not portable
  double sum = 0.0;
  for( unsigned int i = 0; i < 1e6; ++i )
    {
    sum += i;
    }
  localTimer.Stop();

  // Print current values
  std::cout << "NumberOfStarts: " << localTimer.GetNumberOfStarts() << std::endl;
  std::cout << "NumberOfStops: " << localTimer.GetNumberOfStops() << std::endl;
  std::cout << "InstantValue: " << localTimer.GetInstantValue() << std::endl;
  std::cout << "Total: " << localTimer.GetTotal() << std::endl;
  std::cout << "Mean: " << localTimer.GetMean() << std::endl;

  // invoke reset
  localTimer.Reset();

  if( localTimer.GetNumberOfStarts() != 0 )
    {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
    }
  if( localTimer.GetNumberOfStops() != itk::NumericTraits< itk::TimeProbe::CountType >::ZeroValue() )
    {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
    }
 if( itk::Math::NotExactlyEquals(localTimer.GetTotal(), itk::NumericTraits< itk::TimeProbe::TimeStampType  >::ZeroValue()) )
    {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
    }
  if( itk::Math::NotExactlyEquals(localTimer.GetMean(), itk::NumericTraits< itk::TimeProbe::TimeStampType >::ZeroValue()) )
    {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
    }

  /** Invoke GetRealTimeClock. */
  itk::RealTimeStamp timeStamp = localTimer.GetRealTimeClock()->GetRealTimeStamp();
  std::cout << "day  " << timeStamp.GetTimeInDays() << std::endl;
  std::cout << "hour " << timeStamp.GetTimeInHours() << std::endl;
  std::cout << "min  " << timeStamp.GetTimeInMinutes() << std::endl;
  std::cout << "sec  " << timeStamp.GetTimeInSeconds() << std::endl;
  std::cout << "msec " << timeStamp.GetTimeInMilliSeconds() << std::endl;
  std::cout << "usec " << timeStamp.GetTimeInMicroSeconds() << std::endl;

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
