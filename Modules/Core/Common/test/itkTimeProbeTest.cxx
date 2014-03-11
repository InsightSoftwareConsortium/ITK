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
  if( localTimer.GetNumberOfStops() != 0 )
    {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
    }
  if( localTimer.GetTotal() != 0 )
    {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
    }
  if( localTimer.GetMean() != 0 )
    {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
