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
#include "itkIndent.h"
#include "itkTimeProbe.h"
#include "itkMath.h"


int
itkTimeProbeTest(int, char *[])
{
  // Create an ITK time probe
  itk::TimeProbe localTimer;

  // Print the initial values
  std::cout << "Testing itk::TimeProbe" << '\n';
  std::cout << "Type: " << localTimer.GetType() << '\n';
  std::cout << "Unit: " << localTimer.GetUnit() << '\n';

  std::cout << "NumberOfStarts: " << localTimer.GetNumberOfStarts() << '\n';
  std::cout << "NumberOfStops: " << localTimer.GetNumberOfStops() << '\n';
  std::cout << "InstantValue: " << localTimer.GetInstantValue() << '\n';
  std::cout << "Total: " << localTimer.GetTotal() << '\n';
  std::cout << "Mean: " << localTimer.GetMean() << '\n';

  // time a task
  localTimer.Start();
  // usleep( 1000000 );//not portable
  double sum = 0.0;
  for (unsigned int i = 0; i < 1e6; ++i)
  {
    sum += i;
  }
  std::cout << "Dummy sum: " << sum << '\n';
  localTimer.Stop();

  // Print current values
  std::cout << "NumberOfStarts: " << localTimer.GetNumberOfStarts() << '\n';
  std::cout << "NumberOfStops: " << localTimer.GetNumberOfStops() << '\n';
  std::cout << "InstantValue: " << localTimer.GetInstantValue() << '\n';
  std::cout << "Total: " << localTimer.GetTotal() << '\n';
  std::cout << "Mean: " << localTimer.GetMean() << '\n';

  // invoke reset
  localTimer.Reset();

  if (localTimer.GetNumberOfStarts() != 0)
  {
    std::cerr << "Reset() failure" << '\n';
    return EXIT_FAILURE;
  }
  if (localTimer.GetNumberOfStops() != itk::TimeProbe::CountType{})
  {
    std::cerr << "Reset() failure" << '\n';
    return EXIT_FAILURE;
  }
  if (itk::Math::NotExactlyEquals(localTimer.GetTotal(), itk::TimeProbe::TimeStampType{}))
  {
    std::cerr << "Reset() failure" << '\n';
    return EXIT_FAILURE;
  }
  if (itk::Math::NotExactlyEquals(localTimer.GetMean(), itk::TimeProbe::TimeStampType{}))
  {
    std::cerr << "Reset() failure" << '\n';
    return EXIT_FAILURE;
  }

  /** Invoke GetRealTimeClock. */
  const itk::RealTimeStamp timeStamp = localTimer.GetRealTimeClock()->GetRealTimeStamp();
  std::cout << "TimeInDays:         " << timeStamp.GetTimeInDays() << '\n';
  std::cout << "TimeInHours:        " << timeStamp.GetTimeInHours() << '\n';
  std::cout << "TimeInMinutes:      " << timeStamp.GetTimeInMinutes() << '\n';
  std::cout << "TimeInSeconds:      " << timeStamp.GetTimeInSeconds() << '\n';
  std::cout << "TimeInMilliSeconds: " << timeStamp.GetTimeInMilliSeconds() << '\n';
  std::cout << "TimeInMicroSeconds: " << timeStamp.GetTimeInMicroSeconds() << '\n';

  // Exercise the Print method
  const itk::Indent indent{};
  localTimer.Print(std::cout, indent);


  std::cout << "Test finished." << '\n';
  return EXIT_SUCCESS;
}
