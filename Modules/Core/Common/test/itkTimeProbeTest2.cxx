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
#include "itkTimeProbe.h"
#include "itkMath.h"

// Check the validation of resource probe's result
bool
CheckTimeProbe(itk::TimeProbe & probe)
{
  bool check = true;
  // Check the numbers of iteration, starts, and stops
  check &= (probe.GetNumberOfIteration() == probe.GetNumberOfStarts());
  check &= (probe.GetNumberOfIteration() == probe.GetNumberOfStops());

  check &= (probe.GetStandardDeviation() >= 0);
  check &= (probe.GetMinimum() >= 0);
  check &= (probe.GetMean() >= probe.GetMinimum());
  check &= (probe.GetMaximum() >= probe.GetMean());
  check &= (probe.GetTotal() >= probe.GetMaximum());

  return check;
}

int
itkTimeProbeTest2(int, char *[])
{
  // Create an ITK time probe
  itk::TimeProbe localTimer;

  // Set a name of probe
  localTimer.SetNameOfProbe("Simple for-loop");

  // Print the initial values
  std::cout << "Testing itk::TimeProbe" << std::endl;
  std::cout << "NameOfProbe:       " << localTimer.GetNameOfProbe() << std::endl;
  std::cout << "Type:              " << localTimer.GetType() << std::endl;
  std::cout << "Unit:              " << localTimer.GetUnit() << std::endl;

  std::cout << "NumberOfStarts:    " << localTimer.GetNumberOfStarts() << std::endl;
  std::cout << "NumberOfStops:     " << localTimer.GetNumberOfStops() << std::endl;
  std::cout << "Total:             " << localTimer.GetTotal() << std::endl;
  std::cout << "InstantValue:      " << localTimer.GetInstantValue() << std::endl;
  std::cout << "Minimum:           " << localTimer.GetMinimum() << std::endl;
  std::cout << "Mean:              " << localTimer.GetMean() << std::endl;
  std::cout << "Maximum:           " << localTimer.GetMaximum() << std::endl;
  std::cout << "StandardDeviation: " << localTimer.GetStandardDeviation() << std::endl;

  constexpr unsigned int iteration(100);

  for (unsigned int it = 0; it < iteration; ++it)
  {
    // time a task
    localTimer.Start();

    double sum = 0.0;
    for (unsigned int i = 0; i < 1e6; ++i)
    {
      sum += i;
    }
    std::cout << "Dummy sum: " << sum << std::endl;

    localTimer.Stop();
  }

  if (!CheckTimeProbe(localTimer))
  {
    std::cerr << "Validation of Probe failure" << std::endl;
    return EXIT_FAILURE;
  }

  // Print current values
  std::cout << "InstantValue:      " << localTimer.GetInstantValue() << std::endl;

  // Print a regular report (including nameOfProbe, Iteration, Total, Min, Mean, Max, and STD)
  std::cout << std::endl << "Print a normal report" << std::endl;
  localTimer.Report();

  // Print an expanded report (including nameOfProbe, Iteration, Total, Min, Mean-Min
  //                          Mean/Min *100 (%), Mean, Max, Max- Mean, Max/Mean(%),
  //                          Total Diff(:Max - Min) and STD)
  std::cout << std::endl << "Print an expanded report" << std::endl;
  localTimer.ExpandedReport();

  std::cout << std::endl << "Print a JSON report" << std::endl;
  localTimer.JSONReport();

  // invoke reset
  localTimer.Reset();

  if (localTimer.GetNumberOfStarts() != 0)
  {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
  }
  if (localTimer.GetNumberOfStops() != itk::TimeProbe::CountType{})
  {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
  }
  if (itk::Math::NotExactlyEquals(localTimer.GetTotal(), itk::TimeProbe::TimeStampType{}))
  {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
  }
  if (itk::Math::NotExactlyEquals(localTimer.GetMean(), itk::TimeProbe::TimeStampType{}))
  {
    std::cerr << "Reset() failure" << std::endl;
    return EXIT_FAILURE;
  }

  /** Invoke GetRealTimeClock. */
  const itk::RealTimeStamp timeStamp = localTimer.GetRealTimeClock()->GetRealTimeStamp();
  std::cout << std::endl << "Check RealTimeStamp" << std::endl;
  std::cout << "TimeInDays:         " << timeStamp.GetTimeInDays() << std::endl;
  std::cout << "TimeInHours:        " << timeStamp.GetTimeInHours() << std::endl;
  std::cout << "TimeInMinutes:      " << timeStamp.GetTimeInMinutes() << std::endl;
  std::cout << "TimeInSeconds:      " << timeStamp.GetTimeInSeconds() << std::endl;
  std::cout << "TimeInMilliSeconds: " << timeStamp.GetTimeInMilliSeconds() << std::endl;
  std::cout << "TimeInMicroSeconds: " << timeStamp.GetTimeInMicroSeconds() << std::endl;


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
