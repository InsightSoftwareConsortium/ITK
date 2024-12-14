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
  std::cout << "Testing itk::TimeProbe" << '\n';
  std::cout << "NameOfProbe:       " << localTimer.GetNameOfProbe() << '\n';
  std::cout << "Type:              " << localTimer.GetType() << '\n';
  std::cout << "Unit:              " << localTimer.GetUnit() << '\n';

  std::cout << "NumberOfStarts:    " << localTimer.GetNumberOfStarts() << '\n';
  std::cout << "NumberOfStops:     " << localTimer.GetNumberOfStops() << '\n';
  std::cout << "Total:             " << localTimer.GetTotal() << '\n';
  std::cout << "InstantValue:      " << localTimer.GetInstantValue() << '\n';
  std::cout << "Minimum:           " << localTimer.GetMinimum() << '\n';
  std::cout << "Mean:              " << localTimer.GetMean() << '\n';
  std::cout << "Maximum:           " << localTimer.GetMaximum() << '\n';
  std::cout << "StandardDeviation: " << localTimer.GetStandardDeviation() << '\n';

  const unsigned int iteration(100);

  for (unsigned int it = 0; it < iteration; ++it)
  {
    // time a task
    localTimer.Start();

    double sum = 0.0;
    for (unsigned int i = 0; i < 1e6; ++i)
    {
      sum += i;
    }
    std::cout << "Dummy sum: " << sum << '\n';

    localTimer.Stop();
  }

  if (!CheckTimeProbe(localTimer))
  {
    std::cerr << "Validation of Probe failure" << '\n';
    return EXIT_FAILURE;
  }

  // Print current values
  std::cout << "InstantValue:      " << localTimer.GetInstantValue() << '\n';

  // Print a regular report (including nameOfProbe, Iteration, Total, Min, Mean, Max, and STD)
  std::cout << '\n' << "Print a normal report" << '\n';
  localTimer.Report();

  // Print an expanded report (including nameOfProbe, Iteration, Total, Min, Mean-Min
  //                          Mean/Min *100 (%), Mean, Max, Max- Mean, Max/Mean(%),
  //                          Total Diff(:Max - Min) and STD)
  std::cout << '\n' << "Print an expanded report" << '\n';
  localTimer.ExpandedReport();

  std::cout << '\n' << "Print a JSON report" << '\n';
  localTimer.JSONReport();

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
  std::cout << '\n' << "Check RealTimeStamp" << '\n';
  std::cout << "TimeInDays:         " << timeStamp.GetTimeInDays() << '\n';
  std::cout << "TimeInHours:        " << timeStamp.GetTimeInHours() << '\n';
  std::cout << "TimeInMinutes:      " << timeStamp.GetTimeInMinutes() << '\n';
  std::cout << "TimeInSeconds:      " << timeStamp.GetTimeInSeconds() << '\n';
  std::cout << "TimeInMilliSeconds: " << timeStamp.GetTimeInMilliSeconds() << '\n';
  std::cout << "TimeInMicroSeconds: " << timeStamp.GetTimeInMicroSeconds() << '\n';


  std::cout << "Test finished." << '\n';
  return EXIT_SUCCESS;
}
