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
#include "itkMath.h"

#include "itkMath.h"
#include "itkCompositeValleyFunction.h"
#include "itkStdStreamStateSave.h"

int itkCompositeValleyFunctionTest(int , char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  itk::Array< double > means(2);
  itk::Array< double > sigmas(2);

  means[0] = 0.0;
  means[1] = 100.0;
  sigmas[0] = 20.0;
  sigmas[1] = 20.0;

  itk::CompositeValleyFunction function(means, sigmas);

  if ( function.GetUpperBound() != 280.0 )
    {
    std::cout << "Test fails: GetUpperBound()" << std::endl;
    return EXIT_FAILURE;
    }

  if ( itk::Math::NotAlmostEquals( function.GetLowerBound(), -180.0 ) )
    {
    std::cout << "Test fails: GetLowerBound()" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout.setf(std::ios::scientific);
  std::cout.precision(12);

  double interval1 = function.GetInterval();
  double interval2 =
    ( function.GetUpperBound() - function.GetLowerBound() )
    / (1000000.0 - 1.0);
  if (  itk::Math::abs( interval1 - interval2 ) >
        itk::NumericTraits< double >::epsilon() )
    {
    std::cout << "Test fails: GetInterval()" << std::endl;
    std::cout << "Interval from the GetInterval() = " << interval1
              << std::endl;
    std::cout << "Interval value using the calculation = " << interval2
              << std::endl;
    return EXIT_FAILURE;
    }

  long numberOfSamples = function.GetNumberOfSamples();
  double measure = function.GetLowerBound() + interval1* numberOfSamples * 0.5;
  double value1 = function( measure );
  double value2 = function.Evaluate( measure );

  if ( itk::Math::abs(value1 - value2) >
       itk::NumericTraits< double >::epsilon())
    {
    std::cout << "diff = " << itk::Math::abs(value1 - value2) << std::endl;
    std::cout << "Test fails: operator()" << std::endl;

    return EXIT_FAILURE;
    }

  std::cout << "Test succeed" << std::endl;
  return EXIT_SUCCESS;
}
