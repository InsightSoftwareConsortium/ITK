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

#include "itkListSample.h"
#include "itkStatisticsAlgorithm.h"
#include "itkTestingMacros.h"

// Problem has been solved in _MSC_FULL_VER == 191125508
#if ( NDEBUG && (_MSC_FULL_VER == 190024215 || _MSC_FULL_VER == 191025017 ) )
#pragma optimize ("g", off) // disable global optimizations
#endif

int itkStatisticsAlgorithmTest( int, char * [] )
{
  std::cout << "StatisticsAlgorithm Test \n \n";

  const unsigned int measurementVectorSize = 2;

  typedef itk::Array< float > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  SampleType::Pointer sample = SampleType::New();

  SampleType::MeasurementVectorType lower( measurementVectorSize );
  SampleType::MeasurementVectorType upper( measurementVectorSize );

  const SampleType * constSample = sample.GetPointer();

  // Testing the exception throwing for samples of measurement size = 0
  sample->SetMeasurementVectorSize( 0 );

  TRY_EXPECT_EXCEPTION(
      itk::Statistics::Algorithm::FindSampleBound(
      constSample,
      constSample->Begin(), constSample->End(),
      lower, upper)
  );

  // Now set the correct measurement vector size
  sample->SetMeasurementVectorSize( measurementVectorSize );

  // Testing the equivalent of an empty sample by passing
  // the Begin() iterator inlieu of the End() iterator.

  TRY_EXPECT_EXCEPTION(
      itk::Statistics::Algorithm::FindSampleBound(
          constSample,
          constSample->Begin(), constSample->Begin(),
          lower, upper)
  );

  MeasurementVectorType measure( measurementVectorSize );
  MeasurementVectorType realUpper( measurementVectorSize );
  MeasurementVectorType realLower( measurementVectorSize );

  const unsigned int numberOfSamples = 25;

  realLower.Fill( 1000 );
  realUpper.Fill(    0 );

  // Force the first value not to be the min or max.
  measure[0] = 13;
  measure[1] = 39;
  sample->PushBack( measure );

  for( unsigned int i = 1; i < numberOfSamples; i++ )
    {
    float value = i + 3;
    measure[0] = value;
    measure[1] = value * value;
    sample->PushBack( measure );

    for(unsigned int j = 0; j < measurementVectorSize; j++ )
      {
      if( measure[j] < realLower[j] )
        {
        realLower[j] = measure[j];
        }
      if( measure[j] > realUpper[j] )
        {
        realUpper[j] = measure[j];
        }
      }
    }

  // Now testing the real algorithm
  TRY_EXPECT_NO_EXCEPTION(
      itk::Statistics::Algorithm::FindSampleBound(
          constSample,
          constSample->Begin(), constSample->End(),
          lower, upper)
  );

  const float epsilon = 1e-5;

  for(unsigned int j = 0; j < measurementVectorSize; j++ )
    {
    if( itk::Math::abs( lower[j] - realLower[j] ) > epsilon )
      {
      std::cerr << "FindSampleBound() failed" << std::endl;
      std::cerr << "Expected lower = " << realLower << std::endl;
      std::cerr << "Computed lower = " << lower << std::endl;
      return EXIT_FAILURE;
      }
    if( itk::Math::abs( upper[j] - realUpper[j] ) > epsilon )
      {
      std::cerr << "FindSampleBound() failed" << std::endl;
      std::cerr << "Expected upper = " << realUpper << std::endl;
      std::cerr << "Computed upper = " << upper << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
