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

#include "itkManhattanDistanceMetric.h"

int itkManhattanDistanceMetricTest(int, char* [] )
{
  const unsigned int MeasurementVectorSize = 3;

  typedef itk::Array< float  >  MeasurementVectorType;

  typedef itk::Statistics::ManhattanDistanceMetric< MeasurementVectorType >   DistanceMetricType;

  DistanceMetricType::Pointer distance = DistanceMetricType::New();

  std::cout << distance->GetNameOfClass() << std::endl;

  distance->Print(std::cout);

  MeasurementVectorType measurementNew;
  ::itk::NumericTraits<MeasurementVectorType>::SetLength( measurementNew, 3);
  measurementNew[0] = 2.5;
  measurementNew[1] = 3.3;
  measurementNew[2] = 4.0;

  //Attempting to compute distance before setting a measurment vector should
  //throw an exception

  try
    {
    distance->Evaluate( measurementNew );
    std::cerr << "Attempting to compute distance w/o setting measurement vector"
                 "size, Exception should have been thrown" << std::endl;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception: " << excp << std::endl;
    }


  distance->SetMeasurementVectorSize( MeasurementVectorSize );

  if( distance->GetMeasurementVectorSize() != MeasurementVectorSize )
    {
    std::cerr << "GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  //Test if the distance computed is correct
  DistanceMetricType::OriginType origin;
  ::itk::NumericTraits<DistanceMetricType::OriginType>::SetLength( origin, 3);
  origin[0] = 1.5;
  origin[1] = 2.3;
  origin[2] = 1.0;
  distance->SetOrigin( origin );

  MeasurementVectorType measurement;
  ::itk::NumericTraits<MeasurementVectorType>::SetLength( measurement, 3);
  measurement[0] = 2.5;
  measurement[1] = 3.3;
  measurement[2] = 4.0;

  double trueValue = 5.0;
  double distanceComputed = distance->Evaluate( measurement );
  const double tolerance = 0.001;

  if( std::fabs( distanceComputed - trueValue) > tolerance )
    {
    std::cerr << "Distance computed not correct: " << "truevalue= " << trueValue
              << "ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
    }

  //Compute distance between two measurement vectors
  MeasurementVectorType measurement2;
  ::itk::NumericTraits<MeasurementVectorType>::SetLength( measurement2, 3);
  measurement2[0] = 1.5;
  measurement2[1] = 3.5;
  measurement2[2] = 3.5;

  double trueValue2 = 1.7;
  double distanceComputed2 = distance->Evaluate( measurement, measurement2 );

  if( std::fabs( distanceComputed2 - trueValue2) > tolerance )
    {
    std::cerr << "Distance computed not correct: " << "truevalue= " << trueValue2
              << "ComputedValue=" << distanceComputed2 << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
