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
#include "itkDistanceToCentroidMembershipFunction.h"

int itkDistanceToCentroidMembershipFunctionTest(int, char* [] )
{

  const unsigned int MeasurementVectorSize = 3;

  typedef itk::FixedArray<
    float, MeasurementVectorSize >  MeasurementVectorType;

  typedef itk::Statistics::DistanceToCentroidMembershipFunction<
    MeasurementVectorType >   MembershipFunctionType;

  MembershipFunctionType::Pointer function = MembershipFunctionType::New();

  std::cout << function->GetNameOfClass() << std::endl;


  //set the distance metric type
  typedef itk::Statistics::EuclideanDistanceMetric< MeasurementVectorType >  DistanceMetricType;
  typedef DistanceMetricType::MeasurementVectorSizeType MeasurementVectorSizeType;

  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New();
  function->SetDistanceMetric( distanceMetric );

  if( function->GetDistanceMetric() != distanceMetric )
    {
    std::cerr << "Error in GetDistanceMetric() " << std::endl;
    return EXIT_FAILURE;
    }

  function->Print(std::cout);

  function->SetMeasurementVectorSize( MeasurementVectorSize ); // for code coverage

  if( function->GetMeasurementVectorSize() != MeasurementVectorSize )
    {
    std::cerr << "GetMeasurementVectorSize() Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  //Test if an exception will be thrown if we try to resize the measurement vector
  //size
  try
    {
    MeasurementVectorSizeType measurementVector2 = MeasurementVectorSize + 1;
    function->SetMeasurementVectorSize( measurementVector2 );
    std::cerr << "Exception should have been thrown since we are trying to resize\
                  non-resizeable measurement vector type " << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Caughted expected exception: " << excp << std::endl;
    }


  //Test if the distance computed is correct
  MembershipFunctionType::CentroidType origin;
  ::itk::NumericTraits<MembershipFunctionType::CentroidType>::SetLength( origin, 3);
  origin[0] = 1.5;
  origin[1] = 2.3;
  origin[2] = 1.0;
  function->SetCentroid( origin );

  const double tolerance = 0.001;

  if( std::fabs( function->GetCentroid()[0] - origin[0]) > tolerance ||
      std::fabs( function->GetCentroid()[1] - origin[1]) > tolerance ||
      std::fabs( function->GetCentroid()[2] - origin[2]) > tolerance )
    {
    std::cerr << "Error in GetCentroid() method" << std::endl;
    return EXIT_FAILURE;
    }

  MeasurementVectorType measurement;
  ::itk::NumericTraits<MeasurementVectorType>::SetLength( measurement, 3);
  measurement[0] = 2.5;
  measurement[1] = 3.3;
  measurement[2] = 4.0;

  double trueValue = 3.31662;
  double distanceComputed = function->Evaluate( measurement );

  if( std::fabs( distanceComputed - trueValue) > tolerance )
    {
    std::cerr << "Distance computed not correct: " << "truevalue= " << trueValue
              << "ComputedValue=" << distanceComputed << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise the Clone method.
  MembershipFunctionType::Pointer clonedFunction = function->Clone();
  if( clonedFunction.IsNull() )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
