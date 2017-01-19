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

#include "itkTranslationTransform.h"
#include "itkEuclideanDistancePointMetric.h"
#include "itkLevenbergMarquardtOptimizer.h"
#include "itkPointSetToPointSetRegistrationMethod.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkPointSetToImageFilter.h"
#include "itkTestingMacros.h"

#include <iostream>

/**
 *
 *  This program tests the registration of a PointSet against an other PointSet.
 *
 */

int itkPointSetToPointSetRegistrationTest(int, char* [] )
{
  const unsigned int PointSetDimension = 2;

  typedef float PointSetPointType;

  // Fixed Point Set
  typedef itk::PointSet< PointSetPointType, PointSetDimension > FixedPointSetType;
  FixedPointSetType::Pointer fixedPointSet = FixedPointSetType::New();

  const unsigned int numberOfPoints = 500;

  fixedPointSet->SetPointData( FixedPointSetType::PointDataContainer::New() );

  fixedPointSet->GetPoints()->Reserve( numberOfPoints );
  fixedPointSet->GetPointData()->Reserve( numberOfPoints );

  FixedPointSetType::PointType  point;

  unsigned int id = 0;
  for( unsigned int i = 0; i < numberOfPoints/2; i++ )
    {
    point[0] = 0;
    point[1] = i;
    fixedPointSet->SetPoint( id++, point );
    }
  for( unsigned int i = 0; i < numberOfPoints/2; i++ )
    {
    point[0] = i;
    point[1] = 0;
    fixedPointSet->SetPoint( id++, point );
    }

  // Moving Point Set
  typedef itk::PointSet< PointSetPointType, PointSetDimension > MovingPointSetType;
  MovingPointSetType::Pointer movingPointSet = MovingPointSetType::New();

  movingPointSet->SetPointData( MovingPointSetType::PointDataContainer::New() );

  movingPointSet->GetPoints()->Reserve( numberOfPoints );
  movingPointSet->GetPointData()->Reserve( numberOfPoints );

  id = 0;
  for( unsigned int i = 0; i < numberOfPoints/2; i++ )
    {
    point[0] = 0;
    point[1] = i;
    movingPointSet->SetPoint( id++, point );
    }
  for( unsigned int i = 0; i < numberOfPoints/2; i++ )
    {
    point[0] = i;
    point[1] = 0;
    movingPointSet->SetPoint( id++, point );
    }

  // Set up the Metric
  typedef itk::EuclideanDistancePointMetric< FixedPointSetType,
    MovingPointSetType>  MetricType;

  typedef MetricType::TransformType                 TransformBaseType;
  typedef TransformBaseType::ParametersType         ParametersType;

  MetricType::Pointer  metric = MetricType::New();

  // Set up the Transform
  typedef itk::TranslationTransform< double, PointSetDimension > TransformType;
  TransformType::Pointer transform = TransformType::New();

  // Set up the Optimizer
  typedef itk::LevenbergMarquardtOptimizer OptimizerType;
  OptimizerType::Pointer optimizer = OptimizerType::New();

  optimizer->SetUseCostFunctionGradient( false );

  // Set up the Registration method
  typedef itk::PointSetToPointSetRegistrationMethod< FixedPointSetType,
    MovingPointSetType >  RegistrationType;
  RegistrationType::Pointer registration = RegistrationType::New();

  EXERCISE_BASIC_OBJECT_METHODS( registration, PointSetToPointSetRegistrationMethod,
    ProcessObject );

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales( transform->GetNumberOfParameters() );
  scales.Fill( 1.0 );

  unsigned long   numberOfIterations = 100;
  double          gradientTolerance  = 1e-1; // convergence criterion
  double          valueTolerance     = 1e-1; // convergence criterion
  double          epsilonFunction    = 1e-9; // convergence criterion

  optimizer->SetScales( scales );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetValueTolerance( valueTolerance );
  optimizer->SetGradientTolerance( gradientTolerance );
  optimizer->SetEpsilonFunction( epsilonFunction );

  // Connect all the components required for the registration
  registration->SetMetric( metric );
  TEST_SET_GET_VALUE( metric, registration->GetMetric() );

  registration->SetOptimizer( optimizer );
  TEST_SET_GET_VALUE( optimizer, registration->GetOptimizer() );

  registration->SetTransform( transform );
  TEST_SET_GET_VALUE( transform, registration->GetTransform() );

  registration->SetFixedPointSet( fixedPointSet );
  TEST_SET_GET_VALUE( fixedPointSet, registration->GetFixedPointSet() );

  registration->SetMovingPointSet( movingPointSet );
  TEST_SET_GET_VALUE( movingPointSet, registration->GetMovingPointSet() );

  // Set up transform parameters
  ParametersType parameters( transform->GetNumberOfParameters() );

  // Initialize the offset/vector part
  for( unsigned int k = 0; k < parameters.size(); k++ )
    {
    parameters[k] = 10.0;
    }
  transform->SetParameters( parameters );
  registration->SetInitialTransformParameters( transform->GetParameters() );
  TEST_SET_GET_VALUE( transform->GetParameters(),
    registration->GetInitialTransformParameters() );


  TRY_EXPECT_NO_EXCEPTION( registration->Update() );


  // Print the last transform parameters to improve coverage
  //
  ParametersType finalParameters = registration->GetLastTransformParameters();

  const unsigned int numberOfParameters = parameters.Size();

  std::cout << "Last Transform Parameters: " << std::endl;
  for( unsigned int i = 0; i < numberOfParameters; ++i )
    {
    std::cout << finalParameters[i] << std::endl;
    }

  std::cout << "Solution = " << transform->GetParameters() << std::endl;

  if( ( itk::Math::abs( transform->GetParameters()[0] ) > 1.0 ) ||
    ( itk::Math::abs( transform->GetParameters()[1] ) > 1.0 ) )
    {
    return EXIT_FAILURE;
    }

  //
  // Test with the Danielsson distance map.
  //

  const unsigned int ImageDimension = 2;

  typedef itk::Image< unsigned char, ImageDimension >   BinaryImageType;
  typedef itk::Image< unsigned short, ImageDimension >  ImageType;

  typedef itk::PointSetToImageFilter< FixedPointSetType, BinaryImageType > PSToImageFilterType;
  PSToImageFilterType::Pointer psToImageFilter = PSToImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( psToImageFilter, PointSetToImageFilter,
    ImageSource );

  psToImageFilter->SetInput( fixedPointSet );

  double origin[2] = {0.0, 0.0};
  double spacing[2] = {1.0, 1.0};

  psToImageFilter->SetSpacing( spacing );
  psToImageFilter->SetOrigin( origin );

  std::cout << "Spacing and origin: ["
            << psToImageFilter->GetSpacing() << "], ,["
            << psToImageFilter->GetOrigin() << "]" << std::endl;


  TRY_EXPECT_NO_EXCEPTION( psToImageFilter->Update() );

  BinaryImageType::Pointer binaryImage = psToImageFilter->GetOutput();

  typedef itk::DanielssonDistanceMapImageFilter< BinaryImageType, ImageType >
    DDFilterType;
  DDFilterType::Pointer ddFilter = DDFilterType::New();

  ddFilter->SetInput( binaryImage );

  TRY_EXPECT_NO_EXCEPTION( ddFilter->Update() );

  metric->SetDistanceMap( ddFilter->GetOutput() );
  metric->ComputeSquaredDistanceOn();

  // Initialize the offset/vector part
  for( unsigned int k = 0; k < PointSetDimension; k++ )
    {
    parameters[k] = 10.0;
    }

  transform->SetParameters(parameters);
  registration->SetInitialTransformParameters( transform->GetParameters() );

  TRY_EXPECT_NO_EXCEPTION( registration->Update() );

  std::cout << "Solution = " << transform->GetParameters() << std::endl;

  if( ( itk::Math::abs( transform->GetParameters()[0] ) > 1.0 ) ||
    ( itk::Math::abs( transform->GetParameters()[1] ) > 1.0 ) )
    {
    return EXIT_FAILURE;
    }

  std::cout << "TEST DONE" << std::endl;

  return EXIT_SUCCESS;
}
