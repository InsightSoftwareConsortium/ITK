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

#include "itkEuclideanDistancePointSetToPointSetMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromShift.h"
#include "itkAffineTransform.h"

#include <fstream>

int itkEuclideanDistancePointSetMetricRegistrationTest( int argc, char *argv[] )
{
  const unsigned int Dimension = 2;

  unsigned int numberOfIterations = 50;
  if( argc > 1 )
    {
    numberOfIterations = atoi( argv[1] );
    }

  typedef itk::PointSet<unsigned char, Dimension> PointSetType;

  typedef PointSetType::PointType PointType;

  PointSetType::Pointer fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  PointSetType::Pointer movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // Create a few points and apply a small rotation to make the moving point set
  float size = 100.0;
  float theta = vnl_math::pi / 180.0 * 10.0;
  unsigned int numberOfPoints = 4;
  PointType fixedPoint;
  fixedPoint[0] = 0;
  fixedPoint[1] = 0;
  fixedPoints->SetPoint( 0, fixedPoint );
  fixedPoint[0] = size;
  fixedPoint[1] = 0;
  fixedPoints->SetPoint( 1, fixedPoint );
  fixedPoint[0] = size;
  fixedPoint[1] = size;
  fixedPoints->SetPoint( 2, fixedPoint );
  fixedPoint[0] = 0;
  fixedPoint[1] = size;
  fixedPoints->SetPoint( 3, fixedPoint );
  PointType movingPoint;
  for( unsigned int n=0; n < numberOfPoints; n ++ )
    {
    fixedPoint = fixedPoints->GetPoint( n );
    movingPoint[0] = fixedPoint[0] * vcl_cos( theta ) - fixedPoint[1] * vcl_sin( theta );
    movingPoint[1] = fixedPoint[0] * vcl_sin( theta ) + fixedPoint[1] * vcl_cos( theta );
    movingPoints->SetPoint( n, movingPoint );
    std::cout << fixedPoint << " -> " << movingPoint << std::endl;
    }

  // Affine transform for moving point set
  typedef itk::AffineTransform<double, Dimension> AffineTransformType;
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();

  // Instantiate the metric
  typedef itk::EuclideanDistancePointSetToPointSetMetricv4<PointSetType> PointSetMetricType;
  PointSetMetricType::Pointer metric = PointSetMetricType::New();
  metric->SetFixedPointSet( fixedPoints );
  metric->SetMovingPointSet( movingPoints );
  metric->SetMovingTransform( affineTransform );
  metric->Initialize();

  // scales estimator
  // needs updating to handle point sets
  //typedef itk::RegistrationParameterScalesFromShift< PointSetMetricType > RegistrationParameterScalesFromShiftType;
  //RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  //shiftScaleEstimator->SetMetric( metric );

  // optimizer
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  OptimizerType::Pointer  optimizer = OptimizerType::New();
  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );
  //optimizer->SetScalesEstimator( shiftScaleEstimator );

  OptimizerType::ScalesType scales( metric->GetNumberOfParameters() );
  scales.Fill(1.0);
  scales[4] = 100;
  scales[5] = 100;

  optimizer->SetScales( scales );
  optimizer->SetLearningRate( 0.0001 );

  //if( maximumStepSize > 0 )
  //  {
  //  optimizer->SetMaximumStepSizeInPhysicalUnits( maximumStepSize );
  //  }

  optimizer->StartOptimization();

  std::cout << "numberOfIterations: " << numberOfIterations << std::endl;
  std::cout << "Moving-source final value: " << optimizer->GetValue() << std::endl;
  std::cout << "Moving-source final position: " << optimizer->GetCurrentPosition() << std::endl;

  // applying the resultant transform to moving points and verify result
  std::cout << "Fixed\tMoving\tTransformed Moving\tTransformed Fixed\tDiff" << std::endl;
  bool passed = true;
  PointType::ValueType tolerance = 1e-4;
  AffineTransformType::InverseTransformBasePointer movingInverse = metric->GetMovingTransform()->GetInverseTransform();
  AffineTransformType::InverseTransformBasePointer fixedInverse = metric->GetFixedTransform()->GetInverseTransform();
  for( unsigned int n=0; n < numberOfPoints; n++ )
    {
    // compare the points in virtual domain
    PointType transformedMovingPoint = movingInverse->TransformPoint( movingPoints->GetPoint( n ) );
    PointType transformedFixedPoint = fixedInverse->TransformPoint( fixedPoints->GetPoint( n ) );
    PointType difference;
    difference[0] = transformedMovingPoint[0] - transformedFixedPoint[0];
    difference[1] = transformedMovingPoint[1] - transformedFixedPoint[1];
    std::cout << fixedPoints->GetPoint( n ) << "\t" << movingPoints->GetPoint( n )
              << "\t" << transformedMovingPoint << "\t" << transformedFixedPoint << "\t" << difference << std::endl;
    if( fabs( difference[0] ) > tolerance || fabs( difference[1] ) > tolerance )
      {
      passed = false;
      }
    }
  if( ! passed )
    {
    std::cerr << "Results do not match truth within tolerance." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
