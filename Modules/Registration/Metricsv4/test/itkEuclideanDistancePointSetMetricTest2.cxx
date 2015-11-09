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
#include "itkTranslationTransform.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkTestingMacros.h"

#include <fstream>
#include "itkMath.h"

/*
 * Test with a displacement field transform
 */

template<unsigned int Dimension>
int itkEuclideanDistancePointSetMetricTest2Run()
{
  typedef itk::PointSet<unsigned char, Dimension> PointSetType;

  typedef typename PointSetType::PointType PointType;

  typename PointSetType::Pointer fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  typename PointSetType::Pointer movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // Create a few points and apply a small offset to make the moving points
  float pointMax = static_cast<float>(100.0);
  PointType fixedPoint;
  fixedPoint.Fill( 0.0 );
  fixedPoint[0] = 0.0;
  fixedPoint[1] = 0.0;
  fixedPoints->SetPoint( 0, fixedPoint );
  fixedPoint[0] = pointMax;
  fixedPoint[1] = 0.0;
  fixedPoints->SetPoint( 1, fixedPoint );
  fixedPoint[0] = pointMax;
  fixedPoint[1] = pointMax;
  fixedPoints->SetPoint( 2, fixedPoint );
  fixedPoint[0] = 0.0;
  fixedPoint[1] = pointMax;
  fixedPoints->SetPoint( 3, fixedPoint );
  fixedPoint[0] = pointMax / 2.0;
  fixedPoint[1] = pointMax / 2.0;
  fixedPoints->SetPoint( 4, fixedPoint );
  if( Dimension == 3 )
    {
    fixedPoint[0] = pointMax / 2.0;
    fixedPoint[1] = pointMax / 2.0;
    fixedPoint[2] = pointMax / 2.0;
    fixedPoints->SetPoint( 5, fixedPoint );
    fixedPoint[0] = 0.0;
    fixedPoint[1] = 0.0;
    fixedPoint[2] = pointMax / 2.0;
    fixedPoints->SetPoint( 6, fixedPoint );
    }
  unsigned int numberOfPoints = fixedPoints->GetNumberOfPoints();

  PointType movingPoint;
  for( unsigned int n=0; n < numberOfPoints; n ++ )
    {
    fixedPoint = fixedPoints->GetPoint( n );
    movingPoint[0] = fixedPoint[0] + (n + 1) * 0.5;
    movingPoint[1] = fixedPoint[1] - (n + 2) * 0.5;
    if( Dimension == 3 )
      {
      movingPoint[2] = fixedPoint[2] + (n + 3 ) * 0.5;
      }
    movingPoints->SetPoint( n, movingPoint );
    //std::cout << fixedPoint << " -> " << movingPoint << std::endl;
    }

  // Test with displacement field transform
  std::cout << "Testing with displacement field transform." << std::endl;
  //typedef itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<double, Dimension> DisplacementFieldTransformType;
  typedef itk::DisplacementFieldTransform<double, Dimension> DisplacementFieldTransformType;
  typename DisplacementFieldTransformType::Pointer displacementTransform = DisplacementFieldTransformType::New();

  // Setup the physical space to match the point set virtual domain,
  // which is defined by the fixed point set since the fixed transform
  // is identity.
  typedef typename DisplacementFieldTransformType::DisplacementFieldType FieldType;
  typedef typename FieldType::RegionType RegionType;

  typename FieldType::SpacingType spacing;
  spacing.Fill( 1.0 );

  typename FieldType::DirectionType direction;
  direction.Fill( 0.0 );
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    direction[d][d] = 1.0;
    }

  typename FieldType::PointType origin;
  origin.Fill( 0.0 );

  typename RegionType::SizeType regionSize;
  regionSize.Fill( static_cast<itk::SizeValueType>(pointMax + 1.0) );

  typename RegionType::IndexType regionIndex;
  regionIndex.Fill( 0 );

  RegionType region;
  region.SetSize( regionSize );
  region.SetIndex( regionIndex );

  typename FieldType::Pointer displacementField = FieldType::New();
  displacementField->SetOrigin( origin );
  displacementField->SetDirection( direction );
  displacementField->SetSpacing( spacing );
  displacementField->SetRegions( region );
  displacementField->Allocate();
  typename DisplacementFieldTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 0 );
  displacementField->FillBuffer( zeroVector );
  displacementTransform->SetDisplacementField( displacementField );

  // Instantiate the metric
  typedef itk::EuclideanDistancePointSetToPointSetMetricv4<PointSetType> PointSetMetricType;
  typename PointSetMetricType::Pointer metric = PointSetMetricType::New();
  metric->SetFixedPointSet( fixedPoints );
  metric->SetMovingPointSet( movingPoints );
  metric->SetMovingTransform( displacementTransform );
  // If we don't set this explicitly, it will still work because it will be taken from the
  // displacement field during initialization.
  metric->SetVirtualDomain( spacing, origin, direction, region );

  metric->Initialize();

  // test
  typename PointSetMetricType::MeasureType value = metric->GetValue(), value2;
  typename PointSetMetricType::DerivativeType derivative, derivative2;
  metric->GetDerivative( derivative );
  metric->GetValueAndDerivative( value2, derivative2 );

  std::cout << "value: " << value << std::endl;

  // Check for the same results from different methods
  if( itk::Math::NotExactlyEquals(value, value2) )
    {
    std::cerr << "value does not match between calls to different methods: "
              << "value: " << value << " value2: " << value2 << std::endl;
    }
  if( derivative != derivative2 )
    {
    std::cerr << "derivative does not match between calls to different methods: "
              << "derivative: " << derivative << " derivative2: " << derivative2 << std::endl;
    }

  displacementTransform->UpdateTransformParameters( derivative2 );

  // check the results
  bool passed = true;
  for( itk::SizeValueType n=0; n < fixedPoints->GetNumberOfPoints(); n++ )
    {
    PointType transformedPoint;
    fixedPoint = fixedPoints->GetPoint( n );
    movingPoint = movingPoints->GetPoint(n);
    transformedPoint = displacementTransform->TransformPoint( fixedPoint );
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      if( itk::Math::NotExactlyEquals(transformedPoint[d], movingPoint[d]) )
        {
        passed = false;
        }
      }
    std::cout << " fixed, moving, txf'ed, moving-txf: " << fixedPoint << " " << movingPoint << " " << transformedPoint << " " << movingPoint - transformedPoint << std::endl;
    }

  if( ! passed )
    {
    std::cerr << "Not all points match after transformation with result." << std::endl;
    return EXIT_FAILURE;
    }

  // Test the valid points are counted correctly.
  // We should get a warning printed.
  fixedPoint[0] = 0.0;
  fixedPoint[1] = 2 * pointMax;
  unsigned int numberExpected = fixedPoints->GetNumberOfPoints() - 1;
  fixedPoints->SetPoint( fixedPoints->GetNumberOfPoints() - 1, fixedPoint );
  metric->GetValueAndDerivative( value2, derivative2 );
  if( metric->GetNumberOfValidPoints() != numberExpected )
    {
    std::cerr << "Expected " << numberExpected << " valid points, but got " << metric->GetNumberOfValidPoints() << std::endl;
    return EXIT_FAILURE;
    }

  // Test with no valid points.
  typename PointSetType::Pointer fixedPoints2 = PointSetType::New();
  fixedPoints2->Initialize();
  fixedPoint[0] = -pointMax;
  fixedPoint[1] = 0.0;
  fixedPoints2->SetPoint( 0, fixedPoint );
  metric->SetFixedPointSet( fixedPoints2 );
  metric->Initialize();
  metric->GetValueAndDerivative( value2, derivative2 );
  bool derivative2IsZero = true;
  for( itk::SizeValueType n=0; n < metric->GetNumberOfParameters(); n++ )
    {
    if( itk::Math::NotExactlyEquals(derivative2[n], itk::NumericTraits<typename PointSetMetricType::DerivativeValueType>::ZeroValue()) )
      {
      derivative2IsZero = false;
      }
    }
  if( itk::Math::NotExactlyEquals(value2, itk::NumericTraits<typename PointSetMetricType::MeasureType>::max()) || ! derivative2IsZero )
    {
    std::cerr << "Failed testing with no valid points. Number of valid points: " << metric->GetNumberOfValidPoints()
              << " value2: " << value2 << " derivative2IsZero: " << derivative2IsZero << std::endl;
    return EXIT_FAILURE;
    }

  // Test with invalid virtual domain, i.e.
  // one that doesn't match the displacement field.
  typename RegionType::SizeType badSize;
  badSize.Fill( static_cast<itk::SizeValueType>(pointMax / 2.0) );
  RegionType badRegion;
  badRegion.SetSize( badSize );
  badRegion.SetIndex( regionIndex );
  metric->SetVirtualDomain( spacing, origin, direction, badRegion );
  TRY_EXPECT_EXCEPTION( metric->Initialize() );

  typename FieldType::SpacingType badSpacing;
  badSpacing.Fill( 0.5 );
  metric->SetVirtualDomain( badSpacing, origin, direction, region );
  TRY_EXPECT_EXCEPTION( metric->Initialize() );

  return EXIT_SUCCESS;
}

int itkEuclideanDistancePointSetMetricTest2( int, char* [] )
{
  int result = EXIT_SUCCESS;

  if( itkEuclideanDistancePointSetMetricTest2Run<2>() == EXIT_FAILURE )
    {
    std::cerr << "Failed for Dimension 2." << std::endl;
    result = EXIT_FAILURE;
    }

  if( itkEuclideanDistancePointSetMetricTest2Run<3>() == EXIT_FAILURE )
    {
    std::cerr << "Failed for Dimension 3." << std::endl;
    result = EXIT_FAILURE;
    }

  return result;
}
