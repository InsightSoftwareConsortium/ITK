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

#include "itkLabeledPointSetToPointSetMetricv4.h"
#include "itkTranslationTransform.h"

#include <fstream>
#include "itkMath.h"

template<unsigned int Dimension>
int itkLabeledPointSetMetricTestRun()
{
  typedef unsigned int LabelType;

  typedef itk::PointSet<LabelType, Dimension> PointSetType;

  typedef typename PointSetType::PointType PointType;

  typename PointSetType::Pointer fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  typename PointSetType::Pointer movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // Produce two simple point sets of 1) a circle and 2) the same circle with an offset;
  PointType offset;
  for( unsigned int d=0; d < Dimension; d++ )
    {
    offset[d] = 1.1 + d;
    }
  unsigned long count = 0;
  float pointSetRadius = 100.0;
  for( float theta = 0; theta < 2.0 * itk::Math::pi; theta += 0.1 )
    {
    LabelType label = 1;
    if( theta > itk::Math::pi )
      {
      label = 2;
      }

    PointType fixedPoint;
    fixedPoint[0] = pointSetRadius * std::cos( theta );
    fixedPoint[1] = pointSetRadius * std::sin( theta );
    if( Dimension > 2 )
      {
      fixedPoint[2] = pointSetRadius * std::sin( theta );
      }
    fixedPoints->SetPoint( count, fixedPoint );
    fixedPoints->SetPointData( count, label );

    PointType movingPoint;
    movingPoint[0] = fixedPoint[0] + offset[0];
    movingPoint[1] = fixedPoint[1] + offset[1];
    if( Dimension > 2 )
      {
      movingPoint[2] = fixedPoint[2] + offset[2];
      }
    movingPoints->SetPoint( count, movingPoint );
    movingPoints->SetPointData( count, label );

    count++;
    }

  //
  // Simple translation transform for moving point set
  //
  typedef itk::TranslationTransform<double, Dimension> TranslationTransformType;
  typename TranslationTransformType::Pointer translationTransform = TranslationTransformType::New();
  translationTransform->SetIdentity();

  // Instantiate the metric
  typedef itk::LabeledPointSetToPointSetMetricv4<PointSetType> PointSetMetricType;
  typename PointSetMetricType::Pointer metric = PointSetMetricType::New();
  metric->SetFixedPointSet( fixedPoints );
  metric->SetMovingPointSet( movingPoints );
  metric->SetMovingTransform( translationTransform );
  metric->Initialize();

  typename PointSetMetricType::MeasureType value = metric->GetValue(), value2;
  typename PointSetMetricType::DerivativeType derivative, derivative2;
  metric->GetDerivative( derivative );
  metric->GetValueAndDerivative( value2, derivative2 );

  std::cout << "value: " << value << std::endl;
  std::cout << "derivative: " << derivative << std::endl;
  for( unsigned int d=0; d < metric->GetNumberOfParameters(); d++ )
    {
    if( std::fabs( derivative[d] - offset[d] ) / offset[d] > 0.01 )
      {
      std::cerr << "derivative does not match expected offset of " << offset << std::endl;
      return EXIT_FAILURE;
      }
    }

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

  // dump original fixed point set and transformed by derivative
  std::ofstream moving_str1( "sourceMoving.txt" );
  std::ofstream moving_str2( "targetMoving.txt" );

  count = 0;

  moving_str1 << "0 0 0 0" << std::endl;
  moving_str2 << "0 0 0 0" << std::endl;

  typename PointType::VectorType vector;
  for( unsigned int d = 0; d < metric->GetNumberOfParameters(); d++ )
    {
    vector[d] = derivative[count++];
    }

  typename PointSetType::PointsContainer::ConstIterator ItM = movingPoints->GetPoints()->Begin();
  while( ItM != movingPoints->GetPoints()->End() )
    {
    PointType sourcePoint = ItM.Value();
    PointType targetPoint = sourcePoint + vector;

    for( unsigned int d = 0; d < metric->GetNumberOfParameters(); d++ )
      {
      moving_str1 << sourcePoint[d] << " ";
      moving_str2 << targetPoint[d] << " ";
      }
    if( Dimension < 3 )
      {
      moving_str1 << "0 ";
      moving_str2 << "0 ";
      }
    moving_str1 << ItM.Index() << std::endl;
    moving_str2 << ItM.Index() << std::endl;

    ++ItM;
    }

  moving_str1 << "0 0 0 0" << std::endl;
  moving_str2 << "0 0 0 0" << std::endl;

  return EXIT_SUCCESS;
}

int itkLabeledPointSetMetricTest( int, char* [] )
{
  int result = EXIT_SUCCESS;

  if( itkLabeledPointSetMetricTestRun<2>() == EXIT_FAILURE )
    {
    std::cerr << "Failed for Dimension 2." << std::endl;
    result = EXIT_FAILURE;
    }

  if( itkLabeledPointSetMetricTestRun<3>() == EXIT_FAILURE )
    {
    std::cerr << "Failed for Dimension 3." << std::endl;
    result = EXIT_FAILURE;
    }

  return result;
}
