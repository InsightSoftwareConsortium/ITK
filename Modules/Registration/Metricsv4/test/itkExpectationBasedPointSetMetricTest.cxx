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

#include "itkExpectationBasedPointSetToPointSetMetricv4.h"


#include <fstream>

int itkExpectationBasedPointSetMetricTest( int, char* [] )
{
  const unsigned int Dimension = 2;

  typedef itk::PointSet<unsigned char, Dimension> PointSetType;

  typedef PointSetType::PointType PointType;

  PointSetType::Pointer fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  PointSetType::Pointer movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // Produce two simple point sets of 1) a circle and 2) an ellipse;

  unsigned long count = 0;
  for( float theta = 0; theta < 2.0 * vnl_math::pi; theta += 0.01 )
    {
    PointType fixedPoint;
    fixedPoint[0] = 2.0 * vcl_cos( theta );
    fixedPoint[1] = 1.0 * vcl_sin( theta );
    fixedPoints->SetPoint( count, fixedPoint );

    PointType movingPoint;
    movingPoint[0] = 1.0 * vcl_cos( theta + 0.5 * vnl_math::pi );
    movingPoint[1] = 1.0 * vcl_sin( theta + 0.5 * vnl_math::pi );
    movingPoints->SetPoint( count, movingPoint );

    count++;
    }

  // Instantiate the metric

  typedef itk::ExpectationBasedPointSetToPointSetMetricv4<PointSetType>
    PointSetMetricType;
  PointSetMetricType::Pointer metric = PointSetMetricType::New();
  metric->SetFixedPointSet( fixedPoints );
  metric->SetMovingPointSet( movingPoints );
  metric->Initialize();


  // Test derivative source using moving point set
  metric->SetGradientSource( PointSetMetricType::GRADIENT_SOURCE_MOVING );

  metric->GetValue();
  PointSetMetricType::DerivativeType derivative;
  metric->GetDerivative( derivative );

  std::ofstream moving_str1( "sourceMoving.txt" );
  std::ofstream moving_str2( "targetMoving.txt" );

  count = 0;

  moving_str1 << "0 0 0 0" << std::endl;
  moving_str2 << "0 0 0 0" << std::endl;

  PointSetType::PointsContainer::ConstIterator ItM =
    movingPoints->GetPoints()->Begin();
  while( ItM != movingPoints->GetPoints()->End() )
    {
    PointType sourcePoint = ItM.Value();
    PointType::VectorType vector;
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      vector[d] = derivative[count++];
      }
    PointType targetPoint = sourcePoint + vector;

    for( unsigned int d = 0; d < Dimension; d++ )
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

  // Test derivative source using fixed point set
  metric->SetGradientSource( PointSetMetricType::GRADIENT_SOURCE_FIXED );

  metric->GetValue();
  metric->GetDerivative( derivative );

  std::ofstream fixed_str1( "sourceFixed.txt" );
  std::ofstream fixed_str2( "targetFixed.txt" );

  count = 0;

  fixed_str1 << "0 0 0 0" << std::endl;
  fixed_str2 << "0 0 0 0" << std::endl;

  PointSetType::PointsContainer::ConstIterator ItF =
    fixedPoints->GetPoints()->Begin();
  while( ItF != fixedPoints->GetPoints()->End() )
    {
    PointType sourcePoint = ItF.Value();
    PointType::VectorType vector;
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      vector[d] = derivative[count++];
      }
    PointType targetPoint = sourcePoint + vector;

    for( unsigned int d = 0; d < Dimension; d++ )
      {
      fixed_str1 << sourcePoint[d] << " ";
      fixed_str2 << targetPoint[d] << " ";
      }
    if( Dimension < 3 )
      {
      fixed_str1 << "0 ";
      fixed_str2 << "0 ";
      }
    fixed_str1 << ItF.Index() << std::endl;
    fixed_str2 << ItF.Index() << std::endl;

    ++ItF;
    }

  fixed_str1 << "0 0 0 0" << std::endl;
  fixed_str2 << "0 0 0 0" << std::endl;

  return EXIT_SUCCESS;
}
