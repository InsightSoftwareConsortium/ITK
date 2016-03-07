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

#include "itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4.h"
#include "itkTranslationTransform.h"

#include <fstream>

template<unsigned int Dimension>
int itkJensenHavrdaCharvatTsallisPointSetMetricTestRun()
{
  typedef itk::PointSet<unsigned char, Dimension> PointSetType;

  typedef typename PointSetType::PointType PointType;
  typedef typename PointType::VectorType   VectorType;

  typename PointSetType::Pointer fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  typename PointSetType::Pointer movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // Produce two simple point sets of 1) a circle and 2) the same circle with an offset
  PointType offset;
  float normOffset = 0;
  VectorType normalizedOffset;
  for( unsigned int d=0; d < Dimension; d++ )
    {
    offset[d] = 2;
    normOffset += itk::Math::sqr( offset[d] );
    normalizedOffset[d] = offset[d];
    }
  normOffset = std::sqrt( normOffset );
  normalizedOffset /= normOffset;

  unsigned long count = 0;
  for( float theta = 0; theta < 2.0 * itk::Math::pi; theta += 0.1 )
    {
    PointType fixedPoint;
    float radius = 100.0;
    fixedPoint[0] = radius * std::cos( theta );
    fixedPoint[1] = radius * std::sin( theta );
// simplistic point set test:
//    fixedPoint[0] = 1;
//    fixedPoint[1] = 1;
    if( Dimension > 2 )
      {
      fixedPoint[2] = radius * std::sin( theta );
//      fixedPoint[2] = 1;
      }
    fixedPoints->SetPoint( count, fixedPoint );

    PointType movingPoint;
    movingPoint[0] = fixedPoint[0] + offset[0];
    movingPoint[1] = fixedPoint[1] + offset[1];
    if( Dimension > 2 )
      {
      movingPoint[2] = fixedPoint[2] + offset[2];
      }
    movingPoints->SetPoint( count, movingPoint );

    count++;
    }

  // Simple translation transform for moving point set
  typedef itk::TranslationTransform<double, Dimension> TranslationTransformType;
  typename TranslationTransformType::Pointer translationTransform = TranslationTransformType::New();
  translationTransform->SetIdentity();

  // check various alpha values between accepted values of [1.0, 2.0]

  unsigned int numberOfAlphaValues = 6;
  float alphaValues[] = { 1.0f, 1.2f, 1.4f, 1.6f, 1.8f, 2.0f };
  float metricValues2D[] = { 0.143842f, -0.0129571f, -0.00105768f, -0.000115118f, -1.40956e-05f, -1.84099e-06f };
  float metricValues3D[] = { 0.175588f, -0.0086854f, -0.000475248f, -3.46729e-05f, -2.84585e-06f, -2.49151e-07f };

  for( unsigned int i = 0; i < numberOfAlphaValues; i++ )
    {

    std::cout << "Alpha = " << alphaValues[i] << std::endl;

    // Instantiate the metric ( alpha = 1.0 )
    typedef itk::JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<PointSetType> PointSetMetricType;
    typename PointSetMetricType::Pointer metric = PointSetMetricType::New();
    metric->SetFixedPointSet( fixedPoints );
    metric->SetMovingPointSet( movingPoints );
    metric->SetMovingTransform( translationTransform );
    metric->SetAlpha( alphaValues[i] );
    metric->Initialize();

    typename PointSetMetricType::MeasureType value = metric->GetValue(), value2;
    typename PointSetMetricType::DerivativeType derivative, derivative2;
    metric->GetDerivative( derivative );
    metric->GetValueAndDerivative( value2, derivative2 );

    derivative /= derivative.magnitude();
    derivative2 /= derivative2.magnitude();

    std::cout << "value: " << value << std::endl;
    std::cout << "normalized derivative: " << derivative << std::endl;

    for( unsigned int d = 0; d < metric->GetNumberOfParameters(); d++ )
      {
      if( std::fabs( derivative[d] - normalizedOffset[d] ) / normalizedOffset[d] > 0.01 )
        {
        std::cerr << "derivative does not match expected normalized offset of " << offset << std::endl;
        return EXIT_FAILURE;
        }
      }

    if( Dimension == 2 )
      {
      if( std::fabs( value - metricValues2D[i] ) > 0.01 )
        {
        std::cerr << "calculated value is different than expected." << std::endl;
        }
      }
    else if( Dimension == 3 )
      {
      if( std::fabs( value - metricValues3D[i] ) > 0.01 )
        {
        std::cerr << "calculated value is different than expected." << std::endl;
        }
      }

    // Check for the same results from different methods
    if( std::fabs( value - value2 ) > 0.01 )
      {
      std::cerr << "value does not match between calls to different methods: "
                << "value: " << value << " value2: " << value2 << std::endl;
      }
    if( derivative != derivative2 )
      {
      std::cerr << "derivative does not match between calls to different methods: "
                << "derivative: " << derivative << " derivative2: " << derivative2 << std::endl;
      }

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
    }

  return EXIT_SUCCESS;
}

int itkJensenHavrdaCharvatTsallisPointSetMetricTest( int, char* [] )
{
  int result = EXIT_SUCCESS;

  if( itkJensenHavrdaCharvatTsallisPointSetMetricTestRun<2>() == EXIT_FAILURE )
    {
    std::cerr << "Failed for Dimension 2." << std::endl;
    result = EXIT_FAILURE;
    }

  if( itkJensenHavrdaCharvatTsallisPointSetMetricTestRun<3>() == EXIT_FAILURE )
    {
    std::cerr << "Failed for Dimension 3." << std::endl;
    result = EXIT_FAILURE;
    }

  return result;
}
