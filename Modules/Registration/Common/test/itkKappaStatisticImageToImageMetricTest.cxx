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

#include "itkKappaStatisticImageToImageMetric.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkTranslationTransform.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

/**
 *  This test exercised the various methods in the
 *  itkKappaStatisticImageToImageMetric class.  Two binary images are
 *  created for testing purposes -- one of a square and another of the
 *  same square translated in both x and y.
 *
 */

int itkKappaStatisticImageToImageMetricTest(int, char* [] )
{

  const unsigned int Dimension = 2;

  typedef unsigned char FixedImagePixelType;
  typedef unsigned char MovingImagePixelType;

  typedef double CoordRepPixelType;

  typedef double GradientPixelType;

  typedef itk::Image< FixedImagePixelType, Dimension >                              FixedImageType;
  typedef itk::Image< MovingImagePixelType, Dimension >                             MovingImageType;
  typedef itk::Image< GradientPixelType, Dimension >                                GradientImageType;

  typedef itk::KappaStatisticImageToImageMetric< FixedImageType, MovingImageType >  MetricType;
  typedef itk::ImageRegionIteratorWithIndex< FixedImageType >                       FixedImageIteratorType;
  typedef itk::ImageRegionIteratorWithIndex< MovingImageType >                      MovingImageIteratorType;

  typedef itk::ImageRegionIteratorWithIndex< GradientImageType >                    GradientImageIteratorType;
  typedef itk::TranslationTransform< CoordRepPixelType, Dimension >                 TransformType;
  typedef itk::NearestNeighborInterpolateImageFunction< MovingImageType, CoordRepPixelType >
    InterpolatorType;


  double epsilon = 0.000001;

  TransformType::Pointer    transform    = TransformType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  FixedImageType::SizeType fixedImageSize;
  fixedImageSize.Fill( 128 );

  // Create fixed image
  FixedImageType::Pointer fixedImage = FixedImageType::New();
  fixedImage->SetRegions( fixedImageSize );
  fixedImage->Allocate( true ); // initialize buffer to zero
  fixedImage->Update();

  FixedImageIteratorType fixedIt( fixedImage, fixedImage->GetBufferedRegion() );
  for( fixedIt.GoToBegin(); !fixedIt.IsAtEnd(); ++fixedIt )
    {
    FixedImageType::IndexType index = fixedIt.GetIndex();
    if( index[0] >= 48 && index[0] <= 80 && index[1] >= 48 && index[1] <= 80 )
      {
      fixedIt.Set(255);
      }
    }

  MovingImageType::SizeType movingImageSize;
  movingImageSize.Fill( 128 );

  // Create moving image
  MovingImageType::Pointer movingImage = MovingImageType::New();
  movingImage->SetRegions( movingImageSize );
  movingImage->Allocate( true ); // initialize buffer to zero
  movingImage->Update();

  MovingImageIteratorType movingIt( movingImage, movingImage->GetBufferedRegion() );
  for( movingIt.GoToBegin(); !movingIt.IsAtEnd(); ++movingIt )
    {
    MovingImageType::IndexType index = movingIt.GetIndex();
    if( index[0] >= 55 && index[0] <= 87 && index[1] >= 55 && index[1] <= 87 )
      {
      movingIt.Set(255);
      }
    }

  MetricType::Pointer metric = MetricType::New();

  EXERCISE_BASIC_OBJECT_METHODS( metric, KappaStatisticImageToImageMetric,
    ImageToImageMetric );

  MetricType::RealType foregroundValue = 255;
  metric->SetForegroundValue( foregroundValue );
  TEST_SET_GET_VALUE( foregroundValue, metric->GetForegroundValue() );

  bool useComplement = false;
  metric->SetComplement( useComplement );
  TEST_SET_GET_VALUE( useComplement, metric->GetComplement() );

  metric->ComplementOff();
  TEST_SET_GET_VALUE( false, metric->GetComplement() );

  transform->SetIdentity();
  metric->SetTransform( transform );

  TransformType::ParametersType parameters = transform->GetParameters();

  // Test error conditions
  //
  TRY_EXPECT_EXCEPTION( metric->GetValue( parameters ) );

  metric->SetFixedImage( fixedImage );

  TRY_EXPECT_EXCEPTION( metric->GetValue( parameters ) );

  metric->SetMovingImage( movingImage );
  metric->SetInterpolator( interpolator );
  metric->SetFixedImageRegion( fixedImage->GetBufferedRegion() );

  MetricType::DerivativeType derivative;
  TRY_EXPECT_EXCEPTION( metric->GetDerivative( parameters, derivative ) );

  TRY_EXPECT_NO_EXCEPTION( metric->Initialize() );

  metric->SetFixedImage( NULL );
  TRY_EXPECT_EXCEPTION( metric->GetDerivative( parameters, derivative ) );

  metric->SetFixedImage( fixedImage );

  // Test the GetValue method
  //

  // The value 0.620753 was computed by hand for these two images
  double expectedMatchMeasure = 0.620753;
  MetricType::MeasureType value = metric->GetValue( parameters );
  if( !itk::Math::FloatAlmostEqual( (double)value, expectedMatchMeasure, 10, epsilon ) )
    {
      std::cerr << "Error !" << std::endl;
      std::cerr << "Expected: " << expectedMatchMeasure << " but got "
        << static_cast< double >( value ) << std::endl;
      std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }


  // Test the ComputeGradient method
  //
  metric->ComputeGradient();

  GradientImageType::Pointer xGradImage = GradientImageType::New();
  xGradImage->SetRegions( movingImageSize );
  xGradImage->Allocate( true ); // initialize buffer to zero
  xGradImage->Update();

  GradientImageType::Pointer yGradImage = GradientImageType::New();
  yGradImage->SetRegions( movingImageSize );
  yGradImage->Allocate( true ); // initialize buffer to zero
  yGradImage->Update();

  GradientImageIteratorType xGradIt( xGradImage, xGradImage->GetBufferedRegion() );
  GradientImageIteratorType yGradIt( yGradImage, yGradImage->GetBufferedRegion() );

  xGradIt.GoToBegin();
  yGradIt.GoToBegin();

  // Construct the gradient images explicitly based on what we know
  // they should be and use them to validate metric's version
  while( !xGradIt.IsAtEnd() )
    {
    GradientImageType::IndexType index = xGradIt.GetIndex();

    if( ( index[0] == 54 || index[0] == 55 ) && index[1] >= 55 && index[1] <= 87 )
      {
      xGradIt.Set(1);
      }
    if( ( index[0] == 87 || index[0] == 88  ) && index[1] >= 55 && index[1] <= 87 )
      {
      xGradIt.Set(-1);
      }
    if( ( index[1] == 54 || index[1] == 55  ) && index[0] >= 55 && index[0] <= 87 )
      {
      yGradIt.Set(1);
      }
    if( ( index[1] == 87 || index[1] == 88  ) &&(index[0] >= 55) && index[0] <= 87)
      {
      yGradIt.Set(-1);
      }

    ++xGradIt;
    ++yGradIt;
    }

  typedef itk::ImageRegionIteratorWithIndex< const MetricType::GradientImageType > GradIteratorType;
  GradIteratorType gradIt( metric->GetGradientImage(), metric->GetGradientImage()->GetBufferedRegion() );
  gradIt.GoToBegin();
  xGradIt.GoToBegin();
  yGradIt.GoToBegin();
  while( !gradIt.IsAtEnd() )
    {
    if( itk::Math::NotAlmostEquals( ( gradIt.Get())[0], xGradIt.Get() ) ||
        itk::Math::NotAlmostEquals( ( gradIt.Get())[1], yGradIt.Get() ) )
      {
      std::cerr << "Error !" << std::endl;
      std::cerr << "Expected: [" << static_cast< double >( gradIt.Get()[0] )
        << ", " << static_cast< double >( gradIt.Get()[1] ) << "], but got ["
        << static_cast< double >( xGradIt.Get() ) << ", "
        << static_cast< double >( yGradIt.Get() ) << "]"
        << std::endl;
      std::cerr << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }

    ++gradIt;
    ++xGradIt;
    ++yGradIt;
    }


  // Test the GetDerivative method
  //
  metric->GetDerivative( parameters, derivative );

  // The value 0.0477502 was computed by hand
  double expectedDerivativeMeasure = -0.0477502;
  for( unsigned int i = 0; i < derivative.size(); ++i )
    {
    if( !itk::Math::FloatAlmostEqual( (double)derivative[i], expectedDerivativeMeasure, 10, epsilon ) )
      {
        std::cerr << "Error !" << std::endl;
        std::cerr << "Expected: " << expectedDerivativeMeasure << " but got "
          << static_cast< double >( derivative[i] )
          << " at index [" << i << "]" << std::endl;
        std::cerr << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }


  // Test the GetValueAndDerivative method
  //
  metric->GetValueAndDerivative( parameters, value, derivative );

  if( !itk::Math::FloatAlmostEqual( (double)value, expectedMatchMeasure, 10, epsilon ) )
    {
      std::cerr << "Error !" << std::endl;
      std::cerr << "Expected: " << expectedMatchMeasure << " but got "
        << static_cast< double >( value ) << std::endl;
      std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
  for( unsigned int i = 0; i < derivative.size(); ++i )
    {
    if( !itk::Math::FloatAlmostEqual( (double)derivative[i], expectedDerivativeMeasure, 10, epsilon ) )
      {
        std::cerr << "Error !" << std::endl;
        std::cerr << "Expected: " << expectedDerivativeMeasure << " but got "
          << static_cast< double >( derivative[i] )
          << " at index [" << i << "]" << std::endl;
        std::cerr << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }


  // Test with Complement set to true
  //
  useComplement = true;
  metric->SetComplement( useComplement );
  TEST_SET_GET_VALUE( useComplement, metric->GetComplement() );

  metric->ComplementOn();
  TEST_SET_GET_VALUE( true, metric->GetComplement() );

  // The value 0.379247 was computed by hand
  expectedMatchMeasure = 0.379247;
  value = metric->GetValue( parameters );
  if( !itk::Math::FloatAlmostEqual( (double)value, expectedMatchMeasure, 10, epsilon ) )
    {
      std::cerr << "Error !" << std::endl;
      std::cerr << "Expected: " << expectedMatchMeasure << " but got "
        << static_cast< double >( value ) << std::endl;
      std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
