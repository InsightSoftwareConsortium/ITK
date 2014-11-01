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
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkTranslationTransform.h"

/*
 * Simple test to run using unix 'time' function for speed test.
 */

int itkMeanSquaresImageToImageMetricv4SpeedTest(int argc, char *argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "usage: " << argv[0] << ": image-dimension number-of-reps" << std::endl;
    return EXIT_FAILURE;
    }
  int imageSize = atoi( argv[1] );
  int numberOfReps = atoi( argv[2] );

  std::cout << "image dim: " << imageSize << ", reps: " << numberOfReps << std::endl;

  const unsigned int imageDimensionality = 3;
  typedef itk::Image< double, imageDimensionality >              ImageType;

  ImageType::SizeType       size;
  size.Fill( imageSize );
  ImageType::IndexType      index;
  index.Fill( 0 );
  ImageType::RegionType     region;
  region.SetSize( size );
  region.SetIndex( index );
  ImageType::SpacingType    spacing;
  spacing.Fill(1.0);
  ImageType::PointType      origin;
  origin.Fill(0);
  ImageType::DirectionType  direction;
  direction.SetIdentity();

  /* Create simple test images. */
  ImageType::Pointer fixedImage = ImageType::New();
  fixedImage->SetRegions( region );
  fixedImage->SetSpacing( spacing );
  fixedImage->SetOrigin( origin );
  fixedImage->SetDirection( direction );
  fixedImage->Allocate();

  ImageType::Pointer movingImage = ImageType::New();
  movingImage->SetRegions( region );
  movingImage->SetSpacing( spacing );
  movingImage->SetOrigin( origin );
  movingImage->SetDirection( direction );
  movingImage->Allocate();

  /* Fill images */
  itk::ImageRegionIterator<ImageType> itFixed( fixedImage, region );
  itFixed.GoToBegin();
  unsigned int count = 1;
  while( !itFixed.IsAtEnd() )
    {
    itFixed.Set( count );
    count++;
    ++itFixed;
    }

  itk::ImageRegionIteratorWithIndex<ImageType> itMoving( movingImage, region );

  itMoving.GoToBegin();
  count = 1;

  while( !itMoving.IsAtEnd() )
    {
    itMoving.Set( count*count );
    count++;
    ++itMoving;
    }

  /* Transforms */
  typedef itk::TranslationTransform<double,imageDimensionality> FixedTransformType;
  typedef itk::TranslationTransform<double,imageDimensionality> MovingTransformType;

  FixedTransformType::Pointer fixedTransform = FixedTransformType::New();
  MovingTransformType::Pointer movingTransform = MovingTransformType::New();

  fixedTransform->SetIdentity();
  movingTransform->SetIdentity();

  /* The metric */
  typedef itk::MeanSquaresImageToImageMetricv4< ImageType, ImageType, ImageType > MetricType;

  MetricType::Pointer metric = MetricType::New();

  /* Assign images and transforms.
   * By not setting a virtual domain image or virtual domain settings,
   * the metric will use the fixed image for the virtual domain. */
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );

  /* Initialize. */
  std::cout << "Calling Initialize..." << std::endl;
  metric->Initialize();

  // Evaluate with GetValueAndDerivative
  MetricType::MeasureType valueReturn1;
  MetricType::DerivativeType derivativeReturn;

  MetricType::MeasureType sum = itk::NumericTraits<MetricType::MeasureType>::ZeroValue();
  for( int r=0; r < numberOfReps; r++ )
    {
    metric->GetValueAndDerivative( valueReturn1, derivativeReturn );
    //Sum results to prevent optimizations
    sum += valueReturn1 + derivativeReturn[0];
    }

  std::cout << "sum: " << sum << std::endl;

  return EXIT_SUCCESS;
}
