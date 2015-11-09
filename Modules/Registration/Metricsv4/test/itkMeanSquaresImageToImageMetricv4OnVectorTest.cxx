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
#include "itkVectorImageToImageMetricTraitsv4.h"
#include "itkMath.h"

/* Simple test to verify that class builds and runs.
 * Results are not verified. See ImageToImageObjectMetricTest
 * for verification of basic metric functionality.
 *
 * TODO Numerical verification.
 */

int itkMeanSquaresImageToImageMetricv4OnVectorTest(int, char ** const)
{

  const unsigned int imageSize = 5;
  const unsigned int imageDimensionality = 3;
  const unsigned int vectorLength = 2;

  typedef itk::Vector<double, vectorLength>             VectorType;
  typedef itk::Image< VectorType, imageDimensionality > ImageType;

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
    VectorType pix;
    pix[0] = count*count;
    pix[1] = pix[0];
    itFixed.Set( pix );
    count++;
    ++itFixed;
    }

  itk::ImageRegionIteratorWithIndex<ImageType> itMoving( movingImage, region );

  itMoving.GoToBegin();
  count = 1;

  while( !itMoving.IsAtEnd() )
    {
    VectorType pix;
    pix[0] = 1.0/(count*count);
    pix[1] = pix[0];
    itMoving.Set( pix );
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
  typedef itk::VectorImageToImageMetricTraitsv4< ImageType, ImageType, ImageType, vectorLength, double > MetricTraitsType;
  typedef itk::MeanSquaresImageToImageMetricv4< ImageType, ImageType, ImageType, double, MetricTraitsType > MetricType;

  MetricType::Pointer metric = MetricType::New();

  /* Assign images and transforms.
   * By not setting a virtual domain image or virtual domain settings,
   * the metric will use the fixed image for the virtual domain. */
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );
  metric->SetMaximumNumberOfThreads( 1 );

  metric->DebugOn();

  /* Initialize. */
  try
    {
    std::cout << "Calling Initialize..." << std::endl;
    metric->Initialize();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cerr << "Caught unexpected exception during Initialize: " << exc << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Initialized" << std::endl;

  /* Evaluate with GetValueAndDerivative */
  MetricType::MeasureType valueReturn1, valueReturn2;
  MetricType::DerivativeType derivativeReturn;

  try
    {
    std::cout << "Calling GetValueAndDerivative..." << std::endl;
    metric->GetValueAndDerivative( valueReturn1, derivativeReturn );
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cout << "Caught unexpected exception during GetValueAndDerivative: "
              << exc;
    return EXIT_FAILURE;
    }

  /* Re-initialize. */
  try
    {
    std::cout << "Calling Initialize..." << std::endl;
    metric->Initialize();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cerr << "Caught unexpected exception during re-initialize: " << exc << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    std::cout << "Calling GetValue..." << std::endl;
    valueReturn2 = metric->GetValue();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cout << "Caught unexpected exception during GetValue: "
              << exc;
    return EXIT_FAILURE;
    }

  /* Test same value returned by different methods */
  std::cout << "Check Value return values..." << std::endl;
  if( itk::Math::NotExactlyEquals(valueReturn1, valueReturn2) )
    {
    std::cerr << "Results for Value don't match: " << valueReturn1
              << ", " << valueReturn2 << std::endl;
    }
  else
    {
    std::cout << "Metric value = " << valueReturn1 << std::endl;
    std::cout << "Gradient value = " << derivativeReturn << std::endl;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
