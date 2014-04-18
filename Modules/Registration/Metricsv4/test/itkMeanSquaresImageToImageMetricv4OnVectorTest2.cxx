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

/*
 * Compare metric evaluation of scalar image and vector image.
 * Serves as a numerical verification of vector image metric evaluation.
 */

template<typename TMetric>
int itkMeanSquaresImageToImageMetricv4OnVectorTest2Run( typename TMetric::MeasureType & measureReturn, typename TMetric::DerivativeType & derivativeReturn )
{
  const unsigned int imageSize = 5;
  const unsigned int imageDimensionality = 3;

  typedef typename TMetric::FixedImageType  ImageType;

  typename ImageType::SizeType       size;
  size.Fill( imageSize );
  typename ImageType::IndexType      index;
  index.Fill( 0 );
  typename ImageType::RegionType     region;
  region.SetSize( size );
  region.SetIndex( index );
  typename ImageType::SpacingType    spacing;
  spacing.Fill(1.0);
  typename ImageType::PointType      origin;
  origin.Fill(0);
  typename ImageType::DirectionType  direction;
  direction.SetIdentity();

  /* Create simple test images. */
  typename ImageType::Pointer fixedImage = ImageType::New();
  fixedImage->SetRegions( region );
  fixedImage->SetSpacing( spacing );
  fixedImage->SetOrigin( origin );
  fixedImage->SetDirection( direction );
  fixedImage->Allocate();

  typename ImageType::Pointer movingImage = ImageType::New();
  movingImage->SetRegions( region );
  movingImage->SetSpacing( spacing );
  movingImage->SetOrigin( origin );
  movingImage->SetDirection( direction );
  movingImage->Allocate();

  /*
   * Fill both images
   */
  typedef typename ImageType::PixelType PixelType;

  itk::ImageRegionIterator<ImageType> itFixed( fixedImage, region );
  itk::ImageRegionIteratorWithIndex<ImageType> itMoving( movingImage, region );

  itFixed.GoToBegin();
  itMoving.GoToBegin();

  unsigned int count = 1;
  while( !itFixed.IsAtEnd() )
    {
    PixelType pix1(count);
    PixelType pix2( 1.0 / count );
    itFixed.Set( pix1 );
    itMoving.Set( pix2 );
    count++;
    ++itFixed;
    ++itMoving;
    }

  /* Transforms */
  typedef itk::TranslationTransform<double,imageDimensionality> FixedTransformType;
  typedef itk::TranslationTransform<double,imageDimensionality> MovingTransformType;

  typename FixedTransformType::Pointer fixedTransform = FixedTransformType::New();
  typename MovingTransformType::Pointer movingTransform = MovingTransformType::New();

  fixedTransform->SetIdentity();
  movingTransform->SetIdentity();

  typename TMetric::Pointer metric = TMetric::New();

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
  try
    {
    std::cout << "Calling GetValueAndDerivative..." << std::endl;
    metric->GetValueAndDerivative( measureReturn, derivativeReturn );
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cerr << "Caught unexpected exception during GetValueAndDerivative: " << exc;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

///////////////////////////////////////////////////////////////////////

int itkMeanSquaresImageToImageMetricv4OnVectorTest2(int, char ** const)
{
  const unsigned int imageDimensionality = 3;
  const unsigned int vectorLength = 3;

  /* The vector metric */
  typedef itk::Vector<double, vectorLength>                  VectorType;
  typedef itk::Image< VectorType, imageDimensionality >      VectorImageType;

  typedef itk::VectorImageToImageMetricTraitsv4< VectorImageType, VectorImageType, VectorImageType, vectorLength, double >    MetricTraitsType;
  typedef itk::MeanSquaresImageToImageMetricv4< VectorImageType, VectorImageType, VectorImageType, double, MetricTraitsType > VectorMetricType;

  VectorMetricType::MeasureType     vectorMeasure = 0.0;
  VectorMetricType::DerivativeType  vectorDerivative;
  vectorDerivative.Fill(0);

  itkMeanSquaresImageToImageMetricv4OnVectorTest2Run<VectorMetricType>( vectorMeasure, vectorDerivative );
  std::cout << "vectorMeasure: " << vectorMeasure << " vectorDerivative: " << vectorDerivative << std::endl;

  /* The scalar metric */
  typedef itk::Image< double, imageDimensionality >                                                 ScalarImageType;
  typedef itk::MeanSquaresImageToImageMetricv4< ScalarImageType, ScalarImageType, ScalarImageType > ScalarMetricType;

  ScalarMetricType::MeasureType     scalarMeasure = 0.0;
  ScalarMetricType::DerivativeType  scalarDerivative;
  scalarDerivative.Fill(0);

  itkMeanSquaresImageToImageMetricv4OnVectorTest2Run<ScalarMetricType>( scalarMeasure, scalarDerivative );
  std::cout << "scalarMeasure: " << scalarMeasure << " scalarDerivative: " << scalarDerivative << std::endl;

  /* Compare */
  double tolerance = 1e-8;
  if( std::fabs( scalarMeasure - ( vectorMeasure / vectorLength ) ) > tolerance )
    {
    std::cerr << "Measures do not match within tolerance. scalarMeasure, vectorMeasure: " << scalarMeasure << ", " << vectorMeasure << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Measure values match." << std::endl;

  for( itk::SizeValueType n = 0; n < scalarDerivative.Size(); n++ )
    {
    if( std::fabs( scalarDerivative[n] - ( vectorDerivative[n] / vectorLength ) ) > tolerance )
      {
      std::cerr << "Derivatives do not match within tolerance. scalarDerivative, vectorDerivative: " << scalarDerivative << std::endl << vectorDerivative << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "Derivative values match." << std::endl;

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
