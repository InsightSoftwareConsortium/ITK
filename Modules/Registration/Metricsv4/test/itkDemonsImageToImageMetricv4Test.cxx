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
#include "itkDemonsImageToImageMetricv4.h"
#include "itkTranslationTransform.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

/* Simple test to verify that class builds and runs.
 * Results are not verified. See ImageToImageMetricv4Test
 * for verification of basic metric functionality.
 *
 * TODO Numerical verification.
 */

int itkDemonsImageToImageMetricv4Test(int, char ** const)
{

  const unsigned int imageSize = 5;
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
    itFixed.Set( count*count );
    count++;
    ++itFixed;
    }

  itk::ImageRegionIteratorWithIndex<ImageType> itMoving( movingImage, region );

  itMoving.GoToBegin();
  count = 1;

  while( !itMoving.IsAtEnd() )
    {
    itMoving.Set( 1.0/(count*count) );
    count++;
    ++itMoving;
    }

  /* Transforms */
  typedef itk::TranslationTransform<double,imageDimensionality> TranslationTransformType;
  typedef itk::GaussianSmoothingOnUpdateDisplacementFieldTransform< double, imageDimensionality> DisplacementTransformType;

  TranslationTransformType::Pointer translationTransform = TranslationTransformType::New();
  translationTransform->SetIdentity();

  DisplacementTransformType::Pointer displacementTransform = DisplacementTransformType::New();
  typedef DisplacementTransformType::DisplacementFieldType DisplacementFieldType;
  DisplacementFieldType::Pointer field = DisplacementFieldType::New();
  field->SetRegions( fixedImage->GetLargestPossibleRegion() );
  field->CopyInformation( fixedImage );
  field->Allocate();
  DisplacementTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 0 );
  field->FillBuffer( zeroVector );
  displacementTransform->SetDisplacementField( field );
  displacementTransform->SetGaussianSmoothingVarianceForTheUpdateField( 5 );
  displacementTransform->SetGaussianSmoothingVarianceForTheTotalField( 6 );

  /* The metric */
  typedef itk::DemonsImageToImageMetricv4< ImageType, ImageType, ImageType > MetricType;

  MetricType::Pointer metric = MetricType::New();

  /* Assign images and transforms.
   * By not setting a virtual domain image or virtual domain settings,
   * the metric will use the fixed image for the virtual domain. */
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( translationTransform );
  metric->SetMovingTransform( displacementTransform );

  /* Test 1st with fixed image gradient source. This is the default */
  metric->SetGradientSource( MetricType::GRADIENT_SOURCE_FIXED );
  if( metric->GetGradientSource() != MetricType::GRADIENT_SOURCE_FIXED )
    {
    std::cerr << "Failed setting fixed image gradient source." << std::endl;
    return EXIT_FAILURE;
    }

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

  // Evaluate with GetValueAndDerivative
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

  // Test same value returned by different methods
  std::cout << "Check Value return values..." << std::endl;
  if( itk::Math::NotExactlyEquals(valueReturn1, valueReturn2) )
    {
    std::cerr << "Results for Value don't match: " << valueReturn1
              << ", " << valueReturn2 << std::endl;
    }

  /* Test with moving image as gradient source. The default is
   * to have the fixed image used for image gradients.  */
  metric->SetFixedTransform( translationTransform );
  metric->SetMovingTransform( displacementTransform );
  metric->SetGradientSource( MetricType::GRADIENT_SOURCE_MOVING );
  if( metric->GetGradientSource() != MetricType::GRADIENT_SOURCE_MOVING )
    {
    std::cerr << "Failed setting moving image gradient source." << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    std::cout << "Calling Initialize..." << std::endl;
    metric->Initialize();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cerr << "Caught unexpected exception during initialize with moving "
              << "image gradient source: " << exc << std::endl;
    return EXIT_FAILURE;
    }

  // Evaluate with GetValueAndDerivative
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

  // Test same value returned by different methods
  std::cout << "Check Value return values..." << std::endl;
  if( itk::Math::NotExactlyEquals(valueReturn1, valueReturn2) )
    {
    std::cerr << "Moving image gradient source: results for Value don't match: " << valueReturn1
              << ", " << valueReturn2 << std::endl;
    }

  /* Test expected exceptions when transform is not right type */
  metric->SetMovingTransform( translationTransform );
  TRY_EXPECT_EXCEPTION( metric->Initialize() );

  /* Exercise accessor method */
  const MetricType::InternalComputationValueType testValue = static_cast<MetricType::InternalComputationValueType>(0.5);
  metric->SetIntensityDifferenceThreshold( testValue );
  if( itk::Math::NotExactlyEquals(metric->GetIntensityDifferenceThreshold(), testValue) )
    {
    std::cerr << "Set/GetIntensityDifferenceThreshold failed." << std::endl;
    return EXIT_FAILURE;
    }

  /* Print self */
  metric->Print( std::cout );

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
