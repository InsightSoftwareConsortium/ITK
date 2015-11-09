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

#include "itkMath.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkTranslationTransform.h"
#include "itkMath.h"

/* Simple test to verify that class builds and runs.
 * Results are not verified. See ImageToImageMetricv4Test
 * for verification of basic metric functionality.
 *
 * TODO Numerical verification.
 */

int itkJointHistogramMutualInformationImageToImageMetricv4Test( int , char * [] )
{

  const unsigned int imageSize = 10;
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
    itMoving.Set( (count) );
    count++;
    ++itMoving;
    }

  /* Transforms */
  typedef itk::TranslationTransform<double,imageDimensionality>
                                                            FixedTransformType;
  typedef itk::TranslationTransform<double,imageDimensionality>
                                                            MovingTransformType;
  FixedTransformType::Pointer fixedTransform = FixedTransformType::New();
  MovingTransformType::Pointer movingTransform = MovingTransformType::New();
  fixedTransform->SetIdentity();
  movingTransform->SetIdentity();

  /* The metric */
  typedef itk::JointHistogramMutualInformationImageToImageMetricv4< ImageType,
                                                                ImageType,
                                                                ImageType >
                                                                  MetricType;
  MetricType::Pointer metric = MetricType::New();

  /* Assign images and transforms.
   * By not setting a virtual domain image or virtual domain settings,
   * the metric will use the fixed image for the virtual domain. */
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );
  metric->SetNumberOfHistogramBins( 6 );

  /* Initialize. */
  try
    {
    std::cout << "Calling Initialize..." << std::endl;
    metric->Initialize();
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cout << "Caught unexpected exception during Initialize: "
              << exc;
    return EXIT_FAILURE;
    }

  // Evaluate
  MetricType::MeasureType valueReturn1, valueReturn2;
  MetricType::DerivativeType derivativeReturn;
  try
    {
    std::cout << "Calling GetValue..." << std::endl;
    valueReturn1 = metric->GetValue();
    std::cout << "Calling GetValueAndDerivative..." << std::endl;
    metric->GetValueAndDerivative( valueReturn2, derivativeReturn );
    }
  catch( itk::ExceptionObject & exc )
    {
    std::cout << "Caught unexpected exception during GetValueAndDerivative: "
              << exc;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(valueReturn1, valueReturn2) )
    {
    std::cerr << "Value return results are not identical: " << valueReturn1
              << ", " << valueReturn2 << std::endl;
    }

  // Test that non-overlapping images will generate a warning
  // and return max value for metric value.
  MovingTransformType::ParametersType parameters(movingTransform->GetNumberOfParameters());
  parameters.Fill( static_cast<MovingTransformType::ParametersValueType>(1000) );
  movingTransform->SetParameters( parameters );
  MetricType::MeasureType expectedMetricMax;
  expectedMetricMax = itk::NumericTraits<MetricType::MeasureType>::max();
  std::cout << "Testing non-overlapping images. Expect a warning:" << std::endl;
  metric->GetValueAndDerivative( valueReturn2, derivativeReturn );
  if( metric->GetNumberOfValidPoints() != 0 || itk::Math::NotAlmostEquals( valueReturn2, expectedMetricMax ) )
    {
    std::cerr << "Failed testing for non-overlapping images. " << std::endl
              << "  Number of valid points: " << metric->GetNumberOfValidPoints() << std::endl
              << "  Metric value: " << valueReturn2 << std::endl
              << "  Expected metric max value: " << expectedMetricMax << std::endl;
    }
  movingTransform->SetIdentity();

  std::cout << "Test passed." << std::endl;

  //exercise methods
  metric->SetVarianceForJointPDFSmoothing(3);
  metric->GetVarianceForJointPDFSmoothing();

  return EXIT_SUCCESS;
}
