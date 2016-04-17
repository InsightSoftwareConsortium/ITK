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
#include "itkCorrelationImageToImageMetricv4.h"
#include "itkTranslationTransform.h"
#include "itkMath.h"

/* Simple test to verify that class builds and runs.
 * Results are not verified. See ImageToImageMetricv4Test
 * for verification of basic metric functionality.
 *
 * TODO Numerical verification.
 */
template<typename TIndexType, typename TPointType>
double itkCorrelationImageToImageMetricv4Test_GetToyImagePixelValue(TIndexType index, TPointType offset, const unsigned int Dim, double c)
{
  double v = 0.0;
  for(unsigned int i=0; i<Dim; i++)
    {
      v += (index[i]+offset[i])*(index[i]+offset[i]);
    }

  v = std::exp( -1.0 * v / 8 );
  v += c;

  return v;
}

template<typename TMetricPointer, typename TValue, typename TDerivativeType>
int itkCorrelationImageToImageMetricv4Test_WithSpecifiedThreads(TMetricPointer &metric,
                                                                TValue &value,
                                                                TDerivativeType &derivative)
{
  typedef typename TMetricPointer::ObjectType MetricType;

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
  typename MetricType::MeasureType valueReturn1, valueReturn2;
  typename MetricType::DerivativeType derivativeReturn;

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


  std::cout << "value:" << valueReturn1 << std::endl;
  std::cout << "derivativeReturn:" << derivativeReturn << std::endl;

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

  value = valueReturn1;
  derivative = derivativeReturn;

  return EXIT_SUCCESS;
}

int itkCorrelationImageToImageMetricv4Test(int, char ** const)
{

  const unsigned int imageSize = 20;
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
  typedef ImageType::IndexType IndexType;

  typedef ImageType::PointType PointType;
  PointType p0;
  for(unsigned int i=0; i<imageDimensionality; i++) p0[i]=0;

  itFixed.GoToBegin();
  unsigned int count = 1;
  while( !itFixed.IsAtEnd() )
    {
    IndexType ind = itFixed.GetIndex();
    double v = itkCorrelationImageToImageMetricv4Test_GetToyImagePixelValue(ind, p0, imageDimensionality, 0);
    itFixed.Set( v  );
    count++;
    ++itFixed;
    }

  itk::ImageRegionIteratorWithIndex<ImageType> itMoving( movingImage, region );

  itMoving.GoToBegin();
  count = 1;

  PointType p1;
  p1[0] = 1; p1[1] = 0.5; p1[2] = 0.25;

  while( !itMoving.IsAtEnd() )
    {
    IndexType ind = itMoving.GetIndex();
    double v = itkCorrelationImageToImageMetricv4Test_GetToyImagePixelValue(ind, p1, imageDimensionality, 0);
    itMoving.Set( v );
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
  typedef itk::CorrelationImageToImageMetricv4< ImageType, ImageType, ImageType > MetricType;

  MetricType::Pointer metric = MetricType::New();

  /* Assign images and transforms.
   * By not setting a virtual domain image or virtual domain settings,
   * the metric will use the fixed image for the virtual domain. */
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( fixedTransform );
  metric->SetMovingTransform( movingTransform );

  MetricType::MeasureType value1, value2;
  MetricType::DerivativeType derivative1, derivative2;
  int ret;
  int result = EXIT_SUCCESS;

  metric->SetMaximumNumberOfThreads(1);
  std::cerr << "Setting number of metric threads to " << metric->GetMaximumNumberOfThreads() << std::endl;
  ret = itkCorrelationImageToImageMetricv4Test_WithSpecifiedThreads(metric, value1, derivative1);
  if( ret == EXIT_FAILURE )
    {
    result = EXIT_FAILURE;
    }

  metric->SetMaximumNumberOfThreads(8);
  std::cerr << "Setting number of metric threads to " << metric->GetMaximumNumberOfThreads() << std::endl;
  ret = itkCorrelationImageToImageMetricv4Test_WithSpecifiedThreads(metric, value2, derivative2);
  if( ret == EXIT_FAILURE )
    {
    result = EXIT_FAILURE;
    }

  double myeps = 1e-8;
  if (itk::Math::abs(value1 - value2) > 1e-8)
    {
    std::cerr << "value1: " << value1 << std::endl;
    std::cerr << "value2: " << value2 << std::endl;
    std::cerr << "Got different metric values when set threading number differently." << std::endl;
    result = EXIT_FAILURE;
    }

  vnl_vector<double> ddiff = (vnl_vector<double>) derivative1 - (vnl_vector<double>) derivative2;
  if (ddiff.two_norm() > myeps)
    {
    std::cerr << "derivative1: " << derivative1 << std::endl;
    std::cerr << "derivative2: " << derivative2 << std::endl;
    std::cerr << "Got different derivative values when set threading number differently." << std::endl;
    result = EXIT_FAILURE;
    }

  // Test that non-overlapping images will generate a warning
  // and return max value for metric value.
  MovingTransformType::ParametersType parameters( imageDimensionality );
  parameters.Fill( static_cast<MovingTransformType::ParametersValueType>(1000) );
  movingTransform->SetParameters( parameters );
  MetricType::MeasureType expectedMetricMax, valueReturn;
  MetricType::DerivativeType derivativeReturn;
  expectedMetricMax = itk::NumericTraits<MetricType::MeasureType>::max();
  std::cout << "Testing non-overlapping images. Expect a warning:" << std::endl;
  metric->GetValueAndDerivative( valueReturn, derivativeReturn );
  if( metric->GetNumberOfValidPoints() != 0 || itk::Math::NotExactlyEquals(valueReturn, expectedMetricMax) )
    {
    std::cerr << "Failed testing for non-overlapping images. " << std::endl
              << "  Number of valid points: " << metric->GetNumberOfValidPoints() << std::endl
              << "  Metric value: " << valueReturn << std::endl
              << "  Expected metric max value: " << expectedMetricMax << std::endl;
    }

  return result;
}
