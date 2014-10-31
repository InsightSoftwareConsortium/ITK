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
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkCorrelationImageToImageMetricv4.h"
#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImage.h"
#include "itkGaussianImageSource.h"
#include "itkCyclicShiftImageFilter.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkImageRegionIteratorWithIndex.h"

/* This test performs a simple registration test on each
 * ImageToImageMetricv4 metric, testing that:
 *  1) metric value is minimized
 *  2) final optimization position is correct within tolerance
 *  3) different options for sampling and image gradient calculation work
 * New metrics must be added manually to this test.
 */

template<unsigned int Dimension, typename TImage, typename TMetric>
int ImageToImageMetricv4RegistrationTestRun( typename TMetric::Pointer metric, int numberOfIterations, typename TImage::PixelType maximumStepSize, bool doSampling, bool doGradientFilter )
{
  typedef typename TImage::PixelType  PixelType;
  typedef PixelType                   CoordinateRepresentationType;

  // Create two simple images
  itk::SizeValueType ImageSize = 100;
  itk::OffsetValueType boundary = 6;
  if( Dimension == 3 )
    {
    ImageSize = 60;
    boundary = 4;
    }

   // Declare Gaussian Sources
  typedef itk::GaussianImageSource< TImage >        GaussianImageSourceType;

  typename TImage::SizeType size;
  size.Fill( ImageSize );

  typename TImage::SpacingType spacing;
  spacing.Fill( itk::NumericTraits<CoordinateRepresentationType>::OneValue() );

  typename TImage::PointType origin;
  origin.Fill( itk::NumericTraits<CoordinateRepresentationType>::ZeroValue() );

  typename TImage::DirectionType direction;
  direction.Fill( itk::NumericTraits<CoordinateRepresentationType>::OneValue() );

  typename GaussianImageSourceType::Pointer  fixedImageSource = GaussianImageSourceType::New();

  fixedImageSource->SetSize(    size    );
  fixedImageSource->SetOrigin(  origin  );
  fixedImageSource->SetSpacing( spacing );
  fixedImageSource->SetNormalized( false );
  fixedImageSource->SetScale( 1.0f );
  fixedImageSource->Update();
  typename TImage::Pointer  fixedImage  = fixedImageSource->GetOutput();

  // zero-out the boundary
  itk::ImageRegionIteratorWithIndex<TImage> it( fixedImage, fixedImage->GetLargestPossibleRegion() );
  for( it.GoToBegin(); ! it.IsAtEnd(); ++it )
    {
    for( itk::SizeValueType n=0; n < Dimension; n++ )
      {
      if( it.GetIndex()[n] < boundary || (static_cast<itk::OffsetValueType>(size[n]) - it.GetIndex()[n]) <= boundary )
        {
        it.Set( itk::NumericTraits<PixelType>::ZeroValue() );
        break;
        }
      }
    }

  // shift the fixed image to get the moving image
  typedef itk::CyclicShiftImageFilter<TImage, TImage> CyclicShiftFilterType;
  typename CyclicShiftFilterType::Pointer shiftFilter = CyclicShiftFilterType::New();
  typename CyclicShiftFilterType::OffsetType imageShift;
  typename CyclicShiftFilterType::OffsetValueType maxImageShift = boundary-1;
  imageShift.Fill( maxImageShift );
  imageShift[0] = maxImageShift / 2;
  shiftFilter->SetInput( fixedImage );
  shiftFilter->SetShift( imageShift );
  shiftFilter->Update();
  typename TImage::Pointer movingImage = shiftFilter->GetOutput();

  // create an affine transform
  typedef itk::TranslationTransform<double, Dimension> TranslationTransformType;
  typename TranslationTransformType::Pointer translationTransform = TranslationTransformType::New();
  translationTransform->SetIdentity();

  // setup metric
  //
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetMovingTransform( translationTransform );
  metric->SetUseMovingImageGradientFilter( doGradientFilter );
  metric->SetUseFixedImageGradientFilter( doGradientFilter );
  std::cout << "Use image gradient filter: " << doGradientFilter << std::endl;

  // sampling
  if( ! doSampling )
    {
    std::cout << "Dense sampling." << std::endl;
    metric->SetUseFixedSampledPointSet( false );
    }
  else
    {
    typedef typename TMetric::FixedSampledPointSetType PointSetType;
    typedef typename PointSetType::PointType           PointType;
    typename PointSetType::Pointer                     pset(PointSetType::New());
    itk::SizeValueType ind=0,ct=0;
    itk::ImageRegionIteratorWithIndex<TImage> itS(fixedImage, fixedImage->GetLargestPossibleRegion() );
    for( itS.GoToBegin(); !itS.IsAtEnd(); ++itS )
      {
      // take every N^th point
      // not sampling sparsely in order to get all metrics to pass
      // with similar settings
      if ( ct % 2 == 0 )
        {
          PointType pt;
          fixedImage->TransformIndexToPhysicalPoint( itS.GetIndex(), pt);
          pset->SetPoint(ind, pt);
          ind++;
        }
        ct++;
      }
    std::cout << "Setting point set with " << ind << " points of "
              << fixedImage->GetLargestPossibleRegion().GetNumberOfPixels() << " total " << std::endl;
    metric->SetFixedSampledPointSet( pset );
    metric->SetUseFixedSampledPointSet( true );
    std::cout << "Testing metric with point set..." << std::endl;
    }

  // initialize
  metric->Initialize();

  // calculate initial metric value
  typename TMetric::MeasureType initialValue = metric->GetValue();

  // scales estimator
  typedef itk::RegistrationParameterScalesFromPhysicalShift< TMetric > RegistrationParameterScalesFromPhysicalShiftType;
  typename RegistrationParameterScalesFromPhysicalShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromPhysicalShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  //
  // optimizer
  //
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  typename OptimizerType::Pointer  optimizer = OptimizerType::New();
  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetScalesEstimator( shiftScaleEstimator );
  if( maximumStepSize > 0 )
    {
    optimizer->SetMaximumStepSizeInPhysicalUnits( maximumStepSize );
    }
  optimizer->StartOptimization();

  std::cout << "image size: " << size;
  std::cout << ", # of iterations: " << optimizer->GetNumberOfIterations() << ", max step size: "
            << optimizer->GetMaximumStepSizeInPhysicalUnits() << std::endl;
  std::cout << "imageShift: " << imageShift << std::endl;
  std::cout << "Transform final parameters: " << translationTransform->GetParameters() << std::endl;

  // final metric value
  typename TMetric::MeasureType finalValue = metric->GetValue();
  std::cout << "metric value: initial: " << initialValue << ", final: " << finalValue << std::endl;

  // test that the final position is close to the truth
  double tolerance = static_cast<double>(0.11);
  for( itk::SizeValueType n=0; n < Dimension; n++ )
    {
    if( std::fabs( 1.0 - ( static_cast<double>(imageShift[n]) / translationTransform->GetParameters()[n] ) ) > tolerance )
      {
      std::cerr << "XXX Failed. Final transform parameters are not within tolerance of image shift. XXX" << std::endl;
      return EXIT_FAILURE;
      }
    }
  // test that metric value is minimized
  if( finalValue >= initialValue )
    {
    std::cerr << "XXX Failed. Final metric value is not less than initial value. XXX" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////
template<unsigned int Dimension>
int itkImageToImageMetricv4RegistrationTestRunAll (int argc, char *argv[])
{
  typedef itk::Image< double, Dimension > ImageType;

  // options
  // we have two options for iterations and step size to accomodate
  // the different behavior of metrics
  int numberOfIterations1 = 50;
  typename ImageType::PixelType maximumStepSize1 = 1.0;
  int numberOfIterations2 = 120;
  typename ImageType::PixelType maximumStepSize2 = 0.1;
  bool doSampling = false;
  bool doGradientFilter = false;

  if( argc > 1 )
    {
    numberOfIterations1 = atoi( argv[1] );
    }
  if( argc > 2 )
    {
    maximumStepSize1 = atof( argv[2] );
    }
  if( argc > 3 )
    {
    numberOfIterations2 = atoi( argv[3] );
    }
  if( argc > 4 )
    {
    maximumStepSize2 = atof( argv[4] );
    }
  if( argc > 5 )
    {
    doSampling = atoi( argv[5] );
    }
  if( argc > 6 )
    {
    doGradientFilter = atoi( argv[6] );
    }

  std::cout << std::endl << "******************* Dimension: " << Dimension << std::endl;

  bool passed = true;

  // ANTS Neighborhood Correlation
  // This metric does not support sampling
  if( !doSampling )
  {
  typedef itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<ImageType, ImageType> MetricType;
  typename MetricType::Pointer metric = MetricType::New();
  std::cout << std::endl << "*** ANTSNeighborhoodCorrelation metric: " << std::endl;
  if( ImageToImageMetricv4RegistrationTestRun<Dimension, ImageType, MetricType>( metric, numberOfIterations1, maximumStepSize1, doSampling, doGradientFilter ) != EXIT_SUCCESS )
    {
    passed = false;
    }
  }

  // Correlation
  {
  typedef itk::CorrelationImageToImageMetricv4<ImageType, ImageType> MetricType;
  typename MetricType::Pointer metric = MetricType::New();
  std::cout << std::endl << "*** Correlation metric: " << std::endl;
  if( ImageToImageMetricv4RegistrationTestRun<Dimension, ImageType, MetricType>( metric, numberOfIterations1, maximumStepSize1, doSampling, doGradientFilter ) != EXIT_SUCCESS )
    {
    passed = false;
    }
  }

  // Joint Histogram
  {
  typedef itk::JointHistogramMutualInformationImageToImageMetricv4<ImageType, ImageType> MetricType;
  typename MetricType::Pointer metric = MetricType::New();
  std::cout << std::endl << "*** JointHistogramMutualInformation metric: " << std::endl;
  if( ImageToImageMetricv4RegistrationTestRun<Dimension, ImageType, MetricType>( metric, numberOfIterations1, maximumStepSize1, doSampling, doGradientFilter ) != EXIT_SUCCESS )
    {
    passed = false;
    }
  }

  // Mattes
  {
  typedef itk::MattesMutualInformationImageToImageMetricv4<ImageType, ImageType> MetricType;
  typename MetricType::Pointer metric = MetricType::New();
  std::cout << std::endl << "*** MattesMutualInformation metric: " << std::endl;
  if( ImageToImageMetricv4RegistrationTestRun<Dimension, ImageType, MetricType>( metric, numberOfIterations2, maximumStepSize2, doSampling, doGradientFilter ) != EXIT_SUCCESS )
    {
    passed = false;
    }
  }

  // MeanSquares
  {
  typedef itk::MeanSquaresImageToImageMetricv4<ImageType, ImageType> MetricType;
  typename MetricType::Pointer metric = MetricType::New();
  std::cout << std::endl << "*** MeanSquares metric: " << std::endl;
  if( ImageToImageMetricv4RegistrationTestRun<Dimension, ImageType, MetricType>( metric, numberOfIterations1, maximumStepSize1, doSampling, doGradientFilter ) != EXIT_SUCCESS )
    {
    passed = false;
    }
  }

  if( passed )
    {
    return EXIT_SUCCESS;
    }
  else
    {
    return EXIT_FAILURE;
    }
}

//////////////////////////////////////////////////////////////
int itkImageToImageMetricv4RegistrationTest (int argc, char *argv[])
{
  int result = EXIT_SUCCESS;

  if( itkImageToImageMetricv4RegistrationTestRunAll<2>(argc, argv) != EXIT_SUCCESS )
    {
    std::cerr << "Failed for one or more metrics. See error message(s) above." << std::endl;
    result = EXIT_FAILURE;
    }

  return result;
}
