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

/**
 * Test program for DemonsImageToImageMetricv4 and
 * GradientDescentOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 */
#include "itkDemonsImageToImageMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromShift.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"

#include "itkIdentityTransform.h"
#include "itkAffineTransform.h"

#include "itkCastImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itksys/SystemTools.hxx"
#include "itkResampleImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkMultiThreader.h"

/* Helper method to shrink images for faster testing as needed. */
template<class TImageType>
void AffineDemonsImageToImageRegistrationTestShrink( typename TImageType::Pointer & image, unsigned int shrinkFactor)
{
  typedef itk::ShrinkImageFilter<TImageType,TImageType> ShrinkFilterType;

  typename ShrinkFilterType::Pointer filter = ShrinkFilterType::New();

  filter->SetInput( image );
  filter->SetShrinkFactors( shrinkFactor );
  filter->Update();
  image = filter->GetOutput();
}

int itkAffineDemonsImageToImageRegistrationTest(int argc, char *argv[])
{

  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [numberOfIterations] ";
    std::cerr << " [sparseSamplingStep = 1 (== dense sampling)] ";
    std::cerr << " [imageShrinkFactor = 1 ] ";
    std::cerr << " [numberOfThreads = global default ] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << argc << std::endl;
  unsigned int numberOfIterations = 10;
  unsigned int sparseSamplingStep = 1; //dense sampling
  unsigned int imageShrinkFactor = 1;
  itk::ThreadIdType numberOfThreads =
    itk::MultiThreader::GetGlobalDefaultNumberOfThreads();
  if( argc >= 5 )
    {
    numberOfIterations = atoi( argv[4] );
    }
  if( argc >= 6 )
    {
    sparseSamplingStep = atoi( argv[5] );
    }
  if( argc >= 7 )
    {
    imageShrinkFactor = atoi( argv[6] );
    }
  if( argc >= 8 )
    {
    numberOfThreads = atoi( argv[7] );
    }

  bool useSparseSampling = sparseSamplingStep > 1;
  if( sparseSamplingStep < 1 )
    {
    std::cerr << "sparseSamplingStep must be >=1. It is " << sparseSamplingStep
              << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << " affine iterations: "<< numberOfIterations
    << " sparseSamplingStep: " << sparseSamplingStep
    << " useSparseSampling: " << useSparseSampling << std::endl
    << " imageShrinkFactor: " << imageShrinkFactor << std::endl
    << " numberOfThreads: " << numberOfThreads << std::endl;

  const unsigned int Dimension = 2;
  typedef double PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer fixedImageReader   = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName( argv[1] );
  movingImageReader->SetFileName( argv[2] );

  //get the images
  fixedImageReader->Update();
  FixedImageType::Pointer  fixedImage = fixedImageReader->GetOutput();
  movingImageReader->Update();
  MovingImageType::Pointer movingImage = movingImageReader->GetOutput();

  // resize the images for faster computation during default tests
  AffineDemonsImageToImageRegistrationTestShrink<
    FixedImageType>( fixedImage, imageShrinkFactor );
  AffineDemonsImageToImageRegistrationTestShrink<
    MovingImageType>( movingImage, imageShrinkFactor );

  /** define a resample filter that will ultimately be used to deform the image */
  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();


  //create an affine transform
  typedef itk::AffineTransform<double, Dimension>
                                                    AffineTransformType;
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout <<" affineTransform initial params "
            << affineTransform->GetParameters() << std::endl;

  //identity transform for fixed image
  typedef itk::IdentityTransform<double, Dimension> IdentityTransformType;
  IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  // The metric
  typedef itk::DemonsImageToImageMetricv4< FixedImageType, MovingImageType >
                                                                  MetricType;
  typedef MetricType::FixedSampledPointSetType                    PointSetType;
  MetricType::Pointer metric = MetricType::New();
  metric->SetMaximumNumberOfThreads( numberOfThreads );

  if( useSparseSampling )
    {
    std::cout << "Affine: testing metric with sparse point set..." << std::endl;

    // Create a point set for sparse sampling
    typedef PointSetType::PointType     PointType;
    PointSetType::Pointer               pset(PointSetType::New());
    unsigned long ind=0,ct=0;
    itk::ImageRegionIteratorWithIndex<FixedImageType>
      It(fixedImage, fixedImage->GetLargestPossibleRegion() );
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      // take every N^th point
      if ( ct % sparseSamplingStep == 0  )
        {
          PointType pt;
          fixedImage->TransformIndexToPhysicalPoint( It.GetIndex(), pt);
          pset->SetPoint(ind, pt);
          ind++;
        }
        ct++;
      }
    std::cout << "Setting point set with " << ind << " points of "
              << fixedImage->GetLargestPossibleRegion().GetNumberOfPixels()
              << " total " << std::endl;
    metric->SetFixedSampledPointSet( pset );
    }
  else
    {
    std::cout << "Affine: testing metric with dense sampling..." << std::endl;
    }
  metric->SetUseFixedSampledPointSet( useSparseSampling );

  // Assign images and transforms.
  // By not setting a virtual domain image or virtual domain settings,
  // the metric will use the fixed image for the virtual domain.
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( identityTransform );
  metric->SetMovingTransform( affineTransform );
  metric->SetDoFixedImagePreWarp( ! useSparseSampling );
  metric->SetDoMovingImagePreWarp( ! useSparseSampling );
  bool gaussian = false;
  metric->SetUseMovingImageGradientFilter( gaussian );
  metric->SetUseFixedImageGradientFilter( gaussian );
  metric->Initialize();

  // Create a scales/learning-rate estimator for the optimizer
  typedef itk::RegistrationParameterScalesFromShift< MetricType >
    RegistrationParameterScalesFromShiftType;
  RegistrationParameterScalesFromShiftType::Pointer
    shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  // Create an optimzer and initialize
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  OptimizerType::Pointer  optimizer = OptimizerType::New();
  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetScalesEstimator( shiftScaleEstimator );
  optimizer->SetNumberOfThreads( numberOfThreads );

  std::cout << "Start affine registration... " << std::endl;
  try
    {
    optimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error ocurred during optimization:" << std::endl;
    std::cout << e.GetLocation() << std::endl;
    std::cout << e.GetDescription() << std::endl;
    std::cout << e.what()    << std::endl;
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "Finished. Scales: " << optimizer->GetScales() << std::endl;

  //warp the image with the affine transform
  resample->SetTransform( affineTransform );
  resample->SetInput( movingImageReader->GetOutput() );
  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 0 );
  resample->Update();

  //write the warped image into a file
  typedef double                                    OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension >  OutputImageType;
  typedef itk::CastImageFilter<
                        MovingImageType,
                        OutputImageType >           CastFilterType;
  typedef itk::ImageFileWriter< OutputImageType >   WriterType;
  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();
  writer->SetFileName( argv[3] );
  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput() );
  writer->Update();

  std::cout << "Test PASSED." << affineTransform->GetParameters() << std::endl;
  return EXIT_SUCCESS;

}
