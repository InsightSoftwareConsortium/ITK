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
 * Test program for MeanSquaresImageToImageMetricv4 and
 * LBFGSOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 * A regression test is performed using ctest.
 */

#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkLBFGSOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

#include "itkCastImageFilter.h"

#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iomanip>

int itkMeanSquaresImageToImageMetricv4RegistrationTest2(int argc, char *argv[])
{

  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [gradientTolerance=1e-4] [max function iterations=100] [lineSearchTol=0.9] [stepLength=1.0] [trace-debug=false]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  double gTolerance       = 1e-4;  // Gradient magnitude tolerance
  int    maxIterations    = 100;   // Maximum number of iterations
  double lineSearchTol    = 0.9;   // Line search tolerance
  double stepLength       = 1.0;   // Default step length
  bool   trace            = false; // Tracing

  if( argc > 4 )
    {
    gTolerance = atof( argv[4] );
    }
  if( argc > 5 )
    {
    maxIterations = atoi( argv[5] );
    }
  if( argc > 6 )
    {
    lineSearchTol = atof( argv[6] );
    }
  if( argc > 7 )
    {
    stepLength = atof( argv[7] );
    }
  if( argc > 8 )
    {
    trace = static_cast<bool>( atoi( argv[8] ) );
    }

  std::cout << argc << std::endl;
  std::cout << "gTolerance: " << gTolerance << " maxIterations: " << maxIterations << " lineSearchTol: " << lineSearchTol << " stepLength: " << stepLength << " trace: " << trace << std::endl;

  /** load the images **/

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

  fixedImageReader->Update();
  FixedImageType::Pointer  fixedImage = fixedImageReader->GetOutput();
  movingImageReader->Update();
  MovingImageType::Pointer movingImage = movingImageReader->GetOutput();

  /** define a resample filter that will ultimately be used to deform the image */
  typedef itk::ResampleImageFilter< MovingImageType, FixedImageType >    ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  /** create a composite transform holder for other transforms  */
  typedef itk::CompositeTransform<double, Dimension>    CompositeType;
  CompositeType::Pointer compositeTransform = CompositeType::New();

  // create an affine transform
  typedef itk::AffineTransform<double, Dimension>
                                                    AffineTransformType;
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout <<" affineTransform params prior to optimization " << affineTransform->GetParameters() << std::endl;

  // identity transform for fixed image
  typedef itk::IdentityTransform<double, Dimension> IdentityTransformType;
  IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  // the metric
  typedef itk::MeanSquaresImageToImageMetricv4 < FixedImageType, MovingImageType >  MetricType;
  typedef MetricType::FixedSampledPointSetType                                      PointSetType;
  MetricType::Pointer metric = MetricType::New();

  typedef PointSetType::PointType     PointType;
  PointSetType::Pointer               pset(PointSetType::New());
  unsigned long ind=0,ct=0;
  itk::ImageRegionIteratorWithIndex<FixedImageType> it(fixedImage, fixedImage->GetLargestPossibleRegion() );

  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    // take every N^th point
    if ( 1 /*ct % 4 == 0*/  )
      {
        PointType pt;
        fixedImage->TransformIndexToPhysicalPoint( it.GetIndex(), pt);
        pset->SetPoint(ind, pt);
        ind++;
      }
      ct++;
    }
  std::cout << "Setting point set with " << ind << " points of " << fixedImage->GetLargestPossibleRegion().GetNumberOfPixels() << " total " << std::endl;
  metric->SetFixedSampledPointSet( pset );
  metric->SetUseFixedSampledPointSet( true );
  std::cout << "Testing metric with point set..." << std::endl;


  // Assign images and transforms.
  // By not setting a virtual domain image or virtual domain settings,
  // the metric will use the fixed image for the virtual domain.
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( identityTransform );
  metric->SetMovingTransform( affineTransform );
  const bool gaussian = false;
  metric->SetUseMovingImageGradientFilter( gaussian );
  metric->SetUseFixedImageGradientFilter( gaussian );
  metric->Initialize();

  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType > RegistrationParameterScalesFromShiftType;
  RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  std::cout << "Do an affine registration: " << std::endl;

  // optimizer
  typedef itk::LBFGSOptimizerv4  OptimizerType;
  OptimizerType::Pointer  optimizer = OptimizerType::New();
  optimizer->SetMetric( metric );
  optimizer->SetScalesEstimator( shiftScaleEstimator );

  optimizer->SetTrace( trace );
  optimizer->SetMaximumNumberOfFunctionEvaluations( maxIterations );
  optimizer->SetGradientConvergenceTolerance( gTolerance );
  optimizer->SetLineSearchAccuracy( lineSearchTol );
  optimizer->SetDefaultStepLength( stepLength );
  std::cout << "Initial stop description   = " << optimizer->GetStopConditionDescription() << std::endl;

  // optimize
  try
    {
    optimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << "Exception thrown ! " << std::endl;
    std::cerr << "An error occurred during deformation Optimization:" << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.what()    << std::endl;
    std::cerr << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Number of threads: metric: " << metric->GetNumberOfThreadsUsed() << " optimizer: " << optimizer->GetNumberOfThreads() << std::endl;
  std::cout << "Scales: " << optimizer->GetScales() << " DoEstimateScales: " << optimizer->GetDoEstimateScales() << std::endl;
  std::cout << "GetNumberOfSkippedFixedSampledPoints: " << metric->GetNumberOfSkippedFixedSampledPoints() << std::endl;

  //warp the image with the transform
  resample->SetTransform( affineTransform );
  resample->SetInput( movingImageReader->GetOutput() );
  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 0 );
  resample->Update();

  //write the warped image into a file
  typedef double                                                    OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension >                  OutputImageType;
  typedef itk::CastImageFilter< MovingImageType, OutputImageType >  CastFilterType;
  typedef itk::ImageFileWriter< OutputImageType >                   WriterType;

  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();
  writer->SetFileName( argv[3] );
  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput() );
  writer->Update();

  std::cout << "After optimization affine params are: " <<  affineTransform->GetParameters() << std::endl;
  return EXIT_SUCCESS;

}
