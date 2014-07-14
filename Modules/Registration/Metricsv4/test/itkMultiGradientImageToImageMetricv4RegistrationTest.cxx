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
 * Test program for itkMultiStartImageToImageMetricv4RegistrationTest and
 * GradientDescentOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 */
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkMultiGradientOptimizerv4.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkCastImageFilter.h"

#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iomanip>

int itkMultiGradientImageToImageMetricv4RegistrationTest(int argc, char *argv[])
{

  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [numberOfIterations initialAffine ] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << argc << std::endl;
  unsigned int numberOfIterations = 10;
  if( argc >= 5 )
    {
    numberOfIterations = atoi( argv[4] );
    }
  std::cout << " iterations "<< numberOfIterations << std::endl;

  const unsigned int Dimension = 2;
  typedef double PixelType; //I assume png is unsigned short

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

  /** define a resample filter that will ultimately be used to deform the image */
  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();


  /** create a composite transform holder for other transforms  */
  typedef itk::CompositeTransform<double, Dimension>    CompositeType;

  CompositeType::Pointer compositeTransform = CompositeType::New();

  //create an affine transform
  typedef itk::AffineTransform<double, Dimension>
                                                    AffineTransformType;
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout <<" affineTransform params prior to optimization " << affineTransform->GetParameters() << std::endl;

  //identity transform for fixed image
  typedef itk::IdentityTransform<double, Dimension> IdentityTransformType;
  IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  // The metric
  typedef itk::MattesMutualInformationImageToImageMetricv4 < FixedImageType, MovingImageType >  MetricType;
  typedef itk::MeanSquaresImageToImageMetricv4 < FixedImageType, MovingImageType >  MetricType2;
  typedef MetricType::FixedSampledPointSetType                                                              PointSetType;
  MetricType::Pointer metric = MetricType::New();
  metric->SetNumberOfHistogramBins(20);
  MetricType2::Pointer metric2 = MetricType2::New();

  if( 0 )
    {
    std::cout << "Dense sampling." << std::endl;
    metric->SetUseFixedSampledPointSet( false );
    }
  else
    {
    typedef PointSetType::PointType     PointType;
    PointSetType::Pointer               pset(PointSetType::New());
    unsigned long ind=0,ct=0;
    itk::ImageRegionIteratorWithIndex<FixedImageType> It(fixedImage, fixedImage->GetLargestPossibleRegion() );
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      // take every N^th point
      if ( ct % 20 == 0  )
        {
          PointType pt;
          fixedImage->TransformIndexToPhysicalPoint( It.GetIndex(), pt);
          pset->SetPoint(ind, pt);
          ind++;
        }
        ct++;
      }
    metric->SetFixedSampledPointSet( pset );
    metric->SetUseFixedSampledPointSet( true );
    metric2->SetFixedSampledPointSet( pset );
    metric2->SetUseFixedSampledPointSet( true );
    std::cout << "Testing metric with point set..." << std::endl;
    }

  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( identityTransform );
  metric->SetMovingTransform( affineTransform );
  metric->SetUseMovingImageGradientFilter( false );
  metric->SetUseFixedImageGradientFilter( false );
  metric->Initialize();

  metric2->SetFixedImage( fixedImage );
  metric2->SetMovingImage( movingImage );
  metric2->SetFixedTransform( identityTransform );
  metric2->SetMovingTransform( affineTransform );
  metric2->SetUseMovingImageGradientFilter( false );
  metric2->SetUseFixedImageGradientFilter( false );
  metric2->Initialize();

  std::cout << "First do an affine registration " << std::endl;
  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType > RegistrationParameterScalesFromShiftType;
  RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  RegistrationParameterScalesFromShiftType::ScalesType scales(affineTransform->GetNumberOfParameters());
  shiftScaleEstimator->SetMetric(metric);
  shiftScaleEstimator->EstimateScales(scales);
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  OptimizerType::Pointer  optimizer = OptimizerType::New();
  optimizer->SetMetric( metric );
  optimizer->SetScales(scales);
  /** Set just 1 iteration for the sub-optimizer */
  optimizer->SetNumberOfIterations( 1 );
  optimizer->SetScalesEstimator( shiftScaleEstimator );
  optimizer->SetMaximumStepSizeInPhysicalUnits( 0.5 );

  std::cout << "now declare optimizer2  " << std::endl;

  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType2 > RegistrationParameterScalesFromShiftType2;
  RegistrationParameterScalesFromShiftType2::Pointer shiftScaleEstimator2 = RegistrationParameterScalesFromShiftType2::New();
  shiftScaleEstimator2->SetMetric(metric2);
  shiftScaleEstimator2->EstimateScales(scales);
  OptimizerType::Pointer  optimizer2 = OptimizerType::New();
  optimizer2->SetMetric( metric2 );
  optimizer2->SetScales(scales);
  /** Set just 1 iteration for the sub-optimizer */
  optimizer2->SetNumberOfIterations( 1 );
  optimizer2->SetScalesEstimator( shiftScaleEstimator2 );
  optimizer2->SetMaximumStepSizeInPhysicalUnits( 0.5 );

  std::cout << "now declare optimizer to combine the 2 sub-optimizers  " << std::endl;
  typedef  itk::MultiGradientOptimizerv4  MOptimizerType;
  MOptimizerType::Pointer  MOptimizer = MOptimizerType::New();
  MOptimizer->GetOptimizersList().push_back(optimizer);
  MOptimizer->GetOptimizersList().push_back(optimizer2);
  std::cout << "set the # of iterations " << std::endl;
  MOptimizer->SetNumberOfIterations( numberOfIterations );
  std::cout << "begin optimization " << std::endl;
  MOptimizer->StartOptimization();

  //warp the image with the displacement field
  resample->SetTransform( affineTransform );
  resample->SetInput( movingImageReader->GetOutput() );
  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 0 );
  resample->Update();
  std::cout << "GetNumberOfThreadsUsed: " << metric->GetNumberOfThreadsUsed() << std::endl;

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

  std::cout << "After optimization affine params are: " <<  affineTransform->GetParameters() << std::endl;
  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;

}
