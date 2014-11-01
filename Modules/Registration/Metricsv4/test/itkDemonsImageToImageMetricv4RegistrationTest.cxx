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
 * Test program for itkDemonsImageToImageMetricv4 and
 * GradientDescentOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 */
#include "itkDemonsImageToImageMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iomanip>

#include "itkCommand.h"

#include <iostream>
#include <fstream>

template<typename TFilter>
class itkDemonsImageToImageMetricv4RegistrationTestCommandIterationUpdate : public itk::Command
{
public:
  typedef itkDemonsImageToImageMetricv4RegistrationTestCommandIterationUpdate   Self;

  typedef itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  itkDemonsImageToImageMetricv4RegistrationTestCommandIterationUpdate() {};

public:

  virtual void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *) caller, event);
    }

  virtual void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }
    const TFilter *optimizer = static_cast< const TFilter * >( object );

    std::cout << "It: " << optimizer->GetCurrentIteration() << " metric value: " << optimizer->GetValue();
    std::cout << std::endl;
    }
};

int itkDemonsImageToImageMetricv4RegistrationTest(int argc, char *argv[])
{

  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [numberOfIterations = 10] ";
    std::cerr << " [doSampling = false] ";
    std::cerr << " [useImageGradientFilter = false]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << argc << std::endl;
  unsigned int numberOfIterations = 10;
  bool doSampling = false;
  bool useImageGradientFilter = false;
  if( argc >= 5 )
    {
    numberOfIterations = atoi( argv[4] );
    }
  if( argc >= 6 )
    {
    doSampling = atoi( argv[5] );
    }
  if( argc >= 7 )
    {
    useImageGradientFilter = atoi( argv[6] );
    }

  std::cout << " iterations "<< numberOfIterations << std::endl;
  std::cout << " useImageGradientFilter " << useImageGradientFilter << std::endl;

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

  // scale the images to [0,1]
  typedef itk::RescaleIntensityImageFilter<FixedImageType, FixedImageType> FixedRescaleFilterType;
  FixedRescaleFilterType::Pointer fixedRescaleFilter = FixedRescaleFilterType::New();
  fixedRescaleFilter->SetInput( fixedImage );
  fixedRescaleFilter->SetOutputMinimum( itk::NumericTraits<PixelType>::ZeroValue() );
  fixedRescaleFilter->SetOutputMaximum( itk::NumericTraits<PixelType>::OneValue() );
  fixedRescaleFilter->Update();
  fixedImage = fixedRescaleFilter->GetOutput();

  typedef itk::RescaleIntensityImageFilter<MovingImageType, MovingImageType> MovingRescaleFilterType;
  MovingRescaleFilterType::Pointer movingRescaleFilter = MovingRescaleFilterType::New();
  movingRescaleFilter->SetInput( movingImage );
  movingRescaleFilter->SetOutputMinimum( itk::NumericTraits<PixelType>::ZeroValue() );
  movingRescaleFilter->SetOutputMaximum( itk::NumericTraits<PixelType>::OneValue() );
  movingRescaleFilter->Update();
  movingImage = movingRescaleFilter->GetOutput();

  // histogram matching of values
  typedef itk::HistogramMatchingImageFilter<FixedImageType, MovingImageType> MatchingFilterType;
  MatchingFilterType::Pointer matchingFilter = MatchingFilterType::New();
  matchingFilter->SetInput( movingImage );
  matchingFilter->SetReferenceImage( fixedImage );
  matchingFilter->ThresholdAtMeanIntensityOn();
  matchingFilter->SetNumberOfHistogramLevels( 256 ); //from ANTS
  matchingFilter->SetNumberOfMatchPoints( 12 ); //from ANTS
  matchingFilter->Update();
  movingImage = matchingFilter->GetOutput();

  /** Displacement field transform */
  typedef itk::GaussianSmoothingOnUpdateDisplacementFieldTransform< double, Dimension> DisplacementTransformType;
  DisplacementTransformType::Pointer displacementTransform = DisplacementTransformType::New();

  typedef DisplacementTransformType::DisplacementFieldType DisplacementFieldType;
  DisplacementFieldType::Pointer field = DisplacementFieldType::New();

  // set the field to be the same as the fixed image region, which will
  // act by default as the virtual domain in this example.
  field->SetRegions( fixedImage->GetLargestPossibleRegion() );
  //make sure the field has the same spatial information as the image
  field->CopyInformation( fixedImage );
  std::cout << "fixedImage->GetLargestPossibleRegion(): "
            << fixedImage->GetLargestPossibleRegion() << std::endl;
  field->Allocate();
  // Fill it with 0's
  DisplacementTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 0 );
  field->FillBuffer( zeroVector );
  // Assign to transform
  displacementTransform->SetDisplacementField( field );
  displacementTransform->SetGaussianSmoothingVarianceForTheUpdateField( 5 );
  displacementTransform->SetGaussianSmoothingVarianceForTheTotalField( 6 );

  // The metric
  typedef itk::DemonsImageToImageMetricv4 < FixedImageType, MovingImageType >  MetricType;
  typedef MetricType::FixedSampledPointSetType                                 PointSetType;
  MetricType::Pointer metric = MetricType::New();

  // Assign images and transforms.
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetMovingTransform( displacementTransform );
  metric->SetUseMovingImageGradientFilter( useImageGradientFilter );
  metric->SetUseFixedImageGradientFilter( useImageGradientFilter );

  // Sampling
  if( ! doSampling )
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
      if ( ct % 10 == 0  )
        {
          PointType pt;
          fixedImage->TransformIndexToPhysicalPoint( It.GetIndex(), pt);
          pset->SetPoint(ind, pt);
          ind++;
        }
        ct++;
      }
    std::cout << "Setting point set with " << ind << " points of " << fixedImage->GetLargestPossibleRegion().GetNumberOfPixels() << " total " << std::endl;
    metric->SetFixedSampledPointSet( pset );
    metric->SetUseFixedSampledPointSet( true );
    std::cout << "Testing metric with point set..." << std::endl;
    }

  // Initialize
  metric->Initialize();

  // scales & step estimator
  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType > RegistrationParameterScalesFromShiftType;
  RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  // Optimizer
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  OptimizerType::Pointer  optimizer = OptimizerType::New();
  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetScalesEstimator( shiftScaleEstimator );

  try
    {
    optimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error ocurred during deformation Optimization:" << std::endl;
    std::cout << e.GetLocation() << std::endl;
    std::cout << e.GetDescription() << std::endl;
    std::cout << e.what()    << std::endl;
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "...finished. " << std::endl;

  if( doSampling )
    {
    std::cout << "GetNumberOfSkippedFixedSampledPoints: " << metric->GetNumberOfSkippedFixedSampledPoints() << std::endl;
    }

  //std::cout << "\n\n*gradient: " << optimizer->GetGradient() << std::endl;
  std::cout << "Scales: " << optimizer->GetScales() << std::endl;
  std::cout << "Final learning rate: " << optimizer->GetLearningRate() << std::endl;
  std::cout << "MaxStepSizeinPhysUnits: " << optimizer->GetMaximumStepSizeInPhysicalUnits() << std::endl;

  //warp the image with the displacement field
  typedef itk::ResampleImageFilter< MovingImageType, FixedImageType >    ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( displacementTransform );
  resample->SetInput( movingImageReader->GetOutput() );
  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 0 );
  resample->Update();

  //write out the displacement field
  typedef itk::ImageFileWriter< DisplacementFieldType >  DisplacementWriterType;
  DisplacementWriterType::Pointer      displacementwriter =  DisplacementWriterType::New();
  std::string outfilename( argv[3] );
  std::string  ext = itksys::SystemTools::GetFilenameExtension( outfilename );
  std::string name = itksys::SystemTools::GetFilenameWithoutExtension( outfilename );
  std::string path = itksys::SystemTools::GetFilenamePath( outfilename );
  std::string defout = path + std::string( "/" ) + name + std::string("_def") + ext;
  displacementwriter->SetFileName( defout.c_str() );
  displacementwriter->SetInput( displacementTransform->GetDisplacementField() );
  displacementwriter->Update();

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

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
