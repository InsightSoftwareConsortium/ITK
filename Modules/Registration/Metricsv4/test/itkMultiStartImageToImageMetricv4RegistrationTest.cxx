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
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkCorrelationImageToImageMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkMultiStartOptimizerv4.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkCastImageFilter.h"

#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iomanip>

int itkMultiStartImageToImageMetricv4RegistrationTest(int argc, char *argv[])
{

  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [numberOfIterations bool_rotate_input_image_by_180 ] ";
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
  bool rotateinput=false;
  if( argc > 5 )
  if ( atoi(argv[5]) == 1 ) rotateinput=true;
  const unsigned int Dimension = 2;
  typedef unsigned short PixelType; //I assume png is unsigned short
  typedef double InternalPixelType;

  typedef itk::Image< PixelType, Dimension >          InputImageType;
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;

  typedef itk::ImageFileReader< InputImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< InputImageType >  MovingImageReaderType;

  FixedImageReaderType::Pointer fixedImageReader   = FixedImageReaderType::New();
  fixedImageReader->SetFileName( argv[1] );
  fixedImageReader->Update();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();
  movingImageReader->SetFileName( argv[2] );
  movingImageReader->Update();

  //get the images
  typedef itk::CastImageFilter<
                        InputImageType,
                        InternalImageType >           CastFilterType;
  CastFilterType::Pointer  fixedcaster =  CastFilterType::New();
  fixedcaster->SetInput( fixedImageReader->GetOutput() ); // resample->GetOutput()
  fixedcaster->Update();
  InternalImageType::Pointer  fixedImage = fixedcaster->GetOutput();

  //get the images
  CastFilterType::Pointer  movingcaster =  CastFilterType::New();
  movingcaster->SetInput( movingImageReader->GetOutput() );
  movingcaster->Update();
  InternalImageType::Pointer  movingImage = movingcaster->GetOutput();

  /** Now set up a rotation about the center of the image */
  //create an affine transform
  typedef itk::AffineTransform<double, Dimension>
                                                    AffineTransformType;
  InternalImageType::IndexType centerindex;
  InternalImageType::PointType mpoint;
  InternalImageType::PointType fpoint;
  centerindex[0]=movingImage->GetLargestPossibleRegion().GetSize()[0]/2;
  centerindex[1]=movingImage->GetLargestPossibleRegion().GetSize()[1]/2;
  movingImage->TransformIndexToPhysicalPoint(centerindex,mpoint);
  centerindex[0]=fixedImage->GetLargestPossibleRegion().GetSize()[0]/2;
  centerindex[1]=fixedImage->GetLargestPossibleRegion().GetSize()[1]/2;
  fixedImage->TransformIndexToPhysicalPoint(centerindex,fpoint);
  AffineTransformType::OutputVectorType moffset;
  moffset[0]=mpoint[0]*(-1);
  moffset[1]=mpoint[1]*(-1);
  AffineTransformType::OutputVectorType foffset;
  foffset[0]=fpoint[0];
  foffset[1]=fpoint[1];

  AffineTransformType::Pointer affineTransformGroundTruth = AffineTransformType::New();
  affineTransformGroundTruth->SetIdentity();
  affineTransformGroundTruth->Translate(moffset);
  affineTransformGroundTruth->Rotate2D(itk::Math::pi);
  affineTransformGroundTruth->Translate(foffset);

  /** define a resample filter that will ultimately be used to deform the image */
  typedef itk::ResampleImageFilter<
                            InternalImageType,
                            InternalImageType >    ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();
  resample->SetTransform( affineTransformGroundTruth );
  resample->SetInput( movingImage );
  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 0 );
  resample->Update();

  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout <<" affineTransform params prior to optimization " << affineTransform->GetParameters() << std::endl;

  //identity transform for fixed image
  typedef itk::IdentityTransform<double, Dimension> IdentityTransformType;
  IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  // The metric
  //  typedef itk::MeanSquaresImageToImageMetricv4 < InternalImageType, InternalImageType >
  typedef itk::CorrelationImageToImageMetricv4 < InternalImageType, InternalImageType >
  //  typedef itk::MattesMutualInformationImageToImageMetricv4 < InternalImageType, InternalImageType >
  //  typedef itk::JointHistogramMutualInformationImageToImageMetricv4 < InternalImageType, InternalImageType >
                                                                                        MetricType;
  typedef MetricType::FixedSampledPointSetType                                        PointSetType;
  MetricType::Pointer metric = MetricType::New();
  //  metric->SetNumberOfHistogramBins(20);
  typedef PointSetType::PointType     PointType;
  PointSetType::Pointer               pset(PointSetType::New());
  unsigned long ind=0,ct=0;
  itk::ImageRegionIteratorWithIndex<InternalImageType> It(fixedImage, fixedImage->GetLargestPossibleRegion() );
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
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  if( rotateinput ) metric->SetMovingImage( resample->GetOutput() );
  metric->SetFixedTransform( identityTransform );
  metric->SetMovingTransform( affineTransform );
  bool gaussian=false;
  metric->SetUseMovingImageGradientFilter( gaussian );
  metric->SetUseFixedImageGradientFilter( gaussian );
  metric->Initialize();

  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType > RegistrationParameterScalesFromShiftType;
  RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  OptimizerType::Pointer  optimizer = OptimizerType::New();
  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetScalesEstimator( shiftScaleEstimator );
  optimizer->SetMaximumStepSizeInPhysicalUnits( 1 );
  optimizer->SetConvergenceWindowSize(20);
  optimizer->SetMinimumConvergenceValue(-1.e-5);

  typedef  itk::MultiStartOptimizerv4  MOptimizerType;
  MOptimizerType::Pointer  MOptimizer = MOptimizerType::New();
  MOptimizerType::ParametersListType parametersList = MOptimizer->GetParametersList();
  float rotplus=10;
  //  for (  float i = 180; i <= 180; i+=rotplus )
  for (  float i = 0; i < 360; i+=rotplus )
    {
    AffineTransformType::Pointer aff = AffineTransformType::New();
    aff->SetIdentity();
    float rad=(float)i*itk::Math::pi /180.0;
    aff->Translate(moffset);
    aff->Rotate2D(rad);
    aff->Translate(foffset);
    parametersList.push_back( aff->GetParameters() );
    }
  MOptimizer->SetMetric( metric );
  MOptimizer->SetParametersList( parametersList );
  MOptimizer->SetLocalOptimizer(optimizer);
  MOptimizer->StartOptimization();
  affineTransform->SetParameters(MOptimizer->GetBestParameters());

  MOptimizerType::MetricValuesListType  metlist=MOptimizer->GetMetricValuesList();
  for (unsigned int i=0; i < metlist.size(); i++)
    {
    std::cout << " angle " << i*rotplus <<" energy " <<metlist[i] <<  std::endl;
    }
  std::cout << " best angle " << MOptimizer->GetBestParametersIndex()*rotplus << " energy " << metlist[ MOptimizer->GetBestParametersIndex() ] << std::endl;
  std::cout <<" Done.  Best parameters: " <<MOptimizer->GetBestParameters() << " index " << MOptimizer->GetBestParametersIndex() << std::endl;
  std::cout <<" Ground truth parameters: " << affineTransformGroundTruth->GetParameters() << std::endl;
  //warp the image with the displacement field
  ResampleFilterType::Pointer resampleout = ResampleFilterType::New();
  resampleout->SetTransform( affineTransform );
  resampleout->SetInput( movingImage );
  if ( rotateinput )  resampleout->SetInput( resample->GetOutput() );
  resampleout->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resampleout->SetOutputOrigin(  fixedImage->GetOrigin() );
  resampleout->SetOutputSpacing( fixedImage->GetSpacing() );
  resampleout->SetOutputDirection( fixedImage->GetDirection() );
  resampleout->SetDefaultPixelValue( 0 );
  resampleout->Update();
  //write the warped image into a file
  typedef itk::ImageFileWriter< InternalImageType >   WriterType;
  WriterType::Pointer      writer =  WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( resampleout->GetOutput() );
  writer->Update();
  std::cout << "After optimization affine params are: " <<  affineTransform->GetParameters() << std::endl;
  if ( MOptimizer->GetBestParametersIndex() == 18 ) return EXIT_SUCCESS;
  else return EXIT_FAILURE;
}
