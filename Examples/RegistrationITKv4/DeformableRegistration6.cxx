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

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{BSplineTransform}
// class in a multi-resolution scheme. Here we run 3 levels of resolutions.
// The first level of registration is performed with the spline grid of
// low resolution. Then, a common practice is to increase the resolution
// of the B-spline mesh (or, analogously, the control point grid size)
// at each level.
//
// For this purpose, we introduce the concept of transform adaptors.
// Each level of each stage is defined by a transform adaptor
// which describes how to adapt the transform to the current level by
// increasing the resolution from the previous level.
// Here, we used \doxygen{BSplineTransformParametersAdaptor} class
// to adapt the BSpline transform parameters at each resolution level.
// Note that for many transforms, such as affine, the
// concept of an adaptor may be nonsensical since the number of transform
// parameters does not change between resolution levels.
//
// Since this example is quite similar to the previous example on the use
// of the \code{BSplineTransform} we omit most of the details already
// discussed and will focus on the aspects related to the multi-resolution
// approach.
//
// \index{itk::BSplineTransform}
// \index{itk::BSplineTransform!DeformableRegistration}
// \index{itk::LBFGSOptimizerv4}
// \index{itk::BSplineTransformParametersAdaptor}
//
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethodv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"

//  Software Guide : BeginLatex
//
//  We include the header files for the transform, optimizer and adaptor.
//
//  \index{itk::BSplineTransform!header}
//  \index{itk::LBFGSOptimizer!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBSplineTransform.h"
#include "itkLBFGSOptimizerv4.h"
#include "itkBSplineTransformParametersAdaptor.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"

#include "itkIdentityTransform.h"

#include "itkBSplineTransformInitializer.h"
#include "itkTransformToDisplacementFieldFilter.h"

// NOTE: the LBFGSOptimizer does not invoke events


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile  ";
    std::cerr << " [differenceOutputfile] [differenceBeforeRegistration] ";
    std::cerr << " [deformationField] ";
    return EXIT_FAILURE;
    }

  const    unsigned int    ImageDimension = 2;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, ImageDimension >  FixedImageType;
  typedef itk::Image< PixelType, ImageDimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //
  //  We instantiate the type of the \code{BSplineTransform} using
  //  as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the BSpline.
  //
  //  \index{BSplineTransform!New}
  //  \index{BSplineTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int SpaceDimension = ImageDimension;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::BSplineTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            SplineOrder >     TransformType;
  // Software Guide : EndCodeSnippet


  typedef itk::LBFGSOptimizerv4       OptimizerType;


  typedef itk::MeanSquaresImageToImageMetricv4<
                                    FixedImageType,
                                    MovingImageType >    MetricType;

  typedef itk::ImageRegistrationMethodv4<
                                    FixedImageType,
                                    MovingImageType >    RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();


  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImage(  fixedImage   );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  fixedImageReader->Update();

  //  Software Guide : BeginLatex
  //
  //  We construct the transform object, initialize its parameters and
  //  connect that to the registration object.
  //
  //  \index{itk::RegistrationMethod!SetTransform()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer  outputBSplineTransform = TransformType::New();

  // Initialize the fixed parameters of transform (grid size, etc).
  //
  typedef itk::BSplineTransformInitializer<
    TransformType,
    FixedImageType> InitializerType;

  InitializerType::Pointer transformInitializer = InitializerType::New();

  unsigned int numberOfGridNodesInOneDimension = 8;

  TransformType::MeshSizeType             meshSize;
  meshSize.Fill( numberOfGridNodesInOneDimension - SplineOrder );

  transformInitializer->SetTransform( outputBSplineTransform );
  transformInitializer->SetImage( fixedImage );
  transformInitializer->SetTransformDomainMeshSize( meshSize );
  transformInitializer->InitializeTransform();

  // Set transform to identity
  //
  typedef TransformType::ParametersType     ParametersType;
  const unsigned int numberOfParameters =
               outputBSplineTransform->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );
  parameters.Fill( 0.0 );
  outputBSplineTransform->SetParameters( parameters );

  registration->SetInitialTransform( outputBSplineTransform );
  registration->InPlaceOn();
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The registration process is run in three levels. The shrink factors
  //  and smoothing sigmas are set for each level.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfLevels = 3;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( numberOfLevels );
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( numberOfLevels );
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 0;

  registration->SetNumberOfLevels( numberOfLevels );
  registration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  registration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Create the transform adaptors to modify the flexibility
  //  of the deformable transform for each level of this
  //  multi-resolution scheme.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  RegistrationType::TransformParametersAdaptorsContainerType adaptors;

  // First, get fixed image physical dimensions
  TransformType::PhysicalDimensionsType             fixedPhysicalDimensions;
  for( unsigned int i=0; i< SpaceDimension; i++ )
    {
    fixedPhysicalDimensions[i] = fixedImage->GetSpacing()[i] *
    static_cast<double>(
      fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1 );
    }

  // Create the transform adaptors specific to B-splines
  for( unsigned int level = 0; level < numberOfLevels; level++ )
    {
    typedef itk::ShrinkImageFilter<
      FixedImageType,
      FixedImageType> ShrinkFilterType;
    ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors( shrinkFactorsPerLevel[level] );
    shrinkFilter->SetInput( fixedImage );
    shrinkFilter->Update();

    // A good heuristic is to double the b-spline mesh resolution at each level
    //
    TransformType::MeshSizeType requiredMeshSize;
    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      requiredMeshSize[d] = meshSize[d] << level;
      }

    typedef itk::BSplineTransformParametersAdaptor<TransformType>
      BSplineAdaptorType;
    BSplineAdaptorType::Pointer bsplineAdaptor = BSplineAdaptorType::New();
    bsplineAdaptor->SetTransform( outputBSplineTransform );
    bsplineAdaptor->SetRequiredTransformDomainMeshSize( requiredMeshSize );
    bsplineAdaptor->SetRequiredTransformDomainOrigin(
      shrinkFilter->GetOutput()->GetOrigin() );
    bsplineAdaptor->SetRequiredTransformDomainDirection(
      shrinkFilter->GetOutput()->GetDirection() );
    bsplineAdaptor->SetRequiredTransformDomainPhysicalDimensions(
      fixedPhysicalDimensions );

    adaptors.push_back( bsplineAdaptor.GetPointer() );
    }

  registration->SetTransformParametersAdaptorsPerLevel( adaptors );
  //  Software Guide : EndCodeSnippet

  // Scale estimator
  typedef itk::RegistrationParameterScalesFromPhysicalShift<MetricType> ScalesEstimatorType;
  ScalesEstimatorType::Pointer scalesEstimator = ScalesEstimatorType::New();
  scalesEstimator->SetMetric( metric );
  scalesEstimator->SetTransformForward( true );
  scalesEstimator->SetSmallParameterVariation( 1.0 );

  // Set Optimizer
  optimizer->SetScalesEstimator( scalesEstimator );
  optimizer->SetGradientConvergenceTolerance( 0.05 );
  optimizer->SetLineSearchAccuracy( 0.9 );
  optimizer->SetDefaultStepLength( 1.5 );
  optimizer->TraceOn();
  optimizer->SetMaximumNumberOfFunctionEvaluations( 1000 );

  std::cout << "Starting Registration "
            << std::endl;

  try
    {
    registration->Update();
    std::cout << "Optimizer stop condition = "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  // Finally we use the last transform in order to resample the image.
  //
  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( outputBSplineTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 100 );

  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, ImageDimension > OutputImageType;

  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType > CastFilterType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();


  writer->SetFileName( argv[3] );


  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );


  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::SquaredDifferenceImageFilter<
                                  FixedImageType,
                                  FixedImageType,
                                  OutputImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( difference->GetOutput() );


  // Compute the difference image between the
  // fixed and resampled moving image.
  if( argc >= 5 )
    {
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( resample->GetOutput() );
    writer2->SetFileName( argv[4] );
    try
      {
      writer2->Update();
      }
    catch( itk::ExceptionObject & err )
      {
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    }


  // Compute the difference image between the
  // fixed and moving image before registration.
  if( argc >= 6 )
    {
    writer2->SetFileName( argv[5] );
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( movingImageReader->GetOutput() );
    try
      {
      writer2->Update();
      }
    catch( itk::ExceptionObject & err )
      {
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Generate the explicit deformation field resulting from
  // the registration.
  typedef itk::Vector< float, ImageDimension >          VectorPixelType;
  typedef itk::Image< VectorPixelType, ImageDimension > DisplacementFieldImageType;

  typedef itk::TransformToDisplacementFieldFilter<
                        DisplacementFieldImageType,
                        CoordinateRepType >             DisplacementFieldGeneratorType;

  /** Create an setup displacement field generator. */
  DisplacementFieldGeneratorType::Pointer dispfieldGenerator =
                                                  DisplacementFieldGeneratorType::New();
  dispfieldGenerator->UseReferenceImageOn();
  dispfieldGenerator->SetReferenceImage( fixedImage );
  dispfieldGenerator->SetTransform( outputBSplineTransform );
  try
  {
  dispfieldGenerator->Update();
  }
  catch ( itk::ExceptionObject & err )
  {
  std::cerr << "Exception detected while generating deformation field";
  std::cerr << " : "  << err << std::endl;
  return EXIT_FAILURE;
  }

  typedef itk::ImageFileWriter< DisplacementFieldImageType >  FieldWriterType;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput( dispfieldGenerator->GetOutput() );

  if( argc >= 7 )
    {
    fieldWriter->SetFileName( argv[6] );
    try
      {
      fieldWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
