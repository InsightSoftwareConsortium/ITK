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
// class for performing registration of two $2D$ images in an ITKv4
// registration framework. Due to the large number of parameters of
// the BSpline transform, we will use a \doxygen{LBFGSOptimizerv4}
// instead of a simple steepest descent or a conjugate gradient
// descent optimizer.
//
//
// \index{itk::BSplineTransform}
// \index{itk::BSplineTransform!DeformableRegistration}
// \index{itk::LBFGSOptimizerv4}
//
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethodv4.h"
#include "itkCorrelationImageToImageMetricv4.h"

#include "itkTimeProbesCollectorBase.h"
#include "itkMemoryProbesCollectorBase.h"

//  Software Guide : BeginLatex
//
//  The following are the most relevant headers to this example.
//
//  \index{itk::BSplineTransform!header}
//  \index{itk::LBFGSOptimizerv4!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBSplineTransform.h"
#include "itkLBFGSOptimizerv4.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The parameter space of the \code{BSplineTransform} is composed by
//  the set of all the deformations associated with the nodes of the BSpline
//  grid.  This large number of parameters makes it possible to represent a wide
//  variety of deformations, at the cost of requiring a
//  significant amount of computation time.
//
//  \index{itk::BSplineTransform!header}
//
//  Software Guide : EndLatex

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"

#include "itkBSplineTransformInitializer.h"
#include "itkTransformToDisplacementFieldFilter.h"


// NOTE: the LBFGSOptimizerv4 does not invoke events


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
  //  We instantiate now the type of the \code{BSplineTransform} using
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
                                SplineOrder >           TransformType;
  // Software Guide : EndCodeSnippet

  typedef itk::LBFGSOptimizerv4                         OptimizerType;


  typedef itk::CorrelationImageToImageMetricv4<
                                    FixedImageType,
                                    MovingImageType >   MetricType;

  typedef itk::ImageRegistrationMethodv4<
                                    FixedImageType,
                                    MovingImageType >     RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();


  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );

  typedef itk::ImageFileReader< FixedImageType  >       FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType >       MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  fixedImageReader->Update();
  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();


  //  Software Guide : BeginLatex
  //
  //  The transform object is constructed below.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer    transform   = TransformType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Fixed parameters of the BSpline transform should be defined
  //  before the registration. These parameters define origin,
  //  dimension, direction and mesh size of the transform grid
  //  and are set based on specifications of the fixed image space
  //  lattice. We can use \doxygen{BSplineTransformInitializer} to
  //  initialize fixed parameters of a BSpline transform.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::BSplineTransformInitializer<
    TransformType,
    FixedImageType> InitializerType;

  InitializerType::Pointer transformInitializer = InitializerType::New();

  unsigned int numberOfGridNodesInOneDimension = 8;

  TransformType::MeshSizeType             meshSize;
  meshSize.Fill( numberOfGridNodesInOneDimension - SplineOrder );

  transformInitializer->SetTransform( transform );
  transformInitializer->SetImage( fixedImage );
  transformInitializer->SetTransformDomainMeshSize( meshSize );
  transformInitializer->InitializeTransform();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  After setting the fixed parameters of the transform, we set the
  //  initial transform to be an identity transform. It is like setting
  //  all the transform parameters to zero in created parameter space.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  transform->SetIdentity();
  // Software Guide : EndCodeSnippet

  std::cout << "Initial Parameters = " << std::endl;
  std::cout << transform->GetParameters() << std::endl;

  //  Software Guide : BeginLatex
  //
  //  Then, the initialized transform is connected to the registration
  //  object and is set to be optimized directly during the registration
  //  process.
  //
  //  Calling \code{InPlaceOn()} means that the current initialized transform
  //  will optimized directly and is grafted to the output, so it can be
  //  considered as the output transform object. Otherwise, the initial transform
  //  will be copied or ``cloned'' to the output transform object, and the copied
  //  object will be optimized during the registration process.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransform( transform );
  registration->InPlaceOn();
  // Software Guide : EndCodeSnippet


  registration->SetFixedImage( fixedImage );
  registration->SetMovingImage( movingImageReader->GetOutput() );

  //  Software Guide : BeginLatex
  //
  //  The \doxygen{RegistrationParameterScalesFromPhysicalShift} class
  //  is used to estimate the parameters scales before we set the optimizer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RegistrationParameterScalesFromPhysicalShift<MetricType>
    ScalesEstimatorType;
  ScalesEstimatorType::Pointer scalesEstimator = ScalesEstimatorType::New();
  scalesEstimator->SetMetric( metric );
  scalesEstimator->SetTransformForward( true );
  scalesEstimator->SetSmallParameterVariation( 1.0 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Now the scale estimator is passed to the \doxygen{LBFGSOptimizerv4},
  //  and we set other parameters of the optimizer as well.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetGradientConvergenceTolerance( 5e-2 );
  optimizer->SetLineSearchAccuracy( 1.2 );
  optimizer->SetDefaultStepLength( 1.5 );
  optimizer->TraceOn();
  optimizer->SetMaximumNumberOfFunctionEvaluations( 1000 );
  optimizer->SetScalesEstimator( scalesEstimator );
  // Software Guide : EndCodeSnippet

  //  A single level registration process is run using
  //  the shrink factor 1 and smoothing sigma 0.
  //
  const unsigned int numberOfLevels = 1;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 1 );
  shrinkFactorsPerLevel[0] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 1 );
  smoothingSigmasPerLevel[0] = 0;

  registration->SetNumberOfLevels( numberOfLevels );
  registration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  registration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );


  // Add time and memory probes
  itk::TimeProbesCollectorBase chronometer;
  itk::MemoryProbesCollectorBase memorymeter;

  std::cout << std::endl << "Starting Registration" << std::endl;

  try
    {
    memorymeter.Start( "Registration" );
    chronometer.Start( "Registration" );

    registration->Update();

    chronometer.Stop( "Registration" );
    memorymeter.Stop( "Registration" );

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

  // Report the time and memory taken by the registration
  chronometer.Report( std::cout );
  memorymeter.Report( std::cout );

  //  Software Guide : BeginLatex
  //
  //  Let's execute this example using the rat lung images from the previous examples.
  //
  // \begin{itemize}
  // \item \code{RatLungSlice1.mha}
  // \item \code{RatLungSlice2.mha}
  // \end{itemize}
  //
  //  The \emph{transform} object is updated during the registration process
  //  and is passed to the resampler to map the moving image space onto the
  //  fixed image space.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  OptimizerType::ParametersType finalParameters = transform->GetParameters();
  // Software Guide : EndCodeSnippet

  std::cout << "Last Transform Parameters" << std::endl;
  std::cout << finalParameters << std::endl;

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( transform );
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
  dispfieldGenerator->SetTransform( transform );
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
