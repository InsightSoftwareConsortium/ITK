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
// the BSpline transform, we will use a \doxygen{LBFGSOptimizerv4} instead of a
// simple steepest descent or a conjugate gradient descent optimizer.
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
//  grid.  This large number of parameters makes possible to represent a wide
//  variety of deformations, but it also has the price of requiring a
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
                                SplineOrder >     TransformType;
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< FixedImageType  >               FixedImageReaderType;
  FixedImageReaderType::Pointer  fixedImageReader = FixedImageReaderType::New();
  fixedImageReader->SetFileName(  argv[1] );
  fixedImageReader->Update();
  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();
  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();

  typedef itk::ImageFileReader< MovingImageType >               MovingImageReaderType;
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();
  movingImageReader->SetFileName( argv[2] );
  movingImageReader->Update();
  MovingImageType::ConstPointer movingImage = movingImageReader->GetOutput();

  //  Software Guide : BeginLatex
  //
  //  This example works with either CC or MSQ metrics.
  //  Note that in the case of using MSQ metric, you may need to tune the optimizer parameters.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CorrelationImageToImageMetricv4<FixedImageType, MovingImageType> MetricType;
  MetricType::Pointer metric = MetricType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We need to estimate scales before we set the optimizer.
  //  Scales are used to compensate the relative differences between parameters (e.g translation vs rotation)
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RegistrationParameterScalesFromPhysicalShift<MetricType> ScalesEstimatorType;
  ScalesEstimatorType::Pointer scalesEstimator = ScalesEstimatorType::New();
  scalesEstimator->SetMetric( metric );
  scalesEstimator->SetTransformForward( true );
  scalesEstimator->SetSmallParameterVariation( 1.0 );
  // Software Guide : EndCodeSnippet

  typedef itk::LBFGSOptimizerv4       OptimizerType;
  OptimizerType::Pointer      optimizer     = OptimizerType::New();

  //  Software Guide : BeginLatex
  //
  //  Next we set the parameters of the LBFGS Optimizer.
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

  typedef itk::ImageRegistrationMethodv4<
                                        FixedImageType,
                                        MovingImageType,
                                        TransformType >    RegistrationType;
  RegistrationType::Pointer   registration  = RegistrationType::New();

  // One level registration is performed using the shrink factor 1 and smoothing sigma 1

  const unsigned int numberOfLevels = 1;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 1 );
  shrinkFactorsPerLevel[0] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 1 );
  smoothingSigmasPerLevel[0] = 0;

  //  Software Guide : BeginLatex
  //
  //  The transform object is constructed below.
  //  The final transform will be the output of the registration.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer transform = const_cast<TransformType *>( registration->GetOutput()->Get() );
  // Software Guide : EndCodeSnippet

  registration->SetFixedImage( fixedImage );
  registration->SetMovingImage( movingImage );
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetNumberOfLevels( numberOfLevels );
  registration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  registration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );

  // Initialize the BSpline transform

  TransformType::PhysicalDimensionsType   fixedPhysicalDimensions;
  TransformType::MeshSizeType             meshSize;
  TransformType::OriginType               fixedOrigin;

  unsigned int numberOfGridNodesInOneDimension = 8;

  for( unsigned int i=0; i< SpaceDimension; i++ )
    {
    fixedOrigin[i] = fixedImage->GetOrigin()[i];
    fixedPhysicalDimensions[i] = fixedImage->GetSpacing()[i] *
                                  static_cast<double>( fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1 );
    }
  meshSize.Fill( numberOfGridNodesInOneDimension - SplineOrder );

  transform->SetTransformDomainOrigin( fixedOrigin );
  transform->SetTransformDomainPhysicalDimensions( fixedPhysicalDimensions );
  transform->SetTransformDomainMeshSize( meshSize );
  transform->SetTransformDomainDirection( fixedImage->GetDirection() );
  transform->SetIdentity();

  std::cout << "Intial Parameters = " << std::endl;
  std::cout << transform->GetParameters() << std::endl;

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

    const OptimizerType::ConstPointer  outputOptimizer = dynamic_cast<const OptimizerType *>( registration->GetOptimizer() );
    if( outputOptimizer.IsNotNull() )
      {
      std::cout << "Optimizer stop condition = "
                << outputOptimizer->GetStopConditionDescription()
                << std::endl;
      }
    else
      {
      std::cerr << "Output optimizer is null." << std::endl;
      return EXIT_FAILURE;
      }
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  // While the registration filter is run, it updates the output transform parameters with the final registration parameters

  OptimizerType::ParametersType finalParameters = transform->GetParameters();

  std::cout << "Last Transform Parameters" << std::endl;
  std::cout << finalParameters << std::endl;


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
  //
  //  Software Guide : EndLatex

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( transform );
  resample->SetInput( movingImage );

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

  typedef itk::Vector< float, ImageDimension >      VectorType;
  typedef itk::Image< VectorType, ImageDimension >  DisplacementFieldType;

  DisplacementFieldType::Pointer field = DisplacementFieldType::New();
  field->SetRegions( fixedRegion );
  field->SetOrigin( fixedImage->GetOrigin() );
  field->SetSpacing( fixedImage->GetSpacing() );
  field->SetDirection( fixedImage->GetDirection() );
  field->Allocate();

  typedef itk::ImageRegionIterator< DisplacementFieldType > FieldIterator;
  FieldIterator fi( field, fixedRegion );

  fi.GoToBegin();

  TransformType::InputPointType  fixedPoint;
  TransformType::OutputPointType movingPoint;
  DisplacementFieldType::IndexType index;

  VectorType displacement;

  while( ! fi.IsAtEnd() )
    {
    index = fi.GetIndex();
    field->TransformIndexToPhysicalPoint( index, fixedPoint );
    movingPoint = transform->TransformPoint( fixedPoint );
    displacement = movingPoint - fixedPoint;
    fi.Set( displacement );
    ++fi;
    }

  typedef itk::ImageFileWriter< DisplacementFieldType >  FieldWriterType;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput( field );

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
