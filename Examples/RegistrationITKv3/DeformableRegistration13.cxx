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
// This example is almost identical to
// Section~\ref{sec:DeformableRegistration12}, with the difference that it
// illustrates who to use the RegularStepGradientDescentOptimizer for a
// deformable registration task.
//
// \index{itk::BSplineTransform}
// \index{itk::BSplineTransform!DeformableRegistration}
// \index{itk::RegularStepGradientDescentOptimizer}
//
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethod.h"
#include "itkMattesMutualInformationImageToImageMetric.h"

#include "itkTimeProbesCollectorBase.h"
#include "itkMemoryProbesCollectorBase.h"

//  Software Guide : BeginLatex
//
//  The following are the most relevant headers to this example.
//
//  \index{itk::BSplineTransform!header}
//  \index{itk::RegularStepGradientDescentOptimizer!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBSplineTransform.h"
#include "itkRegularStepGradientDescentOptimizer.h"
// Software Guide : EndCodeSnippet
#include "itkMersenneTwisterRandomVariateGenerator.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"


//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
//
#include "itkCommand.h"
class CommandIterationUpdate : public itk::Command
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef itk::SmartPointer<Self>   Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:
  typedef itk::RegularStepGradientDescentOptimizer OptimizerType;
  typedef   const OptimizerType *                  OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
    if( !(itk::IterationEvent().CheckEvent( &event )) )
      {
      return;
      }
    std::cout << "Iteration : ";
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << std::endl;
    }
};


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile  ";
    std::cerr << " [differenceOutputfile] [differenceBeforeRegistration] ";
    std::cerr << " [deformationField] ";
    std::cerr << " [useExplicitPDFderivatives ] [useCachingBSplineWeights ] ";
    std::cerr << " [filenameForFinalTransformParameters] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }


  const    unsigned int    ImageDimension = 2;
  typedef  unsigned char   PixelType;

  typedef itk::Image< PixelType, ImageDimension >  FixedImageType;
  typedef itk::Image< PixelType, ImageDimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //
  //  We instantiate now the type of the \code{BSplineTransform}
  //  using as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the BSpline. We also intantiate
  //  the type of the optimizer.
  //
  //  \index{BSplineTransform!New}
  //  \index{BSplineTransform!Instantiation}
  //  \index{RegularStepGradientDescentOptimizer!Instantiation}
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

  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  // Software Guide : EndCodeSnippet


  typedef itk::MattesMutualInformationImageToImageMetric<
                                    FixedImageType,
                                    MovingImageType >    MetricType;

  typedef itk:: LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double          >    InterpolatorType;

  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType >    RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();


  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetInterpolator(  interpolator  );


  TransformType::Pointer  transform = TransformType::New();
  registration->SetTransform( transform );

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

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();

  registration->SetFixedImageRegion( fixedRegion );

  unsigned int numberOfGridNodesInOneDimension = 7;

  // Software Guide : BeginCodeSnippet

  TransformType::PhysicalDimensionsType   fixedPhysicalDimensions;
  TransformType::MeshSizeType             meshSize;
  TransformType::OriginType               fixedOrigin;

  for( unsigned int i=0; i< SpaceDimension; i++ )
    {
    fixedOrigin[i] = fixedImage->GetOrigin()[i];
    fixedPhysicalDimensions[i] = fixedImage->GetSpacing()[i] *
      static_cast<double>(
      fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1 );
    }
  meshSize.Fill( numberOfGridNodesInOneDimension - SplineOrder );

  transform->SetTransformDomainOrigin( fixedOrigin );
  transform->SetTransformDomainPhysicalDimensions(
    fixedPhysicalDimensions );
  transform->SetTransformDomainMeshSize( meshSize );
  transform->SetTransformDomainDirection( fixedImage->GetDirection() );

  typedef TransformType::ParametersType     ParametersType;

  const unsigned int numberOfParameters =
               transform->GetNumberOfParameters();

  ParametersType parameters( numberOfParameters );

  parameters.Fill( 0.0 );

  transform->SetParameters( parameters );

  registration->SetInitialTransformParameters( transform->GetParameters() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Next we set the parameters of the RegularStepGradientDescentOptimizer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( 10.0   );
  optimizer->SetMinimumStepLength(  0.01 );

  optimizer->SetRelaxationFactor( 0.7 );
  optimizer->SetNumberOfIterations( 200 );
  // Software Guide : EndCodeSnippet

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  metric->SetNumberOfHistogramBins( 50 );

  const unsigned int numberOfSamples =
    static_cast<unsigned int>( fixedRegion.GetNumberOfPixels() * 60.0 / 100.0 );

  metric->SetNumberOfSpatialSamples( numberOfSamples );

  // For consistent results when regression testing.
  metric->ReinitializeSeed( 121213 );

  if( argc > 7 )
    {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the Transform
    // parameters, or doing it by progressively accumulating contributions from
    // each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives( atoi( argv[7] ) );
    }

  if( argc > 8 )
    {
    // Define whether to cache the BSpline weights and indexes corresponding to
    // each one of the samples used to compute the metric. Enabling caching will
    // make the algorithm run faster but it will have a cost on the amount of memory
    // that needs to be allocated. This option is only relevant when using the
    // BSplineTransform.
    metric->SetUseCachingOfBSplineWeights( atoi( argv[8] ) );
    }


  // Add a time probe
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

  OptimizerType::ParametersType finalParameters =
                    registration->GetLastTransformParameters();


  // Report the time and memory taken by the registration
  chronometer.Report( std::cout );
  memorymeter.Report( std::cout );

  transform->SetParameters( finalParameters );


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

  // This value is set to zero in order to make easier to perform
  // regression testing in this example. However, for didactic
  // exercise it will be better to set it to a medium gray value
  // such as 100 or 128.
  resample->SetDefaultPixelValue( 0 );

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
  if( argc > 4 )
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
  if( argc > 5 )
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
  if( argc > 6 )
    {

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

  // Optionally, save the transform parameters in a file
  if( argc > 9 )
    {
    std::ofstream parametersFile;
    parametersFile.open( argv[9] );
    parametersFile << finalParameters << std::endl;
    parametersFile.close();
    }

  return EXIT_SUCCESS;
}
