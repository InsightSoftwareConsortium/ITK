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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainT1SliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceShifted13x17y.png}
//    OUTPUTS: {MultiResImageRegistration1Output.png}
//    ARGUMENTS: 128
//    OUTPUTS: {MultiResImageRegistration1CheckerboardBefore.png}
//    OUTPUTS: {MultiResImageRegistration1CheckerboardAfter.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// \index{itk::ImageRegistrationMethod!Multi-Resolution}
// \index{itk::ImageRegistrationMethod!Multi-Modality}
// \index{itk::Multi\-Resolution\-Image\-Registration\-Method}
//
// This example illustrates the use of the
// \doxygen{MultiResolutionImageRegistrationMethod} to solve a simple
// multi-modality registration problem. In addition to the two input images,
// a transform, a metric, an interpolator and an optimizer, the
// multi-resolution framework also requires two image pyramids for creating
// the sequence of downsampled images.  To begin the example, we include the
// headers of the registration components we will use.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkCheckerBoardImageFilter.h"


// Software Guide : BeginLatex
//
// The MultiResolutionImageRegistrationMethod solves a registration
// problem in a coarse to fine manner as illustrated in Figure
// \ref{fig:MultiResRegistrationConcept}. The registration is first performed
// at the coarsest level using the images at the first level of the fixed and
// moving image pyramids. The transform parameters determined by the
// registration are then used to initialize the registration at the next finer
// level using images from the second level of the pyramids. This process is
// repeated as we work up to the finest level of image resolution.
//
// \begin{figure}
// \center
// \includegraphics[width=\textwidth]{MultiResRegistrationConcept}
// \itkcaption[Conceptual representation of Multi-Resolution
// registration]{Conceptual representation of the multi-resolution registration process.}
// \label{fig:MultiResRegistrationConcept}
// \end{figure}
//
// Software Guide : EndLatex


// Software Guide : BeginLatex
//
// In a typical registration scenario, a user will tweak component settings
// or even swap out components between multi-resolution levels. For example,
// when optimizing at a coarse resolution, it may be possible to take more
// aggressive step sizes and have a more relaxed convergence criterion.
// Another possible scheme is to use a simple translation transform for the
// initial coarse registration and upgrade to an affine transform at the
// finer levels.
//
// Tweaking the components between resolution levels can be done using ITK's
// implementation of the \emph{Command/Observer} design pattern. Before
// beginning registration at each resolution level,
// MultiResolutionImageRegistrationMethod invokes an
// IterationEvent. The registration components can be changed by
// implementing a \doxygen{Command} which responds to the
// event. A brief description the interaction between events and commands was
// previously presented in Section \ref{sec:MonitoringImageRegistration}.
//
// We will illustrate this mechanism by changing the parameters of the
// optimizer between each resolution level by way of a simple interface
// command. First, we include the header file of the Command class.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCommand.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Our new interface command class is called
// \code{RegistrationInterfaceCommand}. It derives from
// Command and is templated over the
// multi-resolution registration type.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
template <typename TRegistration>
class RegistrationInterfaceCommand : public itk::Command
{
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We then define \code{Self}, \code{Superclass}, \code{Pointer},
  // \code{New()} and a constructor in a similar fashion to the
  // \code{CommandIterationUpdate} class in Section
  // \ref{sec:MonitoringImageRegistration}.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet

public:
  typedef  RegistrationInterfaceCommand   Self;
  typedef  itk::Command                   Superclass;
  typedef  itk::SmartPointer<Self>        Pointer;
  itkNewMacro( Self );

protected:
  RegistrationInterfaceCommand() {};
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // For convenience, we declare types useful for converting pointers
  // in the \code{Execute()} method.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet

public:
  typedef   TRegistration                              RegistrationType;
  typedef   RegistrationType *                         RegistrationPointer;
  typedef   itk::RegularStepGradientDescentOptimizer   OptimizerType;
  typedef   OptimizerType *                            OptimizerPointer;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Two arguments are passed to the \code{Execute()} method: the first
  // is the pointer to the object which invoked the event and the
  // second is the event that was invoked.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  void Execute(itk::Object * object, const itk::EventObject & event)
    {
    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    //
    // First we verify if that the event invoked is of the right type.
    // If not, we return without any further action.
    //
    // Software Guide : EndLatex
    // Software Guide : BeginCodeSnippet
    if( !(itk::IterationEvent().CheckEvent( &event )) )
      {
      return;
      }
    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    //
    // We then convert the input object pointer to a RegistrationPointer.
    // Note that no error checking is done here to verify if the
    // \code{dynamic\_cast} was successful since we know the actual object
    // is a multi-resolution registration method.
    //
    // Software Guide : EndLatex
    // Software Guide : BeginCodeSnippet
    RegistrationPointer registration = static_cast<RegistrationPointer>( object );
    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    //
    // If this is the first resolution level we set the maximum step length
    // (representing the first step size) and the minimum step length (representing
    // the convergence criterion) to large values.  At each subsequent resolution
    // level, we will reduce the minimum step length by a factor of 10 in order to
    // allow the optimizer to focus on progressively smaller regions. The maximum
    // step length is set up to the current step length. In this way, when the
    // optimizer is reinitialized at the beginning of the registration process for
    // the next level, the step length will simply start with the last value used
    // for the previous level. This will guarantee the continuity of the path
    // taken by the optimizer through the parameter space.
    //
    // Software Guide : EndLatex
    // Software Guide : BeginCodeSnippet
    OptimizerPointer optimizer = static_cast< OptimizerPointer >(registration->GetModifiableOptimizer() );

    std::cout << "-------------------------------------" << std::endl;
    std::cout << "MultiResolution Level : "
              << registration->GetCurrentLevel()  << std::endl;
    std::cout << std::endl;

    if ( registration->GetCurrentLevel() == 0 )
      {
      optimizer->SetMaximumStepLength( 16.00 );
      optimizer->SetMinimumStepLength( 0.01 );
      }
    else
      {
      optimizer->SetMaximumStepLength(
                                   optimizer->GetMaximumStepLength() * 0.25 );
      optimizer->SetMinimumStepLength(
                                    optimizer->GetMinimumStepLength() * 0.1 );
      }
  }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Another version of the \code{Execute()} method accepting a \code{const}
  // input object is also required since this method is defined as pure virtual
  // in the base class.  This version simply returns without taking any action.
  //
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  void Execute(const itk::Object * , const itk::EventObject & )
    { return; }
};
// Software Guide : EndCodeSnippet


//  The following section of code implements an observer
//  that will monitor the evolution of the registration process.
//
class CommandIterationUpdate : public itk::Command
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef  itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:
  typedef   itk::RegularStepGradientDescentOptimizer  OptimizerType;
  typedef   const OptimizerType *                     OptimizerPointer;

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
      std::cout << optimizer->GetCurrentIteration() << "   ";
      std::cout << optimizer->GetValue() << "   ";
      std::cout << optimizer->GetCurrentPosition() << std::endl;
    }
};


int main( int argc, const char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << " outputImagefile [backgroundGrayLevel]";
    std::cerr << " [checkerBoardBefore] [checkerBoardAfter]";
    std::cerr << " [useExplicitPDFderivatives ] " << std::endl;
    std::cerr << " [numberOfBins] [numberOfSamples ] " << std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  unsigned short  PixelType;

  const std::string fixedImageFile  = argv[1];
  const std::string movingImageFile = argv[2];
  const std::string outImagefile    = argv[3];
  const PixelType backgroundGrayLevel  = (argc >4 )? atoi(argv[4]): 100;
  const std::string checkerBoardBefore = (argc >5 )?      argv[5]: "";
  const std::string checkerBoardAfter  = (argc >6 )?      argv[6]: "";
  const bool useExplicitPDFderivatives = (argc >7 )? static_cast<bool>(atoi(argv[7])): false;
  const int numberOfBins               = (argc >8 )? atoi(argv[8]): 0;
  const int numberOfSamples            = (argc >9 )? atoi(argv[9]): 0;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  //  Software Guide : BeginLatex
  //
  //  The fixed and moving image types are defined as in previous
  //  examples.  Due to the recursive nature of the process by which the
  //  downsampled images are computed by the image pyramids, the output
  //  images are required to have real pixel types. We declare this internal
  //  image type to be \code{InternalPixelType}:
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef   float                                    InternalPixelType;
  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The types for the registration components are then derived using
  //  the internal image type.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction<
                                    InternalImageType,
                                    double             > InterpolatorType;
  typedef itk::MattesMutualInformationImageToImageMetric<
                                    InternalImageType,
                                    InternalImageType >   MetricType;
  typedef itk::MultiResolutionImageRegistrationMethod<
                                    InternalImageType,
                                    InternalImageType >   RegistrationType;
  // Software Guide: EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // In the multi-resolution framework, a
  // \doxygen{MultiResolutionPyramidImageFilter} is used to create a pyramid
  // of downsampled images. The size of each downsampled image is specified
  // by the user in the form of a schedule of shrink factors. A description
  // of the filter and the format of the schedules are found in
  // Section \ref{sec:ImagePyramids}. For this example, we will simply use
  // the default schedules.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::MultiResolutionPyramidImageFilter<
            InternalImageType, InternalImageType >   FixedImagePyramidType;
  typedef itk::MultiResolutionPyramidImageFilter<
            InternalImageType, InternalImageType >   MovingImagePyramidType;
  // Software Guide: EndCodeSnippet


  //  All the components are instantiated using their \code{New()} method
  //  and connected to the registration object as in previous example.
  //
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  MetricType::Pointer         metric        = MetricType::New();

  FixedImagePyramidType::Pointer fixedImagePyramid =
      FixedImagePyramidType::New();
  MovingImagePyramidType::Pointer movingImagePyramid =
      MovingImagePyramidType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );
  registration->SetMetric( metric  );
  registration->SetFixedImagePyramid( fixedImagePyramid );
  registration->SetMovingImagePyramid( movingImagePyramid );


  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  fixedImageFile );
  movingImageReader->SetFileName( movingImageFile );


  //  Software Guide : BeginLatex
  //
  //  The fixed and moving images are read from a file. Before connecting
  //  these images to the registration we need to cast them to the internal
  //  image type using \doxygen{CastImageFilters}.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter<
            FixedImageType, InternalImageType >  FixedCastFilterType;
  typedef itk::CastImageFilter<
            MovingImageType, InternalImageType > MovingCastFilterType;

  FixedCastFilterType::Pointer fixedCaster   = FixedCastFilterType::New();
  MovingCastFilterType::Pointer movingCaster = MovingCastFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The output of the readers is connected as input to the cast
  //  filters. The inputs to the registration method are taken from the
  //  cast filters.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  fixedCaster->SetInput(  fixedImageReader->GetOutput() );
  movingCaster->SetInput( movingImageReader->GetOutput() );

  registration->SetFixedImage(    fixedCaster->GetOutput()    );
  registration->SetMovingImage(   movingCaster->GetOutput()   );
  // Software Guide : EndCodeSnippet


  fixedCaster->Update();

  registration->SetFixedImageRegion(
       fixedCaster->GetOutput()->GetBufferedRegion() );


  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y

  registration->SetInitialTransformParameters( initialParameters );

  metric->SetNumberOfHistogramBins( 128 );
  metric->SetNumberOfSpatialSamples( 50000 );

  if( argc > 8 )
    {
    // optionally, override the values with numbers taken from the command line arguments.
    metric->SetNumberOfHistogramBins( numberOfBins );
    }

  if( argc > 9 )
    {
    // optionally, override the values with numbers taken from the command line arguments.
    metric->SetNumberOfSpatialSamples( numberOfSamples );
    }


 //  Software Guide : BeginLatex
  //
  //  Given that the Mattes Mutual Information metric uses a random iterator in
  //  order to collect the samples from the images, it is usually convenient to
  //  initialize the seed of the random number generator.
  //
  //  \index{itk::Mattes\-Mutual\-Information\-Image\-To\-Image\-Metric!ReinitializeSeed()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  metric->ReinitializeSeed( 76926294 );
  // Software Guide : EndCodeSnippet


  if( argc > 7 )
    {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the Transform
    // parameters, or doing it by progressively accumulating contributions from
    // each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives( useExplicitPDFderivatives );
    }


  optimizer->SetNumberOfIterations( 200 );
  optimizer->SetRelaxationFactor( 0.9 );


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  //  Software Guide : BeginLatex
  //
  //  Once all the registration components are in place we can create
  //  an instance of our interface command and connect it to the
  //  registration object using the \code{AddObserver()} method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef RegistrationInterfaceCommand<RegistrationType> CommandType;
  CommandType::Pointer command = CommandType::New();
  registration->AddObserver( itk::IterationEvent(), command );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We set the number of multi-resolution levels to three and trigger the
  //  registration process by calling \code{Update()}.
  //
  //  \index{itk::Multi\-Resolution\-Image\-Registration\-Method!SetNumberOfLevels()}
  //  \index{itk::Multi\-Resolution\-Image\-Registration\-Method!Update()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetNumberOfLevels( 3 );

  try
    {
    registration->Update();
    std::cout << "Optimizer stop condition: "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet

  ParametersType finalParameters = registration->GetLastTransformParameters();

  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];

  unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  double bestValue = optimizer->GetValue();


  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX  << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


  //  Software Guide : BeginLatex
  //
  //  Let's execute this example using the following images
  //
  //  \begin{itemize}
  //  \item BrainT1SliceBorder20.png
  //  \item BrainProtonDensitySliceShifted13x17y.png
  //  \end{itemize}
  //
  //  The output produced by the execution of the method is
  //
  //  \begin{verbatim}
  //  0   -0.419408   [11.0796, 11.5431]
  //  1   -0.775143   [18.0515, 25.9442]
  //  2   -0.621443   [15.2813, 18.4392]
  //  3   -1.00688    [7.81465, 15.567]
  //  4   -0.733843   [11.7844, 16.0582]
  //  5   -1.17593    [15.2929, 17.9792]
  //
  //  0   -0.902265   [13.4257, 17.2627]
  //  1   -1.21519    [11.6959, 16.2588]
  //  2   -1.04207    [12.6029, 16.68]
  //  3   -1.21741    [13.4286, 17.2439]
  //  4   -1.21605    [12.9899, 17.0041]
  //  5   -1.26825    [13.163,  16.8237]
  //
  //  0   -1.25692    [13.0716, 16.909]
  //  1   -1.29465    [12.9896, 17.0033]
  //  2   -1.30922    [13.0513, 16.9934]
  //  3   -1.30722    [13.0205, 16.9987]
  //  4   -1.30978    [12.9897, 17.0039]
  //
  //  Result =
  //   Translation X = 12.9897
  //   Translation Y = 17.0039
  //   Iterations    = 6
  //   Metric value  = -1.30921
  //  \end{verbatim}
  //
  //  These values are a close match to the true misalignment of $(13,17)$
  //  introduced in the moving image.
  //
  //  Software Guide : EndLatex

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );
  finalTransform->SetFixedParameters( transform->GetFixedParameters() );

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();


  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( backgroundGrayLevel );


  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType > CastFilterType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();


  writer->SetFileName( outImagefile );


  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();

  //
  // Generate checkerboards before and after registration
  //
  typedef itk::CheckerBoardImageFilter< FixedImageType > CheckerBoardFilterType;

  CheckerBoardFilterType::Pointer checker = CheckerBoardFilterType::New();

  checker->SetInput1( fixedImage );
  checker->SetInput2( resample->GetOutput() );

  caster->SetInput( checker->GetOutput() );
  writer->SetInput( caster->GetOutput()   );

  resample->SetDefaultPixelValue( 0 );

  // Before registration
  TransformType::Pointer identityTransform = TransformType::New();
  identityTransform->SetIdentity();
  resample->SetTransform( identityTransform );

  for (int q=0; q< argc; ++q)
    {
    std::cout << q << " " << argv[q] << std::endl;
    }
  if( checkerBoardBefore != std::string("") )
    {
    writer->SetFileName( checkerBoardBefore );
    writer->Update();
    }


  // After registration
  resample->SetTransform( finalTransform );
  if( checkerBoardAfter != std::string("") )
    {
    writer->SetFileName( checkerBoardAfter );
    writer->Update();
    }

  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{MultiResImageRegistration1Output}
  // \includegraphics[width=0.32\textwidth]{MultiResImageRegistration1CheckerboardBefore}
  // \includegraphics[width=0.32\textwidth]{MultiResImageRegistration1CheckerboardAfter}
  // \itkcaption[Multi-Resolution registration input images]{Mapped moving image
  // (left) and composition of fixed and moving images before (center) and
  // after (right) registration.}
  // \label{fig:MultiResImageRegistration1Output}
  // \end{figure}
  //
  //  The result of resampling the moving image is presented in the left image
  //  of Figure \ref{fig:MultiResImageRegistration1Output}. The center and
  //  right images of the figure depict a checkerboard composite of the fixed
  //  and moving images before and after registration.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.44\textwidth]{MultiResImageRegistration1TraceTranslations}
  // \includegraphics[height=0.44\textwidth]{MultiResImageRegistration1TraceMetric}
  // \itkcaption[Multi-Resolution registration output images]{Sequence of
  // translations and metric values at each iteration of the optimizer.}
  // \label{fig:MultiResImageRegistration1Trace}
  // \end{figure}
  //
  //  Figure \ref{fig:MultiResImageRegistration1Trace} (left) shows
  //  the sequence of translations followed by the optimizer as it searched
  //  the parameter space. The right side of the same figure shows the
  //  sequence of metric values computed as the optimizer searched the
  //  parameter space.  From the trace, we can see that with the more
  //  aggressive optimization parameters we get quite close to the optimal
  //  value within 4 iterations with the remaining iterations just doing fine
  //  adjustments. It is interesting to compare these results with the ones
  //  of the single resolution example in Section
  //  \ref{sec:MultiModalityRegistrationMattes}, where 24 iterations were
  //  required as more conservative optimization parameters had to be used.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
