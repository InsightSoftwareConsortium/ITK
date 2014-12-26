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
// \index{itk::ImageRegistrationMethodv4!Multi-Resolution}
// \index{itk::ImageRegistrationMethodv4!Multi-Modality}
//
// This example illustrates the use of the
// \doxygen{ImageRegistrationMethodv4} to solve a simple
// multi-modality registration problem by a multi-resolution approach.
// Since ITKv4 registration method is designed based on a multi-resolution
// structure, a separate set of classes are no longer required to run
// the registration process of this example.
//
// This a great advantage over the previous versions of ITK, as
// in ITKv3 we had to use a different filter
// (\doxygen{MultiResolutionImageRegistrationMethod})
// to run a multi-resolution process. Also, we had to use image pyramids filters
// (\doxygen{MultiResolutionPyramidImageFilter}) for creating the sequence of
// downsampled images. Hence, you can see how ITKv4 framework is
// more user-friendly in more complex situations.
//
// To begin the example, we include the headers of the registration
// components we will use.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethodv4.h"
#include "itkTranslationTransform.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkRegularStepGradientDescentOptimizerv4.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkCheckerBoardImageFilter.h"


// Software Guide : BeginLatex
//
// The ImageRegistrationMethodv4 solves a registration
// problem in a coarse-to-fine manner as illustrated in Figure
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
//
// Tweaking the components between resolution levels can be done using ITK's
// implementation of the \emph{Command/Observer} design pattern. Before
// beginning registration at each resolution level,
// where ImageRegistrationMethodv4 invokes a
// \code{MultiResolutionIterationEvent()}. The registration components can
// be changed by implementing a \doxygen{Command} which responds to the
// event. A brief description of the interaction between events and commands was
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
  typedef   TRegistration      RegistrationType;
  typedef   RegistrationType * RegistrationPointer;
  typedef   itk::RegularStepGradientDescentOptimizerv4<double>  OptimizerType;
  typedef   OptimizerType * OptimizerPointer;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Two arguments are passed to the \code{Execute()} method: the first
  // is the pointer to the object which invoked the event and the
  // second is the event that was invoked.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  void Execute( itk::Object * object,
                const itk::EventObject & event) ITK_OVERRIDE
    {
    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    //
    // First we verify that the event invoked is of the right type,
    // \code{itk::MultiResolutionIterationEvent()}.
    // If not, we return without any further action.
    //
    // Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    if( !(itk::MultiResolutionIterationEvent().CheckEvent( &event ) ) )
      {
      return;
      }
    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    //
    // We then convert the input object pointer to a RegistrationPointer.
    // Note that no error checking is done here to verify the
    // \code{dynamic\_cast} was successful since we know the actual object
    // is a registration method. Then we ask for the optimizer object
    // from the registration method.
    //
    // Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    RegistrationPointer registration =
      static_cast<RegistrationPointer>( object );
    OptimizerPointer optimizer =  static_cast< OptimizerPointer >(
        registration->GetModifiableOptimizer() );
    // Software Guide : EndCodeSnippet

    unsigned int currentLevel = registration->GetCurrentLevel();
    typename RegistrationType::ShrinkFactorsPerDimensionContainerType shrinkFactors =
      registration->GetShrinkFactorsPerDimension( currentLevel );
    typename RegistrationType::SmoothingSigmasArrayType smoothingSigmas =
      registration->GetSmoothingSigmasPerLevel();

    std::cout << "-------------------------------------" << std::endl;
    std::cout << " Current level = " << currentLevel << std::endl;
    std::cout << "    shrink factor = " << shrinkFactors << std::endl;
    std::cout << "    smoothing sigma = " << smoothingSigmas[currentLevel] << std::endl;
    std::cout << std::endl;

    // Software Guide : BeginLatex
    //
    // If this is the first resolution level we set the learning rate
    // (representing the first step size) and the minimum step length (representing
    // the convergence criterion) to large values.  At each subsequent resolution
    // level, we will reduce the minimum step length by a factor of 5 in order to
    // allow the optimizer to focus on progressively smaller regions. The learning
    // rate is set up to the current step length. In this way, when the
    // optimizer is reinitialized at the beginning of the registration process for
    // the next level, the step length will simply start with the last value used
    // for the previous level. This will guarantee the continuity of the path
    // taken by the optimizer through the parameter space.
    //
    // Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    if ( registration->GetCurrentLevel() == 0 )
      {
      optimizer->SetLearningRate( 16.00 );
      optimizer->SetMinimumStepLength( 2.5 );
      }
    else
      {
      optimizer->SetLearningRate( optimizer->GetCurrentStepLength() );
      optimizer->SetMinimumStepLength(
        optimizer->GetMinimumStepLength() * 0.2 );
      }
    // Software Guide : EndCodeSnippet
    }

  // Software Guide : BeginLatex
  //
  // Another version of the \code{Execute()} method accepting a \code{const}
  // input object is also required since this method is defined as pure virtual
  // in the base class.  This version simply returns without taking any action.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  void Execute(const itk::Object * , const itk::EventObject & ) ITK_OVERRIDE
    {
    return;
    }
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
  CommandIterationUpdate(): m_CumulativeIterationIndex(0) {};

public:
  typedef   itk::RegularStepGradientDescentOptimizerv4<double>  OptimizerType;
  typedef   const OptimizerType *                               OptimizerPointer;

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
  std::cout << optimizer->GetCurrentPosition() << "   ";
  std::cout << m_CumulativeIterationIndex++ << std::endl;
  }
private:
  unsigned int m_CumulativeIterationIndex;
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
    std::cerr << " [numberOfBins] " << std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;

  const std::string fixedImageFile  = argv[1];
  const std::string movingImageFile = argv[2];
  const std::string outImagefile    = argv[3];
  const PixelType backgroundGrayLevel  = (argc >4 )? atoi(argv[4]): 100;
  const std::string checkerBoardBefore = (argc >5 )?      argv[5]: "";
  const std::string checkerBoardAfter  = (argc >6 )?      argv[6]: "";
  const int numberOfBins               = (argc >7 )? atoi(argv[7]): 0;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  //  Software Guide : BeginLatex
  //
  //  The fixed and moving image types are defined as in previous
  //  examples. The downsampled images for different resolution levels
  //  are created internally by the registration method based on the
  //  values provided for \emph{ShrinkFactor} and \emph{SmoothingSigma}
  //  vectors.
  //
  //  The types for the registration components are then derived using
  //  the fixed and moving image type, as in previous examples.
  //
  //  Software Guide : EndLatex

  typedef itk::TranslationTransform< double, Dimension >              TransformType;

  typedef itk::RegularStepGradientDescentOptimizerv4<double>          OptimizerType;

  typedef itk::MattesMutualInformationImageToImageMetricv4<
                                                    FixedImageType,
                                                    MovingImageType > MetricType;
  typedef itk::ImageRegistrationMethodv4<
                                      FixedImageType,
                                      MovingImageType,
                                      TransformType >                 RegistrationType;

  //  All the components are instantiated using their \code{New()} method
  //  and connected to the registration object as in previous example.
  //
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  MetricType::Pointer         metric        = MetricType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetOptimizer( optimizer );
  registration->SetMetric( metric  );

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  =
                                            FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader =
                                            MovingImageReaderType::New();

  fixedImageReader->SetFileName(  fixedImageFile );
  movingImageReader->SetFileName( movingImageFile );

  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );


  typedef OptimizerType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y

  transform->SetParameters( initialParameters );

  registration->SetInitialTransform( transform );
  registration->InPlaceOn();

  metric->SetNumberOfHistogramBins( 24 );

  if( argc > 7 )
    {
    // optionally, override the values with numbers taken from the command line arguments.
    metric->SetNumberOfHistogramBins( numberOfBins );
    }

  //  Software Guide : BeginLatex
  //
  //  To set the optimizer parameters, note that \emph{LearningRate}
  //  and \emph{MinimumStepLength} are set in the obsever at the begining
  //  of each resolution level. The other optimizer parameters are set
  //  as follows.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  optimizer->SetNumberOfIterations( 200 );
  optimizer->SetRelaxationFactor( 0.5 );
  // Software Guide : EndCodeSnippet

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  //  Software Guide : BeginLatex
  //
  //  We set the number of multi-resolution levels to three and set
  //  the corresponding shrink factor and smoothing sigma values for each
  //  resolution level. Using smoothing in the subsampled images in
  //  low-resolution levels can avoid large fluctuations in the
  //  metric function, which prevents the optimizer from becoming trapped in
  //  local minima. In this simple example we have no smoothing, and we have
  //  used small shrinkings for the first two resolution levels.
  //
  //  \index{itk::Image\-Registration\-Methodv4!SetNumberOfLevels()}
  //  \index{itk::Image\-Registration\-Methodv4!SetShrinkFactorsPerLevel()}
  //  \index{itk::Image\-Registration\-Methodv4!SetSmoothingSigmasPerLevel()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfLevels = 3;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 3 );
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel[0] = 0;
  smoothingSigmasPerLevel[1] = 0;
  smoothingSigmasPerLevel[2] = 0;

  registration->SetNumberOfLevels ( numberOfLevels );
  registration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );
  registration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  // Software Guide : EndCodeSnippet

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

  registration->AddObserver( itk::MultiResolutionIterationEvent(), command );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then we trigger the registration process by calling \code{Update()}.
  //
  //  Software Guide : EndLatex

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

  ParametersType finalParameters = transform->GetParameters();

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
  //  0   -0.316956   [11.4200, 11.2063]
  //  1   -0.562048   [18.2938, 25.6545]
  //  2   -0.407696   [11.3643, 21.6569]
  //  3   -0.5702     [13.7244, 18.4274]
  //  4   -0.803252   [11.1634, 15.3547]
  //
  //  0   -0.697586   [12.8778, 16.3846]
  //  1   -0.901984   [13.1794, 18.3617]
  //  2   -0.827423   [13.0545, 17.3695]
  //  3   -0.92754    [12.8528, 16.3901]
  //  4   -0.902671   [12.9426, 16.8819]
  //  5   -0.941212   [13.1402, 17.3413]
  //
  //  0   -0.922239   [13.0364, 17.1138]
  //  1   -0.930203   [12.9463, 16.8806]
  //  2   -0.930959   [13.0191, 16.9822]
  //
  //
  //  Result =
  //   Translation X = 13.0192
  //   Translation Y = 16.9823
  //   Iterations    = 4
  //   Metric value  = -0.929237
  //  \end{verbatim}
  //
  //  These values are a close match to the true misalignment of $(13,17)$
  //  introduced in the moving image.
  //
  //  Software Guide : EndLatex

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( transform );
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
  resample->SetTransform( transform );
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
  //  value within 5 iterations with the remaining iterations just doing fine
  //  adjustments. It is interesting to compare these results with those
  //  of the single resolution example in Section
  //  \ref{sec:MultiModalityRegistrationMattes}, where 46 iterations were
  //  required as more conservative optimization parameters had to be used.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
