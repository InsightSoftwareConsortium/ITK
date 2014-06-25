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
//    OUTPUTS: {ImageRegistration4Output.png}
//    ARGUMENTS:    100
//    OUTPUTS: {ImageRegistration4CheckerboardBefore.png}
//    OUTPUTS: {ImageRegistration4CheckerboardAfter.png}
//    ARGUMENTS:    24
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// In this example, we will solve a simple multi-modality problem using another
// implementation of mutual information. This implementation was published by
// Mattes~\emph{et. al}~\cite{Mattes2003}.
//
// Instead of using the whole virtual domain (usually fixed image domain) for the registration,
// we can use a spatial sample set by supplying an arbitrary point list over which to
// evaluate the metric. The point list is expected to be in the fixed image domain, and
// the points are transformed into the virtual domain internally as needed. User can
// define the point set via "SetFixedSampledPointSet", and the point set is enabled to use
// by calling "SetUsedFixedSampledPointSet".
//
// A single virtual domain or spatial sample set is used for the whole registration
// process. The use of a single sample set results in a smooth cost function
// and hence allows the use of intelligent optimizers. In this example, we will
// use the \doxygen{RegularStepGradientDescentOptimizerv4}.
//
// Also, notice that pre-normalization of the images is not necessary in this example
// as the metric rescales internally when building up the discrete density functions.
//
// First, we include the header files of the components used in this example.
//
// \index{itk::ImageRegistrationMethodv4!Multi-Modality}
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
  typedef itk::RegularStepGradientDescentOptimizerv4<double> OptimizerType;
  typedef   const OptimizerType *                            OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
    OptimizerPointer optimizer =
      dynamic_cast< OptimizerPointer >( object );
    if( ! itk::IterationEvent().CheckEvent( &event ) )
      {
      return;
      }
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << optimizer->GetCurrentPosition() << std::endl;
    }
};

int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile [defaultPixelValue]" << std::endl;
    std::cerr << "[checkerBoardAfter] [checkerBoardBefore]" << std::endl;
    std::cerr << "[numberOfBins] [numberOfSamples]";
    std::cerr << "[useExplicitPDFderivatives ] " << std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::TranslationTransform< double, Dimension >         TransformType;
  typedef itk::RegularStepGradientDescentOptimizerv4<double>     OptimizerType;
  typedef itk::ImageRegistrationMethodv4<
                                    FixedImageType,
                                    MovingImageType,
                                    TransformType    > RegistrationType;

  //  Software Guide : BeginLatex
  //
  //  In this example the image types and all registration components,
  //  except the metric, are declared as in Section
  //  \ref{sec:IntroductionImageRegistration}.
  //  The Mattes mutual information metric type is
  //  instantiated using the image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::MattesMutualInformationImageToImageMetricv4<
                                                    FixedImageType,
                                                    MovingImageType >    MetricType;
  // Software Guide : EndCodeSnippet

  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetOptimizer(     optimizer     );


  //  Software Guide : BeginLatex
  //
  //  The metric is created using the \code{New()} method and then
  //  connected to the registration object.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MetricType::Pointer metric = MetricType::New();
  registration->SetMetric( metric  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The metric requires one parameter to be selected: the number of bins
  //  used to compute the entropy. In typical application 50 histogram bins
  //  are sufficient. Note however, that the number of bins may have dramatic
  //  effects on the optimizer's behavior.
  //  In this example the whole virtual image domain is used rather than just a
  //  a sampled point set.
  //  To calculate the image gradients, an image gradient calculator based on
  //  ImageFunction is used instead of image gradient filters. Image gradient
  //  methods are defined in the super class \index{ImageToImageMetricv4}.
  //
  //  \index{itk::Mattes\-Mutual\-Information\-Image\-To\-Image\-Metricv4!SetNumberOfHistogramBins()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int numberOfBins = 24;
  // Software Guide : EndCodeSnippet

  if( argc > 7 )
    {
    numberOfBins = atoi( argv[7] );
    }


  // Software Guide : BeginCodeSnippet
  metric->SetNumberOfHistogramBins( numberOfBins );
  metric->SetUseFixedSampledPointSet( false );
  metric->SetUseMovingImageGradientFilter( false );
  metric->SetUseFixedImageGradientFilter( false );
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );


  //  Software Guide : BeginLatex
  //
  //  Notice that in ITKv4 registration framework, optimizers always try
  //  to minimize the cost function, and the metrics always return a parameter
  //  and derivative result that improves the optimization, so this metric
  //  computes the negative mutual information.
  //  The optimization parameters are tuned for this example, so they are not
  //  exactly the same as the parameters used in Section
  //  \ref{sec:IntroductionImageRegistration}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetLearningRate( 2.00 );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 200 );
  optimizer->ReturnBestParametersAndValueOn();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Whenever the regular step gradient descent optimizer encounters that the
  // direction of movement has changed in the parametric space, it reduces the
  // size of the step length. The rate at which the step length is reduced is
  // controlled by a relaxation factor. The default value of the factor is
  // $0.5$. This value, however may prove to be inadequate for noisy metrics
  // since they tend to induce very erratic movements on the optimizers and
  // therefore result in many directional changes. In those
  // conditions, the optimizer will rapidly shrink the step length while it is
  // still too far from the location of the extrema in the cost function. In
  // this example we set the relaxation factor to a number higher than the
  // default in order to prevent the premature shrinkage of the step length.
  //
  // \index{itk::Regular\-Step\-Gradient\-Descent\-Optimizer!SetRelaxationFactor()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetRelaxationFactor( 0.8 );
  // Software Guide : EndCodeSnippet

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  // One level registration process without shrinking and smoothing.
  //
  const unsigned int numberOfLevels = 1;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 1 );
  shrinkFactorsPerLevel[0] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 1 );
  smoothingSigmasPerLevel[0] = 0;

  registration->SetNumberOfLevels ( numberOfLevels );
  registration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  registration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );

  try
    {
    registration->Update();
    std::cout << "Optimizer stop condition: "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  TransformType::ParametersType finalParameters =
                            registration->GetOutput()->Get()->GetParameters();

  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];

  // For stability reasons it may be desirable to round up the values of translation
  //
  unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  double bestValue = optimizer->GetValue();


  // Print out results
  //
  std::cout << std::endl;
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX  << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;
  std::cout << " Stop Condition  = " << optimizer->GetStopConditionDescription() << std::endl;

  //  Software Guide : BeginLatex
  //
  //  This example is executed using the same multi-modality images as the one
  //  in section~\ref{sec:MultiModalityRegistrationViolaWells} The registration
  //  converges after $40$ iterations and produces the following results:
  //
  //  \begin{verbatim}
  //  Translation X = 13.0153
  //  Translation Y = 17.0798
  //  \end{verbatim}
  //
  //  These values are a very close match to the true misalignment introduced in
  //  the moving image.
  //
  //  Software Guide : EndLatex

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( registration->GetTransform() );
  resample->SetInput( movingImageReader->GetOutput() );

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  PixelType defaultPixelValue = 100;

  if( argc > 4 )
    {
    defaultPixelValue = atoi( argv[4] );
    }

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( defaultPixelValue );


  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType > CastFilterType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();

  writer->SetFileName( argv[3] );

  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration4Output}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration4CheckerboardBefore}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration4CheckerboardAfter}
  // \itkcaption[MattesMutualInformationImageToImageMetricv4 output images]{The mapped
  // moving image (left) and the composition of fixed and moving images before
  // (center) and after (right) registration with Mattes mutual information.}
  // \label{fig:ImageRegistration4Output}
  // \end{figure}
  //
  //  The result of resampling the moving image is presented on the left of
  //  Figure \ref{fig:ImageRegistration4Output}. The center and right parts of
  //  the figure present a checkerboard composite of the fixed and moving
  //  images before and after registration respectively.
  //
  //  Software Guide : EndLatex


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

  if( argc > 5 )
    {
    writer->SetFileName( argv[5] );
    writer->Update();
    }


  // After registration
  resample->SetTransform( registration->GetTransform() );
  if( argc > 6 )
    {
    writer->SetFileName( argv[6] );
    writer->Update();
    }


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{ImageRegistration4TraceTranslations}
  // \includegraphics[width=0.44\textwidth]{ImageRegistration4TraceTranslations2}
  // \includegraphics[width=0.6\textwidth]{ImageRegistration4TraceMetric}
  // \itkcaption[MattesMutualInformationImageToImageMetricv4 output plots]{Sequence
  // of translations and metric values at each iteration of the optimizer.}
  // \label{fig:ImageRegistration4TraceTranslations}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration4TraceTranslations} (upper-left) shows
  //  the sequence of translations followed by the optimizer as it searched the
  //  parameter space. The upper-right figure presents a closer look at the
  //  convergence basin for the last iterations of the optimizer. The bottom of
  //  the same figure shows the sequence of metric values computed as the
  //  optimizer searched the parameter space.  Comparing these trace plots with
  //  Figures \ref{fig:ImageRegistration2TraceTranslations} and
  //  \ref{fig:ImageRegistration2TraceMetric}, we can see that the measures
  //  produced by MattesMutualInformationImageToImageMetricv4 are smoother than
  //  those of the MutualInformationImageToImageMetric. This smoothness allows
  //  the use of more sophisticated optimizers such as the
  //  \doxygen{RegularStepGradientDescentOptimizerv4} which efficiently locks
  //  onto the optimal value.
  //
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginLatex
  //
  // You must note however that there are a number of non-trivial issues
  // involved in the fine tuning of parameters for the optimization. For
  // example, the number of bins used in the estimation of Mutual Information
  // has a dramatic effect on the performance of the optimizer. In order to
  // illustrate this effect, this same example has been executed using a range
  // of different values for the number of bins, from $10$ to $30$. If you
  // repeat this experiment, you will notice that depending on the number of
  // bins used, the optimizer's path may get trapped early on in local minima.
  // Figure \ref{fig:ImageRegistration4TraceTranslationsNumberOfBins} shows the
  // multiple paths that the optimizer took in the parametric space of the
  // transform as a result of different selections on the number of bins used
  // by the Mattes Mutual Information metric. Note that many of the paths die
  // in local minima instead of reaching the extrema value on the upper right
  // corner.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.8\textwidth]{ImageRegistration4TraceTranslationsNumberOfBins}
  // \itkcaption[MattesMutualInformationImageToImageMetricv4 number of
  // bins]{Sensitivity of the optimization path to the number of Bins used for
  // estimating the value of Mutual Information with Mattes et al. approach.}
  // \label{fig:ImageRegistration4TraceTranslationsNumberOfBins}
  // \end{figure}
  //

  // Effects such as the one illustrated here highlight how useless is to
  // compare different algorithms based on a non-exhaustive search of their
  // parameter setting. It is quite difficult to be able to claim that a
  // particular selection of parameters represent the best combination for
  // running a particular algorithm. Therefore, when comparing the performance
  // of two or more different algorithms, we are faced with the challenge of
  // proving that none of the algorithms involved in the comparison is being
  // run with a sub-optimal set of parameters.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginLatex
  //
  //  The plots in Figures~\ref{fig:ImageRegistration4TraceTranslations}
  //  and~\ref{fig:ImageRegistration4TraceTranslationsNumberOfBins} were
  //  generated using Gnuplot. The scripts used for this purpose are available
  //  in the \code{ITKSoftwareGuide} CVS module under the directory
  //
  //  ~\code{SoftwareGuide/Art}
  //
  //  The use of these scripts was similar to what was described at the end of
  //  section~\ref{sec:MultiModalityRegistrationViolaWells}.
  //
  // Software Guide : EndLatex


  return EXIT_SUCCESS;
}
