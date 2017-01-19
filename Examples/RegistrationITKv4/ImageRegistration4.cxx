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
// In this example, we will solve a simple multi-modality problem using an
// implementation of mutual information. This implementation was published by
// Mattes~\emph{et. al}~\cite{Mattes2003}.
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
#include "itkMersenneTwisterRandomVariateGenerator.h"


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

  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
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
  //  except the metric, are declared as in Section \ref{sec:IntroductionImageRegistration}.
  //  The Mattes mutual information metric type is instantiated using the image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::MattesMutualInformationImageToImageMetricv4<
    FixedImageType,
    MovingImageType > MetricType;
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
  //  The metric requires the user to specify the number of bins
  //  used to compute the entropy. In a typical application, 50 histogram bins
  //  are sufficient. Note however, that the number of bins may have dramatic
  //  effects on the optimizer's behavior.
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
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  To calculate the image gradients, an image gradient calculator based on
  //  ImageFunction is used instead of image gradient filters. Image gradient
  //  methods are defined in the superclass \code{ImageToImageMetricv4}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
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
  //  Notice that in the ITKv4 registration framework, optimizers always try
  //  to minimize the cost function, and the metrics always return a parameter
  //  and derivative result that improves the optimization, so this metric
  //  computes the negative mutual information.
  //  The optimization parameters are tuned for this example, so they are not
  //  exactly the same as the parameters used in Section
  //  \ref{sec:IntroductionImageRegistration}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetLearningRate( 8.00 );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 200 );
  optimizer->ReturnBestParametersAndValueOn();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Note that large values of the learning rate will make the optimizer
  // unstable. Small values, on the other hand, may result in the optimizer
  // needing too many iterations in order to walk to the extrema of the cost
  // function. The easy way of fine tuning this parameter is to start with
  // small values, probably in the range of $\{1.0,5.0\}$. Once the other
  // registration parameters have been tuned for producing convergence, you
  // may want to revisit the learning rate and start increasing its value until
  // you observe that the optimization becomes unstable.  The ideal value for
  // this parameter is the one that results in a minimum number of iterations
  // while still keeping a stable path on the parametric space of the
  // optimization. Keep in mind that this parameter is a multiplicative factor
  // applied on the gradient of the metric. Therefore, its effect on the
  // optimizer step length is proportional to the metric values themselves.
  // Metrics with large values will require you to use smaller values for the
  // learning rate in order to maintain a similar optimizer behavior.
  //
  // Whenever the regular step gradient descent optimizer encounters
  // change in the direction of movement in the parametric space, it reduces the
  // size of the step length. The rate at which the step length is reduced is
  // controlled by a relaxation factor. The default value of the factor is
  // $0.5$. This value, however may prove to be inadequate for noisy metrics
  // since they tend to induce erratic movements on the optimizers and
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

  // Software Guide : BeginLatex
  //
  // Instead of using the whole virtual domain (usually fixed image domain) for the registration,
  // we can use a spatial sampled point set by supplying an arbitrary point list over which to
  // evaluate the metric. The point list is expected to be in the \emph{fixed} image domain, and
  // the points are transformed into the \emph{virtual} domain internally as needed. The user can
  // define the point set via \code{SetFixedSampledPointSet()}, and the point set is used
  // by calling \code{SetUsedFixedSampledPointSet()}.
  //
  // Also, instead of dealing with the metric directly, the user may define
  // the sampling percentage and sampling strategy for the registration framework at each level.
  // In this case, the registration filter manages the sampling operation over the fixed image space
  // based on the input strategy (REGULAR, RANDOM) and passes the sampled point set to the metric
  // internally.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  RegistrationType::MetricSamplingStrategyType  samplingStrategy  =
    RegistrationType::RANDOM;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The number of spatial samples to be
  // used depends on the content of the image. If the images are smooth and do
  // not contain many details, the number of spatial samples can usually be as low as $1\%$
  // of the total number of pixels in the fixed image. On the other hand, if the images are
  // detailed, it may be necessary to use a much higher proportion, such as $20\%$ to $50\%$.
  // Increasing the number of samples improves the smoothness of the metric,
  // and therefore helps when this metric is used in conjunction with
  // optimizers that rely of the continuity of the metric values. The trade-off, of
  // course, is that a larger number of samples results in longer computation
  // times per every evaluation of the metric.
  //
  // One mechanism for bringing the metric to its limit is to disable the
  // sampling and use all the pixels present in the FixedImageRegion. This can
  // be done with the \code{SetUseFixedSampledPointSet( false )} method.
  // You may want to try this
  // option only while you are fine tuning all other parameters of your
  // registration. We don't use this method in this current example though.
  //
  // It has been demonstrated empirically that the number of samples is not a
  // critical parameter for the registration process. When you start fine
  // tuning your own registration process, you should start using high values
  // of number of samples, for example in the range of $20\%$ to $50\%$ of the
  // number of pixels in the fixed image. Once you have succeeded to register
  // your images you can then reduce the number of samples progressively until
  // you find a good compromise on the time it takes to compute one evaluation
  // of the metric. Note that it is not useful to have very fast evaluations
  // of the metric if the noise in their values results in more iterations
  // being required by the optimizer to converge. You must then study the
  // behavior of the metric values as the iterations progress, just as
  // illustrated in section~\ref{sec:MonitoringImageRegistration}.
  //
  // \index{itk::Mutual\-Information\-Image\-To\-Image\-Metricv4!Trade-offs}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double samplingPercentage = 0.20;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // In ITKv4, a single virtual domain or spatial sample point set is used for the
  // all iterations of the registration process. The use of a single sample set results
  // in a smooth cost function that can improve the functionality of
  // the optimizer.
  //
  // The spatial point set is pseudo randomly generated. For
  // reproducible results an integer seed should set.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetMetricSamplingStrategy( samplingStrategy );
  registration->SetMetricSamplingPercentage( samplingPercentage );

  registration->MetricSamplingReinitializeSeed( 121213 );
  // Software Guide : EndCodeSnippet

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
  //  Let's execute this example over two of the images provided in
  //  \code{Examples/Data}:
  //
  //  \begin{itemize}
  //  \item \code{BrainT1SliceBorder20.png}
  //  \item \code{BrainProtonDensitySliceShifted13x17y.png}
  //  \end{itemize}
  //
  //  \begin{figure}
  //  \center
  //  \includegraphics[width=0.44\textwidth]{BrainT1SliceBorder20}
  //  \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceShifted13x17y}
  //  \itkcaption[Multi-Modality Registration Inputs]{A T1 MRI (fixed image) and a proton
  //  density MRI (moving image) are provided as input to the registration method.}
  //  \label{fig:FixedMovingImageRegistration2}
  //  \end{figure}
  //
  //  The second image is the result of intentionally translating the image
  //  \code{Brain\-Proton\-Density\-Slice\-Border20.png} by $(13,17)$
  //  millimeters. Both images have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration2}. The registration process
  //  converges after $46$ iterations and produces the following results:
  //
  //  \begin{verbatim}
  //  Translation X = 13.0204
  //  Translation Y = 17.0006
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
  // \includegraphics[width=0.6\textwidth,height=5cm]{ImageRegistration4TraceMetric}
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
  //  optimizer searched the parameter space.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginLatex
  //
  // You must note however that there are a number of non-trivial issues
  // involved in the fine tuning of parameters for the optimization. For
  // example, the number of bins used in the estimation of Mutual Information
  // has a dramatic effect on the performance of the optimizer. In order to
  // illustrate this effect, the same example has been executed using a range
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
  //
  // Effects such as the one illustrated here highlight how useless is to
  // compare different algorithms based on a non-exhaustive search of their
  // parameter setting. It is quite difficult to be able to claim that a
  // particular selection of parameters represent the best combination for
  // running a particular algorithm. Therefore, when comparing the performance
  // of two or more different algorithms, we are faced with the challenge of
  // proving that none of the algorithms involved in the comparison are being
  // run with a sub-optimal set of parameters.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginLatex
  //
  //  The plots in Figures~\ref{fig:ImageRegistration4TraceTranslations}
  //  and~\ref{fig:ImageRegistration4TraceTranslationsNumberOfBins} were
  //  generated using Gnuplot\footnote{\url{http://www.gnuplot.info/}}.
  //  The scripts used for this purpose are available
  //  in the \code{ITKSoftwareGuide} Git repository under the directory
  //
  //  ~\code{ITKSoftwareGuide/SoftwareGuide/Art}.
  //
  //  Data for the plots were taken directly from the output that the
  //  Command/Observer in this example prints out to the console. The output
  //  was processed with the UNIX editor
  //  \code{sed}\footnote{\url{http://www.gnu.org/software/sed/sed.html}} in
  //  order to remove commas and brackets that were confusing for Gnuplot's
  //  parser. Both the shell script for running \code{sed} and for running
  //  {Gnuplot} are available in the directory indicated above. You may find
  //  useful to run them in order to verify the results presented here, and to
  //  eventually modify them for profiling your own registrations.
  //
  //  \index{Open Science}
  //
  //  Open Science is not just an abstract concept. Open Science is something
  //  to be practiced every day with the simple gesture of sharing information
  //  with your peers, and by providing all the tools that they need for
  //  replicating the results that you are reporting. In Open Science, the only
  //  bad results are those that can not be
  //  replicated\footnote{\url{http://science.creativecommons.org/}}. Science
  //  is dead when people blindly trust authorities~\footnote{For example:
  //  Reviewers of Scientific Journals.} instead of verifying their statements
  //  by performing their own experiments ~\cite{Popper1971,Popper2002}.
  //
  // Software Guide : EndLatex


  return EXIT_SUCCESS;
}
