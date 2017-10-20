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
// Mattes~\emph{et. al}~\cite{Mattes2003}. One of the main differences between
// \doxygen{MattesMutualInformationImageToImageMetric} and
// \doxygen{MutualInformationImageToImageMetric} is that only one spatial
// sample set is used for the whole registration process instead of using new
// samples every iteration. The use of a single sample set results in a much
// smoother cost function and hence allows the use of more intelligent
// optimizers. In this example, we will use the
// RegularStepGradientDescentOptimizer.  Another noticeable difference is that
// pre-normalization of the images is not necessary as the metric rescales
// internally when building up the discrete density functions.  Other
// differences between the two mutual information implementations are described
// in detail in Section \ref{sec:MutualInformationMetric}.
//
// First, we include the header files of the components used in this example.
//
// \index{itk::ImageRegistrationMethod!Multi-Modality}
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
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
  typedef itk::RegularStepGradientDescentOptimizer OptimizerType;
  typedef   const OptimizerType *                  OptimizerPointer;

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
  typedef  unsigned short  PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType    > RegistrationType;

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
  typedef itk::MattesMutualInformationImageToImageMetric<
                                          FixedImageType,
                                          MovingImageType >    MetricType;
  // Software Guide : EndCodeSnippet

  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );


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
  //  The metric requires two parameters to be selected: the number of bins
  //  used to compute the entropy and the number of spatial samples used to
  //  compute the density estimates. In typical application 50 histogram bins
  //  are sufficient. Note however, that the number of bins may have dramatic
  //  effects on the optimizer's behavior. The number of spatial samples to be
  //  used depends on the content of the image. If the images are smooth and do
  //  not contain much detail, then using approximately $1$ percent of the
  //  pixels will do. On the other hand, if the images are detailed, it may be
  //  necessary to use a much higher proportion, such as $20$ percent.
  //
  //  \index{itk::Mattes\-Mutual\-Information\-Image\-To\-Image\-Metric!SetNumberOfHistogramBins()}
  //  \index{itk::Mattes\-Mutual\-Information\-Image\-To\-Image\-Metric!SetNumberOfSpatialSamples()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int numberOfBins = 24;
  unsigned int numberOfSamples = 10000;
  // Software Guide : EndCodeSnippet

  if( argc > 7 )
    {
    numberOfBins = atoi( argv[7] );
    }

  if( argc > 8 )
    {
    numberOfSamples = atoi( argv[8] );
    }


  // Software Guide : BeginCodeSnippet
  metric->SetNumberOfHistogramBins( numberOfBins );
  metric->SetNumberOfSpatialSamples( numberOfSamples );
  // Software Guide : EndCodeSnippet

  // For consistent results when regression testing.
  metric->ReinitializeSeed( 121212 );


  // Software Guide : BeginLatex
  //
  // One mechanism for bringing the Metric to its limit is to disable the
  // sampling and use all the pixels present in the FixedImageRegion. This can
  // be done with the \code{UseAllPixelsOn()} method. You may want to try this
  // option only while you are fine tuning all other parameters of your
  // registration. We don't use this method in this current example though.
  //
  //  \index{itk::Mattes\-Mutual\-Information\-Image\-To\-Image\-Metric!UseAllPixelsOn()}
  //
  // Software Guide : EndLatex


  if( argc > 9 )
    {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the Transform
    // parameters, or doing it by progressively accumulating contributions from
    // each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives( atoi( argv[9] ) );
    }

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  fixedImageReader->Update();

  registration->SetFixedImageRegion(
       fixedImageReader->GetOutput()->GetBufferedRegion() );


  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y

  registration->SetInitialTransformParameters( initialParameters );


  //  Software Guide : BeginLatex
  //
  //  Another significant difference in the metric is that it computes the
  //  negative mutual information and hence we need to minimize the cost
  //  function in this case. In this example we will use the same optimization
  //  parameters as in Section \ref{sec:IntroductionImageRegistration}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->MinimizeOn();
  optimizer->SetMaximumStepLength( 2.00 );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 200 );
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

  ParametersType finalParameters = registration->GetLastTransformParameters();

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
  std::cout << " Stop Condition  = " << optimizer->GetStopCondition() << std::endl;


  //  Software Guide : BeginLatex
  //
  //  This example is executed using the same multi-modality images as the one
  //  in section~\ref{sec:MultiModalityRegistrationViolaWells} The registration
  //  converges after $59$ iterations and produces the following results:
  //
  //  \begin{verbatim}
  //  Translation X = 13.0283
  //  Translation Y = 17.007
  //  \end{verbatim}
  //
  //  These values are a very close match to the true misalignment introduced in
  //  the moving image.
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
  // \itkcaption[MattesMutualInformationImageToImageMetric output images]{The mapped
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
  resample->SetTransform( finalTransform );
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
  // \itkcaption[MattesMutualInformationImageToImageMetric output plots]{Sequence
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
  //  produced by MattesMutualInformationImageToImageMetric are smoother than
  //  those of the MutualInformationImageToImageMetric. This smoothness allows
  //  the use of more sophisticated optimizers such as the
  //  \doxygen{RegularStepGradientDescentOptimizer} which efficiently locks
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
  // \itkcaption[MattesMutualInformationImageToImageMetric number of
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
  //  in the \code{ITKSoftwareGuide} Git repository under the directory
  //
  //  ~\code{ITKSoftwareGuide/SoftwareGuide/Art}.
  //
  //  The use of these scripts was similar to what was described at the end of
  //  section~\ref{sec:MultiModalityRegistrationViolaWells}.
  //
  // Software Guide : EndLatex


  return EXIT_SUCCESS;
}
