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
//    OUTPUTS: {ImageRegistration2Output.png}
//    OUTPUTS: {ImageRegistration2CheckerboardBefore.png}
//    OUTPUTS: {ImageRegistration2CheckerboardAfter.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// The following simple example illustrates how multiple imaging modalities can
// be registered using the ITK registration framework. The first difference
// between this and previous examples is the use of the
// \doxygen{MutualInformationImageToImageMetric} as the cost-function to be
// optimized. The second difference is the use of the
// \doxygen{GradientDescentOptimizer}. Due to the stochastic nature of the
// metric computation, the values are too noisy to work successfully with the
// \doxygen{RegularStepGradientDescentOptimizer}.  Therefore, we will use the
// simpler GradientDescentOptimizer with a user defined learning rate.  The
// following headers declare the basic components of this registration method.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkGradientDescentOptimizer.h"
// Software Guide : EndCodeSnippet

#include "itkMersenneTwisterRandomVariateGenerator.h"


//  Software Guide : BeginLatex
//
//  One way to simplify the computation of the mutual information is
//  to normalize the statistical distribution of the two input images. The
//  \doxygen{NormalizeImageFilter} is the perfect tool for this task.
//  It rescales the intensities of the input images in order to produce an
//  output image with zero mean and unit variance. This filter has been
//  discussed in Section \ref{sec:CastingImageFilters}.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkNormalizeImageFilter.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  Additionally, low-pass filtering of the images to be registered will also
//  increase robustness against noise. In this example, we will use the
//  \doxygen{DiscreteGaussianImageFilter} for that purpose. The
//  characteristics of this filter have been discussed in Section
//  \ref{sec:BlurringFilters}.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkDiscreteGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkCheckerBoardImageFilter.h"


//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
#include "itkCommand.h"
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
  typedef   itk::GradientDescentOptimizer     OptimizerType;
  typedef   const OptimizerType   *           OptimizerPointer;

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
    std::cerr << "outputImagefile ";
    std::cerr << "[checkerBoardBefore] [checkerBoardAfter]" << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // The moving and fixed images types should be instantiated first.
  //
  // Software Guide : EndLatex
  //
  // Software Guide : BeginCodeSnippet
  const    unsigned int    Dimension = 2;
  typedef  unsigned short  PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  It is convenient to work with an internal image type because mutual
  //  information will perform better on images with a normalized statistical
  //  distribution. The fixed and moving images will be normalized and
  //  converted to this internal type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   float                                    InternalPixelType;
  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The rest of the image registration components are instantiated as
  //  illustrated in Section \ref{sec:IntroductionImageRegistration} with
  //  the use of the \code{InternalImageType}.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::GradientDescentOptimizer                  OptimizerType;
  typedef itk::LinearInterpolateImageFunction<
                                    InternalImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod<
                                    InternalImageType,
                                    InternalImageType >  RegistrationType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The mutual information metric type is instantiated using the image
  //  types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::MutualInformationImageToImageMetric<
                                          InternalImageType,
                                          InternalImageType >    MetricType;
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
  MetricType::Pointer         metric        = MetricType::New();
  registration->SetMetric( metric  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The metric requires a number of parameters to be selected, including
  //  the standard deviation of the Gaussian kernel for the fixed image
  //  density estimate, the standard deviation of the kernel for the moving
  //  image density and the number of samples use to compute the densities
  //  and entropy values. Details on the concepts behind the computation of
  //  the metric can be found in Section
  //  \ref{sec:MutualInformationMetric}.  Experience has
  //  shown that a kernel standard deviation of $0.4$ works well for images
  //  which have been normalized to a mean of zero and unit variance.  We
  //  will follow this empirical rule in this example.
  //
  //  \index{itk::Mutual\-Information\-Image\-To\-Image\-Metric!SetFixedImageStandardDeviation()}
  //  \index{itk::Mutual\-Information\-Image\-To\-Image\-Metric!SetMovingImageStandardDeviation()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  metric->SetFixedImageStandardDeviation(  0.4 );
  metric->SetMovingImageStandardDeviation( 0.4 );
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );


  //  Software Guide : BeginLatex
  //
  //  The normalization filters are instantiated using the fixed and moving
  //  image types as input and the internal image type as output.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::NormalizeImageFilter<
                                FixedImageType,
                                InternalImageType
                                        > FixedNormalizeFilterType;

  typedef itk::NormalizeImageFilter<
                                MovingImageType,
                                InternalImageType
                                              > MovingNormalizeFilterType;

  FixedNormalizeFilterType::Pointer fixedNormalizer =
                                            FixedNormalizeFilterType::New();

  MovingNormalizeFilterType::Pointer movingNormalizer =
                                            MovingNormalizeFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The blurring filters are declared using the internal image type as both
  //  the input and output types. In this example, we will set the variance
  //  for both blurring filters to $2.0$.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::DiscreteGaussianImageFilter<
                                      InternalImageType,
                                      InternalImageType
                                                    > GaussianFilterType;

  GaussianFilterType::Pointer fixedSmoother  = GaussianFilterType::New();
  GaussianFilterType::Pointer movingSmoother = GaussianFilterType::New();

  fixedSmoother->SetVariance( 2.0 );
  movingSmoother->SetVariance( 2.0 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The output of the readers becomes the input to the normalization
  //  filters. The output of the normalization filters is connected as
  //  input to the blurring filters. The input to the registration method
  //  is taken from the blurring filters.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  fixedNormalizer->SetInput(  fixedImageReader->GetOutput() );
  movingNormalizer->SetInput( movingImageReader->GetOutput() );

  fixedSmoother->SetInput( fixedNormalizer->GetOutput() );
  movingSmoother->SetInput( movingNormalizer->GetOutput() );

  registration->SetFixedImage(    fixedSmoother->GetOutput()    );
  registration->SetMovingImage(   movingSmoother->GetOutput()   );
  // Software Guide : EndCodeSnippet


  fixedNormalizer->Update();
  FixedImageType::RegionType fixedImageRegion =
       fixedNormalizer->GetOutput()->GetBufferedRegion();
  registration->SetFixedImageRegion( fixedImageRegion );

  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y

  registration->SetInitialTransformParameters( initialParameters );

  //  Software Guide : BeginLatex
  //
  //  We should now define the number of spatial samples to be considered in
  //  the metric computation. Note that we were forced to postpone this setting
  //  until we had done the preprocessing of the images because the number of
  //  samples is usually defined as a fraction of the total number of pixels in
  //  the fixed image.
  //
  //  The number of spatial samples can usually be as low as $1\%$ of the total
  //  number of pixels in the fixed image. Increasing the number of samples
  //  improves the smoothness of the metric from one iteration to another and
  //  therefore helps when this metric is used in conjunction with optimizers
  //  that rely of the continuity of the metric values. The trade-off, of
  //  course, is that a larger number of samples result in longer computation
  //  times per every evaluation of the metric.
  //
  //  It has been demonstrated empirically that the number of samples is not a
  //  critical parameter for the registration process. When you start fine
  //  tuning your own registration process, you should start using high values
  //  of number of samples, for example in the range of $20\%$ to $50\%$ of the
  //  number of pixels in the fixed image. Once you have succeeded to register
  //  your images you can then reduce the number of samples progressively until
  //  you find a good compromise on the time it takes to compute one evaluation
  //  of the Metric. Note that it is not useful to have very fast evaluations
  //  of the Metric if the noise in their values results in more iterations
  //  being required by the optimizer to converge. You must then study the
  //  behavior of the metric values as the iterations progress, just as
  //  illustrated in section~\ref{sec:MonitoringImageRegistration}.
  //
  //  \index{itk::Mutual\-Information\-Image\-To\-Image\-Metric!SetNumberOfSpatialSamples()}
  //  \index{itk::Mutual\-Information\-Image\-To\-Image\-Metric!Trade-offs}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfPixels = fixedImageRegion.GetNumberOfPixels();

  const unsigned int numberOfSamples =
                        static_cast< unsigned int >( numberOfPixels * 0.01 );

  metric->SetNumberOfSpatialSamples( numberOfSamples );
  // Software Guide : EndCodeSnippet


  // For consistent results when regression testing.
  metric->ReinitializeSeed(121212);

  //  Software Guide : BeginLatex
  //
  //  Since larger values of mutual information indicate better matches than
  //  smaller values, we need to maximize the cost function in this example.
  //  By default the GradientDescentOptimizer class is set to minimize the
  //  value of the cost-function. It is therefore necessary to modify its
  //  default behavior by invoking the \code{MaximizeOn()} method.
  //  Additionally, we need to define the optimizer's step size using the
  //  \code{SetLearningRate()} method.
  //
  //  \index{itk::Gradient\-Descent\-Optimizer!MaximizeOn()}
  //  \index{itk::Image\-Registration\-Method!Maximize vs Minimize}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  optimizer->SetLearningRate( 15.0 );
  optimizer->SetNumberOfIterations( 200 );
  optimizer->MaximizeOn();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Note that large values of the learning rate will make the optimizer
  // unstable. Small values, on the other hand, may result in the optimizer
  // needing too many iterations in order to walk to the extrema of the cost
  // function. The easy way of fine tuning this parameter is to start with
  // small values, probably in the range of $\{5.0,10.0\}$. Once the other
  // registration parameters have been tuned for producing convergence, you
  // may want to revisit the learning rate and start increasing its value until
  // you observe that the optimization becomes unstable.  The ideal value for
  // this parameter is the one that results in a minimum number of iterations
  // while still keeping a stable path on the parametric space of the
  // optimization. Keep in mind that this parameter is a multiplicative factor
  // applied on the gradient of the Metric. Therefore, its effect on the
  // optimizer step length is proportional to the Metric values themselves.
  // Metrics with large values will require you to use smaller values for the
  // learning rate in order to maintain a similar optimizer behavior.
  //
  // Software Guide : EndLatex

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
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType finalParameters = registration->GetLastTransformParameters();

  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];

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
  std::cout << " Numb. Samples = " << numberOfSamples    << std::endl;


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
  //  \ref{fig:FixedMovingImageRegistration2}. The registration is stopped at
  //  200 iterations and produces as result the parameters:
  //
  //  \begin{verbatim}
  //  Translation X = 12.9147
  //  Translation Y = 17.0871
  //  \end{verbatim}
  //  These values are approximately within one tenth of a pixel from the true
  //  misalignment introduced in the moving image.
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
  resample->SetDefaultPixelValue( 100 );


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


  // Generate checkerboards before and after registration
  //
  typedef itk::CheckerBoardImageFilter< FixedImageType > CheckerBoardFilterType;

  CheckerBoardFilterType::Pointer checker = CheckerBoardFilterType::New();

  checker->SetInput1( fixedImage );
  checker->SetInput2( resample->GetOutput() );

  caster->SetInput( checker->GetOutput() );
  writer->SetInput( caster->GetOutput()   );

  // Before registration
  TransformType::Pointer identityTransform = TransformType::New();
  identityTransform->SetIdentity();
  resample->SetTransform( identityTransform );

  if( argc > 4 )
    {
    writer->SetFileName( argv[4] );
    writer->Update();
    }


  // After registration
  resample->SetTransform( finalTransform );
  if( argc > 5 )
    {
    writer->SetFileName( argv[5] );
    writer->Update();
    }


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration2Output}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration2CheckerboardBefore}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration2CheckerboardAfter}
  // \itkcaption[Multi-Modality Registration outputs]{Mapped moving image (left)
  // and composition of fixed and moving images before (center) and after
  // (right) registration.}
  // \label{fig:ImageRegistration2Output}
  // \end{figure}
  //
  //  The moving image after resampling is presented on the left
  //  side of Figure \ref{fig:ImageRegistration2Output}. The center and right
  //  figures present a checkerboard composite of the fixed and
  //  moving images before and after registration.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{ImageRegistration2TraceTranslations}
  // \includegraphics[width=0.44\textwidth]{ImageRegistration2TraceTranslations2}
  // \itkcaption[Multi-Modality Registration plot of translations]{Sequence of
  // translations during the registration process. On the left are iterations 0 to
  // 200. On the right are iterations 150 to 200.}
  // \label{fig:ImageRegistration2TraceTranslations}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration2TraceTranslations} shows the sequence
  //  of translations followed by the optimizer as it searched the parameter
  //  space. The left plot shows iterations $0$ to $200$ while the right
  //  figure zooms into iterations $150$ to $200$. The area covered by the
  //  right figure has been highlighted by a rectangle in the left image.  It
  //  can be seen that after a certain number of iterations the optimizer
  //  oscillates within one or two pixels of the true solution.  At this
  //  point it is clear that more iterations will not help. Instead it is
  //  time to modify some of the parameters of the registration process, for
  //  example, reducing the learning rate of the optimizer and continuing the
  //  registration so that smaller steps are taken.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{ImageRegistration2TraceMetric}
  // \includegraphics[width=0.44\textwidth]{ImageRegistration2TraceMetric2}
  // \itkcaption[Multi-Modality Registration plot of metrics]{The sequence of metric
  // values produced during the registration process. On the left are
  // iterations 0 to 200. On the right are iterations 150 to 200.}
  // \label{fig:ImageRegistration2TraceMetric}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration2TraceMetric} shows the sequence of
  //  metric values computed as the optimizer searched the parameter space.
  //  The left plot shows values when iterations are extended from $0$ to
  //  $200$ while the right figure zooms into iterations $150$ to $200$.  The
  //  fluctuations in the metric value are due to the stochastic nature in
  //  which the measure is computed. At each call of \code{GetValue()}, two
  //  new sets of intensity samples are randomly taken from the image to
  //  compute the density and entropy estimates.  Even with the fluctuations,
  //  the measure initially increases overall with the number of iterations.
  //  After about 150 iterations, the metric value merely oscillates without further
  //  noticeable convergence.  The trace plots in Figure
  //  \ref{fig:ImageRegistration2TraceMetric} highlight one of the
  //  difficulties associated with this particular metric: the stochastic
  //  oscillations make it difficult to determine convergence and limit the
  //  use of more sophisticated optimization methods. As explained above,
  //  the reduction of the learning rate as the registration progresses is
  //  very important in order to get precise results.
  //
  //  This example shows the importance of tracking the evolution of the
  //  registration method in order to obtain insight into the characteristics
  //  of the particular problem at hand and the components being used.  The
  //  behavior revealed by these plots usually helps to identify possible
  //  improvements in the setup of the registration parameters.
  //
  //  The plots in Figures~\ref{fig:ImageRegistration2TraceTranslations}
  //  and~\ref{fig:ImageRegistration2TraceMetric} were generated using
  //  Gnuplot\footnote{\url{http://www.gnuplot.info/}}.  The scripts used for
  //  this purpose are available in the \code{ITKSoftwareGuide} Git repository
  //  under the directory
  //
  //  ~\code{ITKSoftwareGuide/SoftwareGuide/Art}.
  //
  //  Data for the plots was taken directly from the output that the
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
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
