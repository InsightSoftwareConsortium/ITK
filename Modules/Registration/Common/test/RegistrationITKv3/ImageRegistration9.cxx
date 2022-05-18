/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

//    INPUTS:  {BrainProtonDensitySliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceR10X13Y17.png}
//    OUTPUTS: {ImageRegistration9Output.png}
//    OUTPUTS: {ImageRegistration9DifferenceBefore.png}
//    OUTPUTS: {ImageRegistration9DifferenceAfter.png}
//    ARGUMENTS:    1.0 300

//
// This example illustrates the use of the \doxygen{AffineTransform}
// for performing registration in $2D$. The example code is, for the most part,
// identical to that in \ref{sec:InitializingRegistrationWithMoments}.
// The main difference is the use of the AffineTransform here instead of the
// \doxygen{CenteredRigid2DTransform}. We will focus on the most
// relevant changes in the current code and skip the basic elements already
// explained in previous examples.
//
// \index{itk::AffineTransform}
//

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"


#include "itkCenteredTransformInitializer.h"

//
//  Let's start by including the header file of the AffineTransform.
//
//  \index{itk::AffineTransform!header}
//

#include "itkAffineTransform.h"


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"


//
//  The following piece of code implements an observer
//  that will monitor the evolution of the registration process.
//
#include "itkCommand.h"
class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  CommandIterationUpdate() = default;

public:
  using OptimizerType = itk::RegularStepGradientDescentOptimizer;
  using OptimizerPointer = const OptimizerType *;

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    auto optimizer = static_cast<OptimizerPointer>(object);
    if (!itk::IterationEvent().CheckEvent(&event))
    {
      return;
    }
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << optimizer->GetCurrentPosition();

    // Print the angle for the trace plot
    vnl_matrix<double> p(2, 2);
    p[0][0] = static_cast<double>(optimizer->GetCurrentPosition()[0]);
    p[0][1] = static_cast<double>(optimizer->GetCurrentPosition()[1]);
    p[1][0] = static_cast<double>(optimizer->GetCurrentPosition()[2]);
    p[1][1] = static_cast<double>(optimizer->GetCurrentPosition()[3]);
    vnl_svd<double>    svd(p);
    vnl_matrix<double> r(2, 2);
    r = svd.U() * vnl_transpose(svd.V());
    double angle = std::asin(r[1][0]);
    std::cout << " AffineAngle: " << angle * 180.0 / itk::Math::pi << std::endl;
  }
};


#include "itkTestDriverIncludeRequiredFactories.h"
int
main(int argc, char * argv[])
{
  RegisterRequiredFactories();
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << "   fixedImageFile  movingImageFile " << std::endl;
    std::cerr << "   outputImagefile  [differenceBeforeRegistration] " << std::endl;
    std::cerr << "   [differenceAfterRegistration] " << std::endl;
    std::cerr << "   [stepLength] [maxNumberOfIterations] " << std::endl;
    return EXIT_FAILURE;
  }


  //
  //  We then define the types of the images to be registered.
  //

  constexpr unsigned int Dimension = 2;
  using PixelType = float;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;


  //
  //  The transform type is instantiated using the code below. The template
  //  parameters of this class are the representation type of the space
  //  coordinates and the space dimension.
  //
  //  \index{itk::AffineTransform!Instantiation}
  //

  using TransformType = itk::AffineTransform<double, Dimension>;


  using OptimizerType = itk::RegularStepGradientDescentOptimizer;
  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;
  using RegistrationType = itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;

  MetricType::Pointer       metric = MetricType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();

  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetInterpolator(interpolator);


  //
  //  The transform object is constructed below and is initialized before the registration
  //  process starts.
  //
  //  \index{itk::AffineTransform!New()}
  //  \index{itk::AffineTransform!Pointer}
  //  \index{itk::RegistrationMethod!SetTransform()}
  //

  TransformType::Pointer transform = TransformType::New();
  registration->SetTransform(transform);


  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;
  FixedImageReaderType::Pointer  fixedImageReader = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();
  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);


  registration->SetFixedImage(fixedImageReader->GetOutput());
  registration->SetMovingImage(movingImageReader->GetOutput());
  fixedImageReader->Update();

  registration->SetFixedImageRegion(fixedImageReader->GetOutput()->GetBufferedRegion());


  //
  //  In this example, we again use the
  //  \doxygen{CenteredTransformInitializer} helper class in order to compute
  //  a reasonable value for the initial center of rotation and the
  //  translation. The initializer is set to use the center of mass of each
  //  image as the initial correspondence correction.
  //

  using TransformInitializerType = itk::CenteredTransformInitializer<TransformType, FixedImageType, MovingImageType>;
  TransformInitializerType::Pointer initializer = TransformInitializerType::New();
  initializer->SetTransform(transform);
  initializer->SetFixedImage(fixedImageReader->GetOutput());
  initializer->SetMovingImage(movingImageReader->GetOutput());
  initializer->MomentsOn();
  initializer->InitializeTransform();


  //
  //  Now we pass the parameters of the current transform as the initial
  //  parameters to be used when the registration process starts.
  //

  registration->SetInitialTransformParameters(transform->GetParameters());


  //
  //  Keeping in mind that the scale of units in scaling, rotation and
  //  translation are quite different, we take advantage of the scaling
  //  functionality provided by the optimizers. We know that the first $N
  //  \times N$ elements of the parameters array correspond to the rotation
  //  matrix factor, and the last $N$ are the components of the translation to
  //  be applied after multiplication with the matrix is performed.
  //


  double translationScale = 1.0 / 1000.0;
  if (argc > 8)
  {
    translationScale = std::stod(argv[8]);
  }


  using OptimizerScalesType = OptimizerType::ScalesType;
  OptimizerScalesType optimizerScales(transform->GetNumberOfParameters());

  optimizerScales[0] = 1.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = 1.0;
  optimizerScales[3] = 1.0;
  optimizerScales[4] = translationScale;
  optimizerScales[5] = translationScale;

  optimizer->SetScales(optimizerScales);


  //
  //  We also set the usual parameters of the optimization method. In this
  //  case we are using an
  //  \doxygen{RegularStepGradientDescentOptimizer}. Below, we define the
  //  optimization parameters like initial step length, minimal step length
  //  and number of iterations. These last two act as stopping criteria for
  //  the optimization.
  //

  double steplength = 0.1;

  if (argc > 6)
  {
    steplength = std::stod(argv[6]);
  }


  unsigned int maxNumberOfIterations = 300;

  if (argc > 7)
  {
    maxNumberOfIterations = std::stoi(argv[7]);
  }


  optimizer->SetMaximumStepLength(steplength);
  optimizer->SetMinimumStepLength(0.0001);
  optimizer->SetNumberOfIterations(maxNumberOfIterations);


  //
  //  We also set the optimizer to do minimization by calling the
  //  \code{MinimizeOn()} method.
  //
  //  \index{itk::Regular\-Step\-Gradient\-Descent\-Optimizer!MinimizeOn()}
  //

  optimizer->MinimizeOn();


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);


  //
  //  Finally we trigger the execution of the registration method by calling
  //  the \code{Update()} method. The call is placed in a \code{try/catch}
  //  block in case any exceptions are thrown.
  //

  try
  {
    registration->Update();
    std::cout << "Optimizer stop condition: " << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }


  //
  //  Once the optimization converges, we recover the parameters from the
  //  registration method. This is done with the
  //  \code{GetLastTransformParameters()} method. We can also recover the
  //  final value of the metric with the \code{GetValue()} method and the
  //  final number of iterations with the \code{GetCurrentIteration()}
  //  method.
  //
  //  \index{itk::RegistrationMethod!GetValue()}
  //  \index{itk::RegistrationMethod!GetCurrentIteration()}
  //  \index{itk::RegistrationMethod!GetLastTransformParameters()}
  //

  OptimizerType::ParametersType finalParameters = registration->GetLastTransformParameters();

  const double finalRotationCenterX = transform->GetCenter()[0];
  const double finalRotationCenterY = transform->GetCenter()[1];
  const double finalTranslationX = finalParameters[4];
  const double finalTranslationY = finalParameters[5];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  const double       bestValue = optimizer->GetValue();


  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Center X      = " << finalRotationCenterX << std::endl;
  std::cout << " Center Y      = " << finalRotationCenterY << std::endl;
  std::cout << " Translation X = " << finalTranslationX << std::endl;
  std::cout << " Translation Y = " << finalTranslationY << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue << std::endl;

  // Compute the rotation angle and scaling from SVD of the matrix
  // \todo Find a way to figure out if the scales are along X or along Y.
  // VNL returns the eigenvalues ordered from largest to smallest.

  vnl_matrix<double> p(2, 2);
  p[0][0] = static_cast<double>(finalParameters[0]);
  p[0][1] = static_cast<double>(finalParameters[1]);
  p[1][0] = static_cast<double>(finalParameters[2]);
  p[1][1] = static_cast<double>(finalParameters[3]);
  vnl_svd<double>    svd(p);
  vnl_matrix<double> r(2, 2);
  r = svd.U() * vnl_transpose(svd.V());
  double angle = std::asin(r[1][0]);

  const double angleInDegrees = angle * 180.0 / itk::Math::pi;

  std::cout << " Scale 1         = " << svd.W(0) << std::endl;
  std::cout << " Scale 2         = " << svd.W(1) << std::endl;
  std::cout << " Angle (degrees) = " << angleInDegrees << std::endl;


  //
  //  Let's execute this example over two of the images provided in
  //  \code{Examples/Data}:
  //
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png}
  //  \item \code{BrainProtonDensitySliceR10X13Y17.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first
  //  image by $10$ degrees and then translating by $(-13,-17)$.  Both images
  //  have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration9}. We execute the code using the
  //  following parameters: step length=1.0, translation scale= 0.0001 and
  //  maximum number of iterations = 300. With these images and parameters
  //  the registration takes $98$ iterations and produces
  //
  //  \begin{center}
  //  \begin{verbatim}
  //   96 58.09 [0.986481, -0.169104, 0.166411, 0.986174, 12.461, 16.0754]
  //  \end{verbatim}
  //  \end{center}
  //
  //  These results are interpreted as
  //
  //  \begin{itemize}
  //  \item Iterations   = 98
  //  \item Final Metric = 58.09
  //  \item Center       = $( 111.204,   131.6   )$ millimeters
  //  \item Translation  = $(   12.461,  16.0754 )$ millimeters
  //  \item Affine scales = $(1.00185, .999137)$
  //  \end{itemize}
  //
  //  The second component of the matrix values is usually associated with
  //  $\sin{\theta}$. We obtain the rotation through SVD of the affine
  //  matrix. The value is $9.6526$ degrees, which is approximately the
  //  intentional misalignment of $10.0$ degrees.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17}
  // \itkcaption[AffineTransform registration]{Fixed and moving images
  // provided as input to the registration method using the AffineTransform.}
  // \label{fig:FixedMovingImageRegistration9}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration9Output}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration9DifferenceBefore}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration9DifferenceAfter}
  // \itkcaption[AffineTransform output images]{The resampled moving image
  // (left), and the difference between the fixed and moving images before (center)
  // and after (right) registration with the
  // AffineTransform transform.}
  // \label{fig:ImageRegistration9Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration9Outputs} shows the output of the
  // registration. The right most image of this figure shows the squared
  // magnitude difference between the fixed image and the resampled
  // moving image.
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration9TraceMetric}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration9TraceAngle}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration9TraceTranslations}
  // \itkcaption[AffineTransform output plots]{Metric values,
  // rotation angle and translations during the registration using the
  // AffineTransform transform.}
  // \label{fig:ImageRegistration9Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration9Plots} shows the plots of the main
  //  output parameters of the registration process. The metric values at every
  //  iteration are shown on the top plot. The angle values are shown on the bottom left plot,
  //  while the translation components of the registration are presented
  //  on the bottom right plot. Note that the final total offset of the transform
  //  is to be computed as a combination of the shift due rotation plus the
  //  explicit translation set on the transform.
  //


  //  The following code is used to dump output images to files.
  //  They illustrate the final results of the registration.
  //  We will resample the moving image and write out the difference image
  //  before and after registration. We will also rescale the intensities of the
  //  difference images, so that they look better!
  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters(finalParameters);
  finalTransform->SetFixedParameters(transform->GetFixedParameters());

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  resampler->SetTransform(finalTransform);
  resampler->SetInput(movingImageReader->GetOutput());

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resampler->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resampler->SetOutputOrigin(fixedImage->GetOrigin());
  resampler->SetOutputSpacing(fixedImage->GetSpacing());
  resampler->SetOutputDirection(fixedImage->GetDirection());
  resampler->SetDefaultPixelValue(100);

  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using CastFilterType = itk::CastImageFilter<FixedImageType, OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;


  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();


  writer->SetFileName(argv[3]);


  caster->SetInput(resampler->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();


  using DifferenceFilterType = itk::SubtractImageFilter<FixedImageType, FixedImageType, FixedImageType>;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  difference->SetInput1(fixedImageReader->GetOutput());
  difference->SetInput2(resampler->GetOutput());

  WriterType::Pointer writer2 = WriterType::New();

  using RescalerType = itk::RescaleIntensityImageFilter<FixedImageType, OutputImageType>;

  RescalerType::Pointer intensityRescaler = RescalerType::New();

  intensityRescaler->SetInput(difference->GetOutput());
  intensityRescaler->SetOutputMinimum(0);
  intensityRescaler->SetOutputMaximum(255);

  writer2->SetInput(intensityRescaler->GetOutput());
  resampler->SetDefaultPixelValue(1);

  // Compute the difference image between the
  // fixed and resampled moving image.
  if (argc > 5)
  {
    writer2->SetFileName(argv[5]);
    writer2->Update();
  }


  using IdentityTransformType = itk::IdentityTransform<double, Dimension>;
  IdentityTransformType::Pointer identity = IdentityTransformType::New();

  // Compute the difference image between the
  // fixed and moving image before registration.
  if (argc > 4)
  {
    resampler->SetTransform(identity);
    writer2->SetFileName(argv[4]);
    writer2->Update();
  }

  return EXIT_SUCCESS;
}
