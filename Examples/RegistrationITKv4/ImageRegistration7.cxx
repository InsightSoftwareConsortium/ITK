/*=========================================================================
 *
 *  Copyright NumFOCUS
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
//    INPUTS:  {BrainProtonDensitySliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceR10X13Y17S12.png}
//    OUTPUTS: {ImageRegistration7Output.png}
//    OUTPUTS: {ImageRegistration7DifferenceBefore.png}
//    OUTPUTS: {ImageRegistration7DifferenceAfter.png}
//    ARGUMENTS:    1.0   1.0   0.0
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{Simularity2DTransform}
// class for performing registration in $2D$. The example code is for
// the most part identical to the code presented in Section
// \ref{sec:InitializingRegistrationWithMoments}.  The main difference is the
// use of \doxygen{Simularity2DTransform} here rather than the
// \doxygen{Euler2DTransform} class.
//
// A similarity transform can be seen as a composition of rotations,
// translations and uniform $\left(\text{isotropic}\right)$ scaling. It
// preserves angles and maps lines into
// lines. This transform is implemented in the toolkit as deriving from a rigid
// $2D$ transform and with a scale parameter added.
//
// When using this transform, attention should be paid to the fact that scaling
// and translations are not independent.  In the same way that rotations can
// locally be seen as translations, scaling also results in local displacements.
// Scaling is performed in general with respect to the origin of coordinates.
// However, we already saw how ambiguous that could be in the case of
// rotations. For this reason, this transform also allows users to setup a
// specific center. This center is used both for rotation and scaling.
//
//
// \index{itk::Simularity2DTransform}
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethodv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegularStepGradientDescentOptimizerv4.h"


#include "itkCenteredTransformInitializer.h"


//  Software Guide : BeginLatex
//
//  In addition to the headers included in previous examples, here the
//  following header must be included.
//
//  \index{itk::Simularity2DTransform!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSimilarity2DTransform.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkIdentityTransform.h"


//  The following section of code implements a Command observer
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
  using OptimizerType = itk::RegularStepGradientDescentOptimizerv4<double>;
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
    std::cout << optimizer->GetCurrentPosition() << std::endl;
  }
};

int
main(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << " outputImagefile  [differenceBeforeRegistration] ";
    std::cerr << " [differenceAfterRegistration] ";
    std::cerr << " [steplength] ";
    std::cerr << " [initialScaling] [initialAngle] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = float;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;


  //  Software Guide : BeginLatex
  //
  //  The Transform class is instantiated using the code below. The only
  //  template parameter of this class is the representation type of the
  //  space coordinates.
  //
  //  \index{itk::Simularity2DTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using TransformType = itk::Similarity2DTransform<double>;
  // Software Guide : EndCodeSnippet


  using OptimizerType = itk::RegularStepGradientDescentOptimizerv4<double>;
  using MetricType =
    itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType>;
  using RegistrationType =
    itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, TransformType>;

  MetricType::Pointer       metric = MetricType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  RegistrationType::Pointer registration = RegistrationType::New();

  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);


  //  Software Guide : BeginLatex
  //
  //  As before, the transform object is constructed and initialized before it
  //  is passed to the registration filter.
  //
  //  \index{itk::Simularity2DTransform!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer transform = TransformType::New();
  // Software Guide : EndCodeSnippet


  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  FixedImageReaderType::Pointer  fixedImageReader = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);


  registration->SetFixedImage(fixedImageReader->GetOutput());
  registration->SetMovingImage(movingImageReader->GetOutput());


  //  Software Guide : BeginLatex
  //
  //  In this example, we again use the helper class
  //  \doxygen{CenteredTransformInitializer} to compute a reasonable
  //  value for the initial center of rotation and scaling along with
  //  an initial translation.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  using TransformInitializerType =
    itk::CenteredTransformInitializer<TransformType, FixedImageType, MovingImageType>;

  TransformInitializerType::Pointer initializer = TransformInitializerType::New();

  initializer->SetTransform(transform);

  initializer->SetFixedImage(fixedImageReader->GetOutput());
  initializer->SetMovingImage(movingImageReader->GetOutput());

  initializer->MomentsOn();

  initializer->InitializeTransform();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The remaining parameters of the transform are initialized below.
  //
  //  \index{itk::Simularity2DTransform!SetScale()}
  //  \index{itk::Simularity2DTransform!SetAngle()}
  //
  //  Software Guide : EndLatex

  double initialScale = 1.0;

  if (argc > 7)
  {
    initialScale = std::stod(argv[7]);
  }

  double initialAngle = 0.0;

  if (argc > 8)
  {
    initialAngle = std::stod(argv[8]);
  }

  // Software Guide : BeginCodeSnippet
  transform->SetScale(initialScale);
  transform->SetAngle(initialAngle);
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Now the initialized transform object will be set to the registration method,
  //  and its initial parameters are used to initialize the registration process.
  //
  //  Also, by calling the \code{InPlaceOn()} method, this initialized
  //  transform will be the output transform
  //  object or ``grafted'' to the output of the registration process.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransform(transform);
  registration->InPlaceOn();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Keeping in mind that the scale of units in scaling, rotation and
  //  translation are quite different, we take advantage of the scaling
  //  functionality provided by the optimizers. We know that the first element
  //  of the parameters array corresponds to the scale factor, the second
  //  corresponds to the angle, third and fourth are the remaining
  //  translation. We use henceforth small factors in the scales
  //  associated with translations.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using OptimizerScalesType = OptimizerType::ScalesType;
  OptimizerScalesType optimizerScales(transform->GetNumberOfParameters());
  const double        translationScale = 1.0 / 100.0;

  optimizerScales[0] = 10.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = translationScale;
  optimizerScales[3] = translationScale;

  optimizer->SetScales(optimizerScales);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We also set the ordinary parameters of the optimization method. In this
  //  case we are using a
  //  \doxygen{RegularStepGradientDescentOptimizerv4}. Below we define the
  //  optimization parameters, i.e. initial learning rate (step length), minimal
  //  step length and number of iterations. The last two act as stopping criteria
  //  for the optimization.
  //
  //  Software Guide : EndLatex

  double steplength = 1.0;

  if (argc > 6)
  {
    steplength = std::stod(argv[6]);
  }

  // Software Guide : BeginCodeSnippet
  optimizer->SetLearningRate(steplength);
  optimizer->SetMinimumStepLength(0.0001);
  optimizer->SetNumberOfIterations(500);
  // Software Guide : EndCodeSnippet


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  // One level registration process without shrinking and smoothing.
  //
  constexpr unsigned int numberOfLevels = 1;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize(1);
  shrinkFactorsPerLevel[0] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize(1);
  smoothingSigmasPerLevel[0] = 0;

  registration->SetNumberOfLevels(numberOfLevels);
  registration->SetSmoothingSigmasPerLevel(smoothingSigmasPerLevel);
  registration->SetShrinkFactorsPerLevel(shrinkFactorsPerLevel);


  try
  {
    registration->Update();
    std::cout << "Optimizer stop condition: "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  TransformType::ParametersType finalParameters = transform->GetParameters();


  const double finalScale = finalParameters[0];
  const double finalAngle = finalParameters[1];
  const double finalTranslationX = finalParameters[2];
  const double finalTranslationY = finalParameters[3];

  const double rotationCenterX =
    registration->GetOutput()->Get()->GetFixedParameters()[0];
  const double rotationCenterY =
    registration->GetOutput()->Get()->GetFixedParameters()[1];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();


  // Print out results
  //
  const double finalAngleInDegrees = finalAngle * 180.0 / itk::Math::pi;

  std::cout << std::endl;
  std::cout << "Result = " << std::endl;
  std::cout << " Scale           = " << finalScale << std::endl;
  std::cout << " Angle (radians) = " << finalAngle << std::endl;
  std::cout << " Angle (degrees) =  " << finalAngleInDegrees << std::endl;
  std::cout << " Translation X   = " << finalTranslationX << std::endl;
  std::cout << " Translation Y   = " << finalTranslationY << std::endl;
  std::cout << " Fixed Center X  = " << rotationCenterX << std::endl;
  std::cout << " Fixed Center Y  = " << rotationCenterY << std::endl;
  std::cout << " Iterations      = " << numberOfIterations << std::endl;
  std::cout << " Metric value    = " << bestValue << std::endl;


  //  Software Guide : BeginLatex
  //
  //  Let's execute this example over some of the images provided in
  //  \code{Examples/Data}, for example:
  //
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png}
  //  \item \code{BrainProtonDensitySliceR10X13Y17S12.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first image
  //  by $10$ degrees, scaling by $1/1.2$ and then translating by $(-13,-17)$.
  //  Both images have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration7}. The registration takes $53$
  //  iterations and produces:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [0.833237, -0.174511, -12.8065, -12.7244 ]
  //  \end{verbatim}
  //  \end{center}
  //
  //  That are interpreted as
  //
  //  \begin{itemize}
  //  \item Scale factor  =                     $0.833237$
  //  \item Angle         =                     $-0.174511$   radians
  //  \item Translation   = $( -12.8065, -12.7244 )$ millimeters
  //  \end{itemize}
  //
  //
  //  These values approximate the misalignment intentionally introduced into
  //  the moving image. Since $10$ degrees is about $0.174532$ radians.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17S12}
  // \itkcaption[Fixed and Moving image registered with
  // Simularity2DTransform]{Fixed and Moving image provided as input to the
  // registration method using the Similarity2D transform.}
  // \label{fig:FixedMovingImageRegistration7}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration7Output}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration7DifferenceBefore}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration7DifferenceAfter}
  // \itkcaption[Output of the Simularity2DTransform registration]{Resampled
  // moving image (left). Differences between fixed and
  // moving images, before (center) and after (right) registration with the
  // Similarity2D transform.}
  // \label{fig:ImageRegistration7Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration7Outputs} shows the output of the
  // registration. The right image shows the squared magnitude of pixel
  // differences between the fixed image and the resampled moving image.
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration7TraceMetric}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration7TraceAngle}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration7TraceScale}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration7TraceTranslations}
  // \itkcaption[Simularity2DTransform registration plots]{Plots of the Metric,
  // rotation angle, scale factor, and translations during
  // the registration using
  // Similarity2D transform.}
  // \label{fig:ImageRegistration7Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration7Plots} shows the plots of the main
  //  output parameters of the registration process. The metric values at every
  //  iteration are shown on the left. The rotation angle and scale factor values are
  //  shown in the two center plots while the translation components of the registration
  //  are presented in the plot on the right.
  //
  //  Software Guide : EndLatex

  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  resampler->SetTransform(transform);
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


  using DifferenceFilterType =
    itk::SubtractImageFilter<FixedImageType, FixedImageType, FixedImageType>;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();


  using RescalerType =
    itk::RescaleIntensityImageFilter<FixedImageType, OutputImageType>;

  RescalerType::Pointer intensityRescaler = RescalerType::New();

  intensityRescaler->SetInput(difference->GetOutput());
  intensityRescaler->SetOutputMinimum(0);
  intensityRescaler->SetOutputMaximum(255);

  difference->SetInput1(fixedImageReader->GetOutput());
  difference->SetInput2(resampler->GetOutput());

  resampler->SetDefaultPixelValue(1);

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput(intensityRescaler->GetOutput());


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
