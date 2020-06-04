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
//    INPUTS:  {BrainT1SliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceShifted13x17y.png}
//    OUTPUTS: {MultiResImageRegistration2Output.png}
//    ARGUMENTS:    100
//    OUTPUTS: {MultiResImageRegistration2CheckerboardBefore.png}
//    OUTPUTS: {MultiResImageRegistration2CheckerboardAfter.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
//  This example illustrates the use of more complex components of the
//  registration framework. In particular, it introduces the use of the
//  \doxygen{AffineTransform} and the importance of fine-tuning the scale
//  parameters of the optimizer.
//
// \index{itk::ImageRegistrationMethod!AffineTransform}
// \index{itk::ImageRegistrationMethod!Scaling parameter space}
// \index{itk::AffineTransform!Image Registration}
//
// The AffineTransform is a linear transformation that maps lines into
// lines. It can be used to represent translations, rotations, anisotropic
// scaling, shearing or any combination of them. Details about the affine
// transform can be seen in Section~\ref{sec:AffineTransform}.
//
// In order to use the AffineTransform class, the following header
// must be included.
//
// \index{itk::AffineTransform!Header}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkAffineTransform.h"
// Software Guide : EndCodeSnippet

#include "itkCenteredTransformInitializer.h"
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkImage.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkCheckerBoardImageFilter.h"

//  The following section of code implements an observer
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
    if (!(itk::IterationEvent().CheckEvent(&event)))
    {
      return;
    }
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << optimizer->GetCurrentPosition() << "  "
              << m_CumulativeIterationIndex++ << std::endl;
  }

private:
  unsigned int m_CumulativeIterationIndex{ 0 };
};

//  The following section of code implements a Command observer
//  that will control the modification of optimizer parameters
//  at every change of resolution level.
//
template <typename TRegistration>
class RegistrationInterfaceCommand : public itk::Command
{
public:
  using Self = RegistrationInterfaceCommand;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  RegistrationInterfaceCommand() = default;

public:
  using RegistrationType = TRegistration;
  using RegistrationPointer = RegistrationType *;
  using OptimizerType = itk::RegularStepGradientDescentOptimizer;
  using OptimizerPointer = OptimizerType *;
  void
  Execute(itk::Object * object, const itk::EventObject & event) override
  {
    if (!(itk::IterationEvent().CheckEvent(&event)))
    {
      return;
    }
    auto registration = static_cast<RegistrationPointer>(object);
    auto optimizer =
      static_cast<OptimizerPointer>(registration->GetModifiableOptimizer());

    std::cout << "-------------------------------------" << std::endl;
    std::cout << "MultiResolution Level : " << registration->GetCurrentLevel()
              << std::endl;
    std::cout << std::endl;

    if (registration->GetCurrentLevel() == 0)
    {
      optimizer->SetMaximumStepLength(16.00);
      optimizer->SetMinimumStepLength(0.01);
    }
    else
    {
      optimizer->SetMaximumStepLength(optimizer->GetMaximumStepLength() /
                                      4.0);
      optimizer->SetMinimumStepLength(optimizer->GetMinimumStepLength() /
                                      10.0);
    }
  }
  void
  Execute(const itk::Object *, const itk::EventObject &) override
  {
    return;
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
    std::cerr << " outputImagefile [backgroundGrayLevel]";
    std::cerr << " [checkerboardbefore] [CheckerBoardAfter]";
    std::cerr << " [useExplicitPDFderivatives ] " << std::endl;
    std::cerr << " [numberOfBins] [numberOfSamples ] " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned short;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using InternalPixelType = float;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;

  //  Software Guide : BeginLatex
  //
  //  The configuration of the registration method in this example closely
  //  follows the procedure in the previous section. The main changes involve
  //  the construction and initialization of the transform. The instantiation
  //  of the transform type requires only the dimension of the space and the
  //  type used for representing space coordinates.
  //
  //  \index{itk::AffineTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using TransformType = itk::AffineTransform<double, Dimension>;
  // Software Guide : EndCodeSnippet

  using OptimizerType = itk::RegularStepGradientDescentOptimizer;
  using InterpolatorType =
    itk::LinearInterpolateImageFunction<InternalImageType, double>;
  using MetricType =
    itk::MattesMutualInformationImageToImageMetric<InternalImageType,
                                                   InternalImageType>;

  using OptimizerScalesType = OptimizerType::ScalesType;

  using RegistrationType =
    itk::MultiResolutionImageRegistrationMethod<InternalImageType,
                                                InternalImageType>;

  OptimizerType::Pointer    optimizer = OptimizerType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();
  MetricType::Pointer       metric = MetricType::New();

  registration->SetOptimizer(optimizer);
  registration->SetInterpolator(interpolator);
  registration->SetMetric(metric);

  //  Software Guide : BeginLatex
  //
  //  The transform is constructed using the standard \code{New()} method and
  //  assigning it to a SmartPointer.
  //
  //  \index{itk::AffineTransform!New()}
  //  \index{itk::AffineTransform!Pointer}
  //  \index{itk::Multi\-Resolution\-Image\-Registration\-Method!SetTransform()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer transform = TransformType::New();
  registration->SetTransform(transform);
  // Software Guide : EndCodeSnippet

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  FixedImageReaderType::Pointer fixedImageReader =
    FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader =
    MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  using FixedCastFilterType =
    itk::CastImageFilter<FixedImageType, InternalImageType>;
  using MovingCastFilterType =
    itk::CastImageFilter<MovingImageType, InternalImageType>;
  FixedCastFilterType::Pointer  fixedCaster = FixedCastFilterType::New();
  MovingCastFilterType::Pointer movingCaster = MovingCastFilterType::New();

  fixedCaster->SetInput(fixedImageReader->GetOutput());
  movingCaster->SetInput(movingImageReader->GetOutput());

  registration->SetFixedImage(fixedCaster->GetOutput());
  registration->SetMovingImage(movingCaster->GetOutput());

  fixedCaster->Update();

  registration->SetFixedImageRegion(
    fixedCaster->GetOutput()->GetBufferedRegion());

  //  Software Guide : BeginLatex
  //
  //  One of the easiest ways of preparing a consistent set of parameters for
  //  the transform is to use the \doxygen{CenteredTransformInitializer}. Once
  //  the transform is initialized, we can invoke its \code{GetParameters()}
  //  method to extract the array of parameters. Finally the array is passed
  //  to the registration method using its
  //  \code{SetInitialTransformParameters()} method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using TransformInitializerType =
    itk::CenteredTransformInitializer<TransformType,
                                      FixedImageType,
                                      MovingImageType>;
  TransformInitializerType::Pointer initializer =
    TransformInitializerType::New();
  initializer->SetTransform(transform);
  initializer->SetFixedImage(fixedImageReader->GetOutput());
  initializer->SetMovingImage(movingImageReader->GetOutput());
  initializer->MomentsOn();
  initializer->InitializeTransform();
  registration->SetInitialTransformParameters(transform->GetParameters());
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The set of parameters in the AffineTransform have different
  //  dynamic ranges. Typically the parameters associated with the matrix
  //  have values around $[-1:1]$, although they are not restricted to this
  //  interval.  Parameters associated with translations, on the other hand,
  //  tend to have much higher values, typically in the order of $10.0$ to
  //  $100.0$. This difference in dynamic range negatively affects the
  //  performance of gradient descent optimizers. ITK provides a mechanism to
  //  compensate for such differences in values among the parameters when
  //  they are passed to the optimizer. The mechanism consists of providing an
  //  array of scale factors to the optimizer. These factors re-normalize the
  //  gradient components before they are used to compute the step of the
  //  optimizer at the current iteration. In our particular case, a common
  //  choice for the scale parameters is to set to $1.0$ all those associated
  //  with the matrix coefficients, that is, the first $N \times N$
  //  factors. Then, we set the remaining scale factors to a small value. The
  //  following code sets up the scale coefficients.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  OptimizerScalesType optimizerScales(transform->GetNumberOfParameters());

  optimizerScales[0] = 1.0; // scale for M11
  optimizerScales[1] = 1.0; // scale for M12
  optimizerScales[2] = 1.0; // scale for M21
  optimizerScales[3] = 1.0; // scale for M22

  optimizerScales[4] = 1.0 / 1e7; // scale for translation on X
  optimizerScales[5] = 1.0 / 1e7; // scale for translation on Y
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Here the affine transform is represented by the matrix $\bf{M}$ and the
  //  vector $\bf{T}$. The transformation of a point $\bf{P}$ into $\bf{P'}$
  //  is expressed as
  //
  //  \begin{equation}
  //  \left[
  //  \begin{array}{c}
  //  {P'}_x  \\  {P'}_y  \\  \end{array}
  //  \right]
  //  =
  //  \left[
  //  \begin{array}{cc}
  //  M_{11} & M_{12} \\ M_{21} & M_{22} \\  \end{array}
  //  \right]
  //  \cdot
  //  \left[
  //  \begin{array}{c}
  //  P_x  \\ P_y  \\  \end{array}
  //  \right]
  //  +
  //  \left[
  //  \begin{array}{c}
  //  T_x  \\ T_y  \\  \end{array}
  //  \right]
  //  \end{equation}
  //
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  //  The array of scales is then passed to the optimizer using the
  //  \code{SetScales()} method.
  //
  //  \index{itk::Optimizer!SetScales()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetScales(optimizerScales);
  // Software Guide : EndCodeSnippet

  metric->SetNumberOfHistogramBins(128);
  metric->SetNumberOfSpatialSamples(50000);

  if (argc > 8)
  {
    // optionally, override the values with numbers taken from the command
    // line arguments.
    metric->SetNumberOfHistogramBins(std::stoi(argv[8]));
  }

  if (argc > 9)
  {
    // optionally, override the values with numbers taken from the command
    // line arguments.
    metric->SetNumberOfSpatialSamples(std::stoi(argv[9]));
  }

  //  Software Guide : BeginLatex
  //
  //  Given that the Mattes Mutual Information metric uses a random iterator
  //  in order to collect the samples from the images, it is usually
  //  convenient to initialize the seed of the random number generator.
  //
  //  \index{itk::Mattes\-Mutual\-Information\-Image\-To\-Image\-Metric!ReinitializeSeed()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  metric->ReinitializeSeed(76926294);
  // Software Guide : EndCodeSnippet

  if (argc > 7)
  {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the
    // Transform parameters, or doing it by progressively accumulating
    // contributions from each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives(std::stoi(argv[7]));
  }

  //  Software Guide : BeginLatex
  //
  //  The step length has to be proportional to the expected values of the
  //  parameters in the search space. Since the expected values of the matrix
  //  coefficients are around $1.0$, the initial step of the optimization
  //  should be a small number compared to $1.0$. As a guideline, it is
  //  useful to think of the matrix coefficients as combinations of
  //  $cos(\theta)$ and $sin(\theta)$.  This leads to use values close to the
  //  expected rotation measured in radians. For example, a rotation of $1.0$
  //  degree is about $0.017$ radians. As in the previous example, the
  //  maximum and minimum step length of the optimizer are set by the
  //  \code{RegistrationInterfaceCommand} when it is called at the beginning
  //  of registration at each multi-resolution level.
  //
  //  Software Guide : EndLatex

  optimizer->SetNumberOfIterations(200);
  optimizer->SetRelaxationFactor(0.8);

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  // Create the Command interface observer and register it with the optimizer.
  //
  using CommandType = RegistrationInterfaceCommand<RegistrationType>;
  CommandType::Pointer command = CommandType::New();
  registration->AddObserver(itk::IterationEvent(), command);
  registration->SetNumberOfLevels(3);

  try
  {
    registration->Update();
    std::cout << "Optimizer stop condition: "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Optimizer Stopping Condition = "
            << optimizer->GetStopCondition() << std::endl;

  using ParametersType = RegistrationType::ParametersType;
  ParametersType finalParameters = registration->GetLastTransformParameters();

  double TranslationAlongX = finalParameters[4];
  double TranslationAlongY = finalParameters[5];

  unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  double bestValue = optimizer->GetValue();

  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue << std::endl;

  //  Software Guide : BeginLatex
  //
  //  Let's execute this example using the same multi-modality images as
  //  before.  The registration converges after $5$ iterations in the first
  //  level, $7$ in the second level and $4$ in the third level. The final
  //  results when printed as an array of parameters are
  //
  //  \begin{verbatim}
  // [1.00164, 0.00147688, 0.00168372, 1.0027, 12.6296, 16.4768]
  //  \end{verbatim}
  //
  //  By reordering them as coefficient of matrix $\bf{M}$ and vector $\bf{T}$
  //  they can now be seen as
  //
  //  \begin{equation}
  //  M =
  //  \left[
  //  \begin{array}{cc}
  //  1.00164 & 0.0014 \\ 0.00168 & 1.0027 \\  \end{array}
  //  \right]
  //  \mbox{ and }
  //  T =
  //  \left[
  //  \begin{array}{c}
  //  12.6296  \\  16.4768  \\  \end{array}
  //  \right]
  //  \end{equation}
  //
  //  In this form, it is easier to interpret the effect of the
  //  transform. The matrix $\bf{M}$ is responsible for scaling, rotation and
  //  shearing while $\bf{T}$ is responsible for translations.  It can be seen
  //  that the translation values in this case closely match the true
  //  misalignment introduced in the moving image.
  //
  //  It is important to note that once the images are registered at a
  //  sub-pixel level, any further improvement of the registration relies
  //  heavily on the quality of the interpolator. It may then be reasonable to
  //  use a coarse and fast interpolator in the lower resolution levels and
  //  switch to a high-quality but slow interpolator in the final resolution
  //  level.
  //
  //  Software Guide : EndLatex

  using ResampleFilterType =
    itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters(finalParameters);
  finalTransform->SetFixedParameters(transform->GetFixedParameters());

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform(finalTransform);
  resample->SetInput(movingImageReader->GetOutput());

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  PixelType backgroundGrayLevel = 100;
  if (argc > 4)
  {
    backgroundGrayLevel = std::stoi(argv[4]);
  }

  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(backgroundGrayLevel);

  using OutputPixelType = unsigned char;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using CastFilterType =
    itk::CastImageFilter<FixedImageType, OutputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();

  writer->SetFileName(argv[3]);

  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{MultiResImageRegistration2Output}
  // \includegraphics[width=0.32\textwidth]{MultiResImageRegistration2CheckerboardBefore}
  // \includegraphics[width=0.32\textwidth]{MultiResImageRegistration2CheckerboardAfter}
  // \itkcaption[Multi-Resolution Registration Input Images]{Mapped moving
  // image (left) and composition of fixed and moving images before (center)
  // and after (right) multi-resolution registration with the AffineTransform
  // class.} \label{fig:MultiResImageRegistration2Output} \end{figure}
  //
  //  The result of resampling the moving image is shown in the left image
  //  of Figure \ref{fig:MultiResImageRegistration2Output}. The center and
  //  right images of the figure present a checkerboard composite of the fixed
  //  and moving images before and after registration.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.44\textwidth]{MultiResImageRegistration2TraceTranslations}
  // \includegraphics[height=0.44\textwidth]{MultiResImageRegistration2TraceMetric}
  // \itkcaption[Multi-Resolution Registration output plots]{Sequence of
  // translations and metric values at each iteration of the optimizer for
  // multi-resolution with the AffineTransform class.}
  // \label{fig:MultiResImageRegistration2Trace}
  // \end{figure}
  //
  //  Figure \ref{fig:MultiResImageRegistration2Trace} (left) presents the
  //  sequence of translations followed by the optimizer as it searched the
  //  parameter space. The right side of the same figure shows the sequence of
  //  metric values computed as the optimizer explored the parameter space.
  //
  //  Software Guide : EndLatex

  //
  // Generate checkerboards before and after registration
  //
  using CheckerBoardFilterType = itk::CheckerBoardImageFilter<FixedImageType>;

  CheckerBoardFilterType::Pointer checker = CheckerBoardFilterType::New();

  checker->SetInput1(fixedImage);
  checker->SetInput2(resample->GetOutput());

  caster->SetInput(checker->GetOutput());
  writer->SetInput(caster->GetOutput());

  resample->SetDefaultPixelValue(0);

  // Write out checkerboard outputs
  // Before registration
  TransformType::Pointer identityTransform = TransformType::New();
  identityTransform->SetIdentity();
  resample->SetTransform(identityTransform);

  if (argc > 5)
  {
    writer->SetFileName(argv[5]);
    writer->Update();
  }

  // After registration
  resample->SetTransform(finalTransform);
  if (argc > 6)
  {
    writer->SetFileName(argv[6]);
    writer->Update();
  }

  return EXIT_SUCCESS;
}
