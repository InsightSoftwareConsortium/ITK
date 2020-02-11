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

//    INPUTS:  {BrainProtonDensitySliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceR10X13Y17S12.png}
//    OUTPUTS: {ImageRegistration7Output.png}
//    OUTPUTS: {ImageRegistration7DifferenceBefore.png}
//    OUTPUTS: {ImageRegistration7DifferenceAfter.png}
//    ARGUMENTS:    1.0   1.0   0.0

//
// This example illustrates the use of the \doxygen{CenteredSimilarity2DTransform}
// class for performing registration in $2D$. The example code is for
// the most part identical to the code presented in Section
// \ref{sec:InitializingRegistrationWithMoments}.  The main difference is the
// use of \doxygen{CenteredSimilarity2DTransform} here rather than the
// \doxygen{CenteredRigid2DTransform} class.
//
// A similarity transform can be seen as a composition of rotations,
// translations and uniform scaling. It preserves angles and map lines into
// lines. This transform is implemented in the toolkit as deriving from a rigid
// $2D$ transform and with a scale parameter added.
//
// When using this transform, attention should be paid to the fact that scaling
// and translations are not independent.  In the same way that rotations can
// locally be seen as translations, scaling also result in local displacements.
// Scaling is performed in general with respect to the origin of coordinates.
// However, we already saw how ambiguous that could be in the case of
// rotations. For this reason, this transform also allows users to setup a
// specific center. This center is use both for rotation and scaling.
//
//
// \index{itk::CenteredSimilarity2DTransform}
//

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"


#include "itkCenteredTransformInitializer.h"


//
//  In addition to the headers included in previous examples, here the
//  following header must be included.
//
//  \index{itk::CenteredSimilarity2DTransform!header}
//

#include "itkCenteredSimilarity2DTransform.h"


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
    std::cout << optimizer->GetCurrentPosition() << std::endl;
  }
};

#include "itkTestDriverIncludeRequiredIOFactories.h"
int
main(int argc, char * argv[])
{
  RegisterRequiredFactories();
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


  //
  //  The Transform class is instantiated using the code below. The only
  //  template parameter of this class is the representation type of the
  //  space coordinates.
  //
  //  \index{itk::CenteredSimilarity2DTransform!Instantiation}
  //

  using TransformType = itk::CenteredSimilarity2DTransform<double>;


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
  //  The transform object is constructed below and passed to the registration
  //  method.
  //
  //  \index{itk::CenteredSimilarity2DTransform!New()}
  //  \index{itk::CenteredSimilarity2DTransform!Pointer}
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
  //  In this example, we again use the helper class
  //  \doxygen{CenteredTransformInitializer} to compute a reasonable
  //  value for the initial center of rotation and the translation.
  //


  using TransformInitializerType = itk::CenteredTransformInitializer<TransformType, FixedImageType, MovingImageType>;

  TransformInitializerType::Pointer initializer = TransformInitializerType::New();

  initializer->SetTransform(transform);

  initializer->SetFixedImage(fixedImageReader->GetOutput());
  initializer->SetMovingImage(movingImageReader->GetOutput());

  initializer->MomentsOn();

  initializer->InitializeTransform();


  //
  //  The remaining parameters of the transform are initialized below.
  //
  //  \index{itk::CenteredSimilarity2DTransform!SetScale()}
  //  \index{itk::CenteredSimilarity2DTransform!SetAngle()}
  //

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

  transform->SetScale(initialScale);
  transform->SetAngle(initialAngle);

  //
  //  Now the initialized transform object will be set to the registration method,
  //  and its initial parameters are used to initialize the registration process.
  //
  //  Also, by calling the \code{InPlaceOn()} method, this initialized
  //  transform will be the output transform
  //  object or ``grafted'' to the output of the registration process.
  //

  registration->SetInitialTransformParameters(transform->GetParameters());


  //
  //  Keeping in mind that the scale of units in scaling, rotation and
  //  translation are quite different, we take advantage of the scaling
  //  functionality provided by the optimizers. We know that the first element
  //  of the parameters array corresponds to the scale factor, the second
  //  corresponds to the angle, third and forth are the center of rotation and
  //  fifth and sixth are the remaining translation. We use henceforth small
  //  factors in the scales associated with translations and the rotation
  //  center.
  //

  using OptimizerScalesType = OptimizerType::ScalesType;
  OptimizerScalesType optimizerScales(transform->GetNumberOfParameters());
  const double        translationScale = 1.0 / 100.0;

  optimizerScales[0] = 10.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = translationScale;
  optimizerScales[3] = translationScale;
  optimizerScales[4] = translationScale;
  optimizerScales[5] = translationScale;

  optimizer->SetScales(optimizerScales);


  //
  //  We also set the ordinary parameters of the optimization method. In this
  //  case we are using a
  //  \doxygen{RegularStepGradientDescentOptimizer}. Below we define the
  //  optimization parameters, i.e. initial learning rate (step length), minimal
  //  step length and number of iterations. The last two act as stopping criteria
  //  for the optimization.
  //

  double steplength = 1.0;

  if (argc > 6)
  {
    steplength = std::stod(argv[6]);
  }

  optimizer->SetMaximumStepLength(steplength);
  optimizer->SetMinimumStepLength(0.0001);
  optimizer->SetNumberOfIterations(500);


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);


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

  OptimizerType::ParametersType finalParameters = registration->GetLastTransformParameters();


  const double finalScale = finalParameters[0];
  const double finalAngle = finalParameters[1];
  const double finalRotationCenterX = finalParameters[2];
  const double finalRotationCenterY = finalParameters[3];
  const double finalTranslationX = finalParameters[4];
  const double finalTranslationY = finalParameters[5];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();


  // Print out results
  //
  const double finalAngleInDegrees = finalAngle * 180.0 / itk::Math::pi;

  std::cout << std::endl;
  std::cout << "Result = " << std::endl;
  std::cout << " Scale         = " << finalScale << std::endl;
  std::cout << " Angle (radians) " << finalAngle << std::endl;
  std::cout << " Angle (degrees) " << finalAngleInDegrees << std::endl;
  std::cout << " Center X      = " << finalRotationCenterX << std::endl;
  std::cout << " Center Y      = " << finalRotationCenterY << std::endl;
  std::cout << " Translation X = " << finalTranslationX << std::endl;
  std::cout << " Translation Y = " << finalTranslationY << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue << std::endl;


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
  //  \ref{fig:FixedMovingImageRegistration7}. The registration takes $16$
  //  iterations and produces:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [0.833222, -0.174521, 111.437, 131.741, -12.8272, -12.7862]
  //  \end{verbatim}
  //  \end{center}
  //
  //  That are interpreted as
  //
  //  \begin{itemize}
  //  \item Scale factor  =                     $0.833222$
  //  \item Angle         =                     $0.174521$   radians
  //  \item Center        = $( 111.437     , 131.741     )$ millimeters
  //  \item Translation   = $( -12.8272    , -12.7862    )$ millimeters
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
  // CenteredSimilarity2DTransform]{Fixed and Moving image provided as input to the
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
  // \itkcaption[Output of the CenteredSimilarity2DTransform registration]{Resampled
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
  // \itkcaption[CenteredSimilarity2DTransform registration plots]{Plots of the Metric,
  // rotation angle and translations during
  // the registration using
  // Similarity2D transform.}
  // \label{fig:ImageRegistration7Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration7Plots} shows the plots of the main
  //  output parameters of the registration process. The metric values at every
  //  iteration are shown on the top. The angle values are shown in the plot at
  //  left while the translation components of the registration are presented
  //  in the plot at right.
  //


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


  using RescalerType = itk::RescaleIntensityImageFilter<FixedImageType, OutputImageType>;

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
