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

// Software Guide : BeginLatex
//
//  This example illustrates how to do registration with a 2D Rigid Transform
//  and with MutualInformation metric.
//
// Software Guide : EndLatex


#include "itkImageRegistrationMethodv4.h"

#include "itkEuler2DTransform.h"
#include "itkCenteredTransformInitializer.h"

// Software Guide : BeginCodeSnippet
#include "itkMattesMutualInformationImageToImageMetricv4.h"
// Software Guide : EndCodeSnippet

#include "itkRegularStepGradientDescentOptimizerv4.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"


//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
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
  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = float;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  // Software Guide : BeginLatex
  //
  // The Euler2DTransform applies a rigid transform in 2D space.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using TransformType = itk::Euler2DTransform<double>;
  // Software Guide : EndCodeSnippet

  using OptimizerType = itk::RegularStepGradientDescentOptimizerv4<double>;

  using RegistrationType = itk::
    ImageRegistrationMethodv4<FixedImageType, MovingImageType, TransformType>;


  // Software Guide : BeginCodeSnippet
  using MetricType =
    itk::MattesMutualInformationImageToImageMetricv4<FixedImageType,
                                                     MovingImageType>;
  // Software Guide : EndCodeSnippet

  auto transform = TransformType::New();
  auto metric = MetricType::New();
  auto optimizer = OptimizerType::New();
  auto registration = RegistrationType::New();

  registration->SetOptimizer(optimizer);
  registration->SetMetric(metric);

  // For consistent results when regression testing.
  registration->MetricSamplingReinitializeSeed(121212);

  // Software Guide : BeginCodeSnippet
  metric->SetNumberOfHistogramBins(20);


  double samplingPercentage = 0.20;
  registration->SetMetricSamplingPercentage(samplingPercentage);

  RegistrationType::MetricSamplingStrategyEnum samplingStrategy =
    RegistrationType::MetricSamplingStrategyEnum::RANDOM;
  registration->SetMetricSamplingStrategy(samplingStrategy);
  // Software Guide : EndCodeSnippet


  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  registration->SetFixedImage(fixedImageReader->GetOutput());
  registration->SetMovingImage(movingImageReader->GetOutput());

  fixedImageReader->Update();

  // Software Guide : BeginLatex
  //
  // The \doxygen{Euler2DTransform} is initialized with 3 parameters,
  // indicating the angle of rotation and the
  // translation to be applied after rotation. The initialization is done
  // by the \doxygen{CenteredTransformInitializer}.
  // The transform initializer can operate in two modes, the first of
  // which assumes that the
  // anatomical objects to be registered are centered in their respective
  // images. Hence the best initial guess for the registration is the one
  // that superimposes those two centers.
  // This second approach assumes that the moments of the anatomical
  // objects are similar for both images and hence the best initial guess
  // for registration is to superimpose both mass centers. The center of
  // mass is computed from the moments obtained from the gray level values.
  // Here we adopt the first approach. The \code{GeometryOn()} method
  // toggles between the approaches.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using TransformInitializerType =
    itk::CenteredTransformInitializer<TransformType,
                                      FixedImageType,
                                      MovingImageType>;
  auto initializer = TransformInitializerType::New();
  initializer->SetTransform(transform);

  initializer->SetFixedImage(fixedImageReader->GetOutput());
  initializer->SetMovingImage(movingImageReader->GetOutput());
  initializer->GeometryOn();
  initializer->InitializeTransform();
  // Software Guide : EndCodeSnippet

  transform->SetAngle(0.0);

  registration->SetInitialTransform(transform);
  registration->InPlaceOn();

  // Software Guide : BeginLatex
  //
  // The optimizer scales the metrics (the gradient in this case) by the
  // scales during each iteration. Here we
  // assume that the fixed and moving images are likely to be related by
  // a translation.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using OptimizerScalesType = OptimizerType::ScalesType;
  OptimizerScalesType optimizerScales(transform->GetNumberOfParameters());

  const double translationScale = 1.0 / 128.0;

  optimizerScales[0] = 1.0;
  optimizerScales[1] = translationScale;
  optimizerScales[2] = translationScale;

  optimizer->SetScales(optimizerScales);

  optimizer->SetLearningRate(0.5);
  optimizer->SetMinimumStepLength(0.0001);
  optimizer->SetNumberOfIterations(400);
  // Software Guide : EndCodeSnippet

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

  // Create the Command observer and register it with the optimizer.
  //
  auto observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);


  try
  {
    registration->Update();
    std::cout << "Optimizer stop condition = "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }

  using ParametersType = TransformType::ParametersType;
  ParametersType finalParameters = transform->GetParameters();

  const double finalAngle = finalParameters[0];
  const double finalTranslationX = finalParameters[1];
  const double finalTranslationY = finalParameters[2];

  const double rotationCenterX =
    registration->GetOutput()->Get()->GetFixedParameters()[0];
  const double rotationCenterY =
    registration->GetOutput()->Get()->GetFixedParameters()[1];

  unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  double bestValue = optimizer->GetValue();

  // Print out results
  //

  const double finalAngleInDegrees = finalAngle * 180 / itk::Math::pi;

  std::cout << "Result = " << std::endl;
  std::cout << " Angle (radians) " << finalAngle << std::endl;
  std::cout << " Angle (degrees) " << finalAngleInDegrees << std::endl;
  std::cout << " Translation X  = " << finalTranslationX << std::endl;
  std::cout << " Translation Y  = " << finalTranslationY << std::endl;
  std::cout << " Fixed Center X = " << rotationCenterX << std::endl;
  std::cout << " Fixed Center Y = " << rotationCenterY << std::endl;
  std::cout << " Iterations     = " << numberOfIterations << std::endl;
  std::cout << " Metric value   = " << bestValue << std::endl;


  using ResampleFilterType =
    itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  auto resample = ResampleFilterType::New();

  resample->SetTransform(transform);
  resample->SetInput(movingImageReader->GetOutput());

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(100);

  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using CastFilterType =
    itk::CastImageFilter<FixedImageType, OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();
  auto caster = CastFilterType::New();

  writer->SetFileName(argv[3]);

  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}

//  Software Guide : BeginLatex
//
//  Let's execute this example over some of the images provided in
//  \code{Examples/Data}, for example:
//
//  \begin{itemize}
//  \item \code{BrainProtonDensitySlice.png}
//  \item \code{BrainProtonDensitySliceR10X13Y17.png}
//  \end{itemize}
//
//  The second image is the result of intentionally rotating the first
//  image by $10$ degrees and shifting it $13mm$ in $X$ and $17mm$ in
//  $Y$. Both images have unit-spacing and are shown in Figure
//  \ref{fig:FixedMovingImageRegistration5}. The example
//  yielded the following results.
//
//  \begin{verbatim}
//
//  Angle (radians) 0.174569
//  Angle (degrees) 10.0021
//  Translation X = 13.0958
//  Translation Y = 15.9156
//
//  \end{verbatim}
//
//  These values match the true misalignment introduced in the moving image.
//
//  Software Guide : EndLatex
