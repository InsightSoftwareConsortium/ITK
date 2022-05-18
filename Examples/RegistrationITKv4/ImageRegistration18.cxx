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
// This example illustrates how to use the
// \doxygen{GradientDifferenceImageToImageMetric}.
//
// This metric is particularly useful for registration scenarios where fitting
// the edges of both images is the most relevant criteria for registration
// success.
//
// \index{itk::ImageRegistrationMethod!Monitoring}
//
//
// Software Guide : EndLatex


#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkGradientDifferenceImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

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
    std::cout << optimizer->GetCurrentIteration() << " = ";
    std::cout << optimizer->GetValue() << " : ";
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
    std::cerr << "outputImagefile" << std::endl;
    std::cerr << "[initialTx] [initialTy]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned short;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using TransformType = itk::TranslationTransform<double, Dimension>;

  using OptimizerType = itk::RegularStepGradientDescentOptimizer;

  using InterpolatorType =
    itk::LinearInterpolateImageFunction<MovingImageType, double>;

  using RegistrationType =
    itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;

  using MetricType =
    itk::GradientDifferenceImageToImageMetric<FixedImageType,
                                              MovingImageType>;

  auto transform = TransformType::New();
  auto optimizer = OptimizerType::New();
  auto interpolator = InterpolatorType::New();
  auto registration = RegistrationType::New();

  registration->SetOptimizer(optimizer);
  registration->SetTransform(transform);
  registration->SetInterpolator(interpolator);

  auto metric = MetricType::New();

  metric->SetDerivativeDelta(0.5);

  registration->SetMetric(metric);

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  registration->SetFixedImage(fixedImageReader->GetOutput());
  registration->SetMovingImage(movingImageReader->GetOutput());

  fixedImageReader
    ->Update(); // This is needed to make the BufferedRegion below valid.

  registration->SetFixedImageRegion(
    fixedImageReader->GetOutput()->GetBufferedRegion());

  using ParametersType = RegistrationType::ParametersType;
  ParametersType initialParameters(transform->GetNumberOfParameters());

  initialParameters[0] = 0.0; // Initial offset in mm along X
  initialParameters[1] = 0.0; // Initial offset in mm along Y

  if (argc > 4)
  {
    initialParameters[0] = std::stod(argv[4]);
  }

  if (argc > 5)
  {
    initialParameters[1] = std::stod(argv[5]);
  }

  std::cout << "Initial parameters = " << initialParameters << std::endl;

  registration->SetInitialTransformParameters(initialParameters);

  optimizer->SetMaximumStepLength(4.00);
  optimizer->SetMinimumStepLength(0.01);
  optimizer->SetNumberOfIterations(200);
  optimizer->SetGradientMagnitudeTolerance(1e-40);

  optimizer->MaximizeOn();


  auto observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

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


  ParametersType finalParameters = registration->GetLastTransformParameters();

  const double TranslationAlongX = finalParameters[0];
  const double TranslationAlongY = finalParameters[1];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();

  std::cout << "Registration done !" << std::endl;
  std::cout << "Optimizer stop condition = "
            << registration->GetOptimizer()->GetStopConditionDescription()
            << std::endl;
  std::cout << "Number of iterations = " << numberOfIterations << std::endl;
  std::cout << "Translation along X  = " << TranslationAlongX << std::endl;
  std::cout << "Translation along Y  = " << TranslationAlongY << std::endl;
  std::cout << "Optimal metric value = " << bestValue << std::endl;


  // Prepare the resampling filter in order to map the moving image.
  //
  using ResampleFilterType =
    itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  auto finalTransform = TransformType::New();

  finalTransform->SetParameters(finalParameters);
  finalTransform->SetFixedParameters(transform->GetFixedParameters());

  auto resample = ResampleFilterType::New();

  resample->SetTransform(finalTransform);
  resample->SetInput(movingImageReader->GetOutput());

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(100);


  // Prepare a writer and caster filters to send the resampled moving image to
  // a file
  //
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
