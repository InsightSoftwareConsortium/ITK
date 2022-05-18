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

//
// This example is the 3D version of the 2D example in
// MultiResImageRegistration1.cxx
//

#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkCheckerBoardImageFilter.h"


#include "itkCommand.h"


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
    if (registration == nullptr)
    {
      return;
    }
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


//  The following section of code implements an observer
//  that will monitor the evolution of the registration process.
//
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
    std::cerr << " outputImagefile [backgroundGrayLevel]";
    std::cerr << " [checkerBoardBefore] [checkerBoardAfter]";
    std::cerr << " [useExplicitPDFderivatives ] " << std::endl;
    std::cerr << " [numberOfBins] [numberOfSamples ] " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using PixelType = unsigned short;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using InternalPixelType = float;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;

  using TransformType = itk::TranslationTransform<double, Dimension>;
  using OptimizerType = itk::RegularStepGradientDescentOptimizer;
  using InterpolatorType =
    itk::LinearInterpolateImageFunction<InternalImageType, double>;
  using MetricType =
    itk::MattesMutualInformationImageToImageMetric<InternalImageType,
                                                   InternalImageType>;
  using RegistrationType =
    itk::MultiResolutionImageRegistrationMethod<InternalImageType,
                                                InternalImageType>;

  using FixedImagePyramidType =
    itk::MultiResolutionPyramidImageFilter<InternalImageType,
                                           InternalImageType>;
  using MovingImagePyramidType =
    itk::MultiResolutionPyramidImageFilter<InternalImageType,
                                           InternalImageType>;


  //  All the components are instantiated using their \code{New()} method
  //  and connected to the registration object as in previous example.
  //
  auto transform = TransformType::New();
  auto optimizer = OptimizerType::New();
  auto interpolator = InterpolatorType::New();
  auto registration = RegistrationType::New();
  auto metric = MetricType::New();

  auto fixedImagePyramid = FixedImagePyramidType::New();
  auto movingImagePyramid = MovingImagePyramidType::New();

  registration->SetOptimizer(optimizer);
  registration->SetTransform(transform);
  registration->SetInterpolator(interpolator);
  registration->SetMetric(metric);
  registration->SetFixedImagePyramid(fixedImagePyramid);
  registration->SetMovingImagePyramid(movingImagePyramid);


  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);


  using FixedCastFilterType =
    itk::CastImageFilter<FixedImageType, InternalImageType>;
  using MovingCastFilterType =
    itk::CastImageFilter<MovingImageType, InternalImageType>;

  auto fixedCaster = FixedCastFilterType::New();
  auto movingCaster = MovingCastFilterType::New();

  fixedCaster->SetInput(fixedImageReader->GetOutput());
  movingCaster->SetInput(movingImageReader->GetOutput());

  registration->SetFixedImage(fixedCaster->GetOutput());
  registration->SetMovingImage(movingCaster->GetOutput());


  fixedCaster->Update();

  registration->SetFixedImageRegion(
    fixedCaster->GetOutput()->GetBufferedRegion());


  using ParametersType = RegistrationType::ParametersType;
  ParametersType initialParameters(transform->GetNumberOfParameters());

  initialParameters[0] = 0.0; // Initial offset in mm along X
  initialParameters[1] = 0.0; // Initial offset in mm along Y
  initialParameters[2] = 0.0; // Initial offset in mm along Z

  registration->SetInitialTransformParameters(initialParameters);

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

  metric->ReinitializeSeed(76926294);


  if (argc > 7)
  {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the
    // Transform parameters, or doing it by progressively accumulating
    // contributions from each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives(std::stoi(argv[7]));
  }


  optimizer->SetNumberOfIterations(200);
  optimizer->SetRelaxationFactor(0.9);


  // Create the Command observer and register it with the optimizer.
  //
  auto observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  using CommandType = RegistrationInterfaceCommand<RegistrationType>;
  auto command = CommandType::New();
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

  ParametersType finalParameters = registration->GetLastTransformParameters();

  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];
  double TranslationAlongZ = finalParameters[2];

  unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  double bestValue = optimizer->GetValue();


  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY << std::endl;
  std::cout << " Translation Z = " << TranslationAlongZ << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue << std::endl;

  using ResampleFilterType =
    itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  auto finalTransform = TransformType::New();

  finalTransform->SetParameters(finalParameters);
  finalTransform->SetFixedParameters(transform->GetFixedParameters());

  auto resample = ResampleFilterType::New();

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


  auto writer = WriterType::New();
  auto caster = CastFilterType::New();


  writer->SetFileName(argv[3]);


  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  //
  // Generate checkerboards before and after registration
  //
  using CheckerBoardFilterType = itk::CheckerBoardImageFilter<FixedImageType>;

  auto checker = CheckerBoardFilterType::New();

  checker->SetInput1(fixedImage);
  checker->SetInput2(resample->GetOutput());

  caster->SetInput(checker->GetOutput());
  writer->SetInput(caster->GetOutput());

  resample->SetDefaultPixelValue(0);

  // Before registration
  auto identityTransform = TransformType::New();
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
