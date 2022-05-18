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
//  This example illustrates how to do registration with a 2D Translation
//  Transform, the Normalized Mutual Information metric and the One+One
//  evolutionary optimizer.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"

#include "itkTranslationTransform.h"
#include "itkNormalizedMutualInformationHistogramImageToImageMetric.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkNormalVariateGenerator.h"

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
  CommandIterationUpdate() { m_LastMetricValue = 0; }

public:
  using OptimizerType = itk::OnePlusOneEvolutionaryOptimizer;
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
    double currentValue = optimizer->GetValue();
    // Only print out when the Metric value changes
    if (itk::Math::abs(m_LastMetricValue - currentValue) > 1e-7)
    {
      std::cout << optimizer->GetCurrentIteration() << "   ";
      std::cout << currentValue << "   ";
      std::cout << optimizer->GetFrobeniusNorm() << "   ";
      std::cout << optimizer->GetCurrentPosition() << std::endl;
      m_LastMetricValue = currentValue;
    }
  }

private:
  double m_LastMetricValue;
};


int
main(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile [numberOfHistogramBins] ";
    std::cerr << "[initialRadius] [epsilon] [initialTx] [initialTy]"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using TransformType = itk::TranslationTransform<double, Dimension>;

  using OptimizerType = itk::OnePlusOneEvolutionaryOptimizer;
  using InterpolatorType =
    itk::LinearInterpolateImageFunction<MovingImageType, double>;
  using RegistrationType =
    itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;


  using MetricType =
    itk::NormalizedMutualInformationHistogramImageToImageMetric<
      FixedImageType,
      MovingImageType>;

  auto transform = TransformType::New();
  auto optimizer = OptimizerType::New();
  auto interpolator = InterpolatorType::New();
  auto registration = RegistrationType::New();

  registration->SetOptimizer(optimizer);
  registration->SetTransform(transform);
  registration->SetInterpolator(interpolator);


  auto metric = MetricType::New();
  registration->SetMetric(metric);


  unsigned int numberOfHistogramBins = 32;
  if (argc > 4)
  {
    numberOfHistogramBins = std::stoi(argv[4]);
    std::cout << "Using " << numberOfHistogramBins << " Histogram bins"
              << std::endl;
  }
  MetricType::HistogramType::SizeType histogramSize;
  histogramSize.SetSize(2);
  histogramSize[0] = numberOfHistogramBins;
  histogramSize[1] = numberOfHistogramBins;
  metric->SetHistogramSize(histogramSize);


  const unsigned int numberOfParameters = transform->GetNumberOfParameters();

  using ScalesType = MetricType::ScalesType;
  ScalesType scales(numberOfParameters);

  scales.Fill(1.0);

  metric->SetDerivativeStepLengthScales(scales);

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  registration->SetFixedImage(fixedImageReader->GetOutput());
  registration->SetMovingImage(movingImageReader->GetOutput());

  fixedImageReader->Update();
  movingImageReader->Update();

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();
  registration->SetFixedImageRegion(fixedImage->GetBufferedRegion());

  transform->SetIdentity();
  using ParametersType = RegistrationType::ParametersType;
  ParametersType initialParameters = transform->GetParameters();
  initialParameters[0] = 0.0;
  initialParameters[1] = 0.0;

  if (argc > 8)
  {
    initialParameters[0] = std::stod(argv[7]);
    initialParameters[1] = std::stod(argv[8]);
  }
  registration->SetInitialTransformParameters(initialParameters);

  std::cout << "Initial transform parameters = ";
  std::cout << initialParameters << std::endl;

  using OptimizerScalesType = OptimizerType::ScalesType;
  OptimizerScalesType optimizerScales(transform->GetNumberOfParameters());

  FixedImageType::RegionType  region = fixedImage->GetLargestPossibleRegion();
  FixedImageType::SizeType    size = region.GetSize();
  FixedImageType::SpacingType spacing = fixedImage->GetSpacing();

  optimizerScales[0] = 1.0 / (0.1 * size[0] * spacing[0]);
  optimizerScales[1] = 1.0 / (0.1 * size[1] * spacing[1]);
  optimizer->SetScales(optimizerScales);

  using GeneratorType = itk::Statistics::NormalVariateGenerator;
  auto generator = GeneratorType::New();
  generator->Initialize(12345);

  optimizer->MaximizeOn();
  optimizer->SetNormalVariateGenerator(generator);
  double initialRadius = 0.01;
  if (argc > 5)
  {
    initialRadius = std::stod(argv[5]);
    std::cout << "Using initial radius = " << initialRadius << std::endl;
  }
  optimizer->Initialize(initialRadius);
  double epsilon = 0.001;
  if (argc > 6)
  {
    epsilon = std::stod(argv[6]);
    std::cout << "Using epsilon = " << epsilon << std::endl;
  }
  optimizer->SetEpsilon(epsilon);
  optimizer->SetMaximumIteration(2000);

  // Create the Command observer and register it with the optimizer.
  //
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
  const double   finalTranslationX = finalParameters[0];
  const double   finalTranslationY = finalParameters[1];
  unsigned int   numberOfIterations = optimizer->GetCurrentIteration();
  const double   bestValue = optimizer->GetValue();

  // Print out results
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << finalTranslationX << std::endl;
  std::cout << " Translation Y = " << finalTranslationY << std::endl;
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
  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(100);

  using OutputImageType = itk::Image<PixelType, Dimension>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(resample->GetOutput());
  writer->Update();
  // Software Guide : EndCodeSnippet
  return EXIT_SUCCESS;
}
