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

// Software Guide : BeginLatex
//
// This example illustrates how to combine the MutualInformation metric with
// an Evolutionary algorithm for optimization.  Evolutionary algorithms are
// naturally well-suited for optimizing the Mutual Information metric given
// its random and noisy behavior.
//
// The structure of the example is almost identical to the one illustrated in
// ImageRegistration4. Therefore we focus here on the setup that is
// specifically required for the evolutionary optimizer.
//
//
// \index{itk::ImageRegistrationMethodv4!Multi-Modality}
// \index{itk::OnePlusOneEvolutionaryOptimizerv4!Multi-Modality}
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethodv4.h"
#include "itkTranslationTransform.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkOnePlusOneEvolutionaryOptimizerv4.h"
#include "itkNormalVariateGenerator.h"
// Software Guide : EndCodeSnippet


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
  CommandIterationUpdate() { m_LastMetricValue = 0.0; };

public:
  using OptimizerType = itk::OnePlusOneEvolutionaryOptimizerv4<double>;
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
    if (std::fabs(m_LastMetricValue - currentValue) > 1e-7)
    {
      std::cout << optimizer->GetCurrentIteration() << "   ";
      std::cout << currentValue << "   ";
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
  if (argc < 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile ";
    std::cerr << "[samplingPercentage ] " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = float;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using TransformType = itk::TranslationTransform<double, Dimension>;
  using OptimizerType = itk::OnePlusOneEvolutionaryOptimizerv4<double>;
  using RegistrationType =
    itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType>;

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
  using MetricType =
    itk::MattesMutualInformationImageToImageMetricv4<FixedImageType,
                                                     MovingImageType>;
  // Software Guide : EndCodeSnippet

  TransformType::Pointer    transform = TransformType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  MetricType::Pointer       metric = MetricType::New();
  RegistrationType::Pointer registration = RegistrationType::New();

  registration->SetOptimizer(optimizer);
  registration->SetMetric(metric);

  //  Software Guide : BeginLatex
  //
  // The histogram bins metric parameter is set as follows.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  metric->SetNumberOfHistogramBins(20);
  // Software Guide : EndCodeSnippet

  double samplingPercentage = 0.20;
  if (argc > 4)
  {
    samplingPercentage = std::stod(argv[4]);
  }

  //  Software Guide : BeginLatex
  //
  //  As our previous discussion in section
  //  ~\ref{sec:MultiModalityRegistrationMattes}, only a subsample of the
  //  virtual domain is needed to evaluate the metric. The number of spatial
  //  samples to be used depends on the content of the image, and the user can
  //  define the sampling percentage and the way that sampling operation is
  //  managed by the registration framework as follows. Sampling startegy can
  //  can be defined as \code{REGULAR} or \code{RANDOM}, while the default
  //  value is \code{NONE}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetMetricSamplingPercentage(samplingPercentage);

  RegistrationType::MetricSamplingStrategyEnum samplingStrategy =
    RegistrationType::MetricSamplingStrategyEnum::RANDOM;
  registration->SetMetricSamplingStrategy(samplingStrategy);
  // Software Guide : EndCodeSnippet

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  FixedImageReaderType::Pointer fixedImageReader =
    FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader =
    MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  registration->SetFixedImage(fixedImageReader->GetOutput());
  registration->SetMovingImage(movingImageReader->GetOutput());

  fixedImageReader->Update();


  using ParametersType = TransformType::ParametersType;
  ParametersType initialParameters(transform->GetNumberOfParameters());

  initialParameters[0] = 0.0; // Initial offset in mm along X
  initialParameters[1] = 0.0; // Initial offset in mm along Y

  transform->SetParameters(initialParameters);

  registration->SetInitialTransform(transform);
  registration->InPlaceOn();

  //  Software Guide : BeginLatex
  //
  //  Evolutionary algorithms are based on testing random variations
  //  of parameters. In order to support the computation of random values,
  //  ITK provides a family of random number generators. In this example, we
  //  use the \doxygen{NormalVariateGenerator} which generates values with a
  //  normal distribution.
  //
  //  \index{itk::NormalVariateGenerator!New()}
  //  \index{itk::NormalVariateGenerator!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using GeneratorType = itk::Statistics::NormalVariateGenerator;

  GeneratorType::Pointer generator = GeneratorType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The random number generator must be initialized with a seed.
  //
  //  \index{itk::NormalVariateGenerator!Initialize()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  generator->Initialize(12345);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Now we set the optimizer parameters.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetNormalVariateGenerator(generator);
  optimizer->Initialize(10);
  optimizer->SetEpsilon(1.0);
  optimizer->SetMaximumIteration(4000);
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
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);


  try
  {
    registration->Update();
    std::cout << "Registration completed!" << std::endl;
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

  ParametersType finalParameters = transform->GetParameters();

  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];

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
  //  This example is executed using the same multi-modality images as
  //  in the previous one.  The registration converges after $24$ iterations
  //  and produces the following results:
  //
  //  \begin{verbatim}
  //  Translation X = 13.1719
  //  Translation Y = 16.9006
  //  \end{verbatim}
  //  These values are a very close match to
  //  the true misalignment introduced in the moving image.
  //
  //  Software Guide : EndLatex


  using ResampleFilterType =
    itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

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

  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();

  writer->SetFileName(argv[3]);

  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
