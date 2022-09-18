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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


#include "itkEuler2DTransform.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkTestingMacros.h"
#include "itkImageRegistrationMethodv4.h"
#include "itkConjugateGradientLineSearchOptimizerv4.h"

#include <iomanip>

namespace
{
template <typename TOptimizer>
class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  CommandIterationUpdate()
  {
    // mark used to avoid warnings
    (void)&Self::Clone;
  };

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {

    const auto * optimizer = dynamic_cast<const TOptimizer *>(object);

    if (typeid(event) != typeid(itk::IterationEvent) || !optimizer)
    {
      return;
    }

    // stash the stream state
    std::ios state(nullptr);
    state.copyfmt(std::cout);
    std::cout << std::fixed << std::setfill(' ') << std::setprecision(5);
    std::cout << std::setw(3) << optimizer->GetCurrentIteration();
    std::cout << " = " << std::setw(10) << optimizer->GetCurrentMetricValue();
    std::cout << " : " << optimizer->GetCurrentPosition() << std::endl;
    std::cout << "\nScales: " << optimizer->GetScales() << std::endl;
  }
};

template <unsigned int TDimension>
int
ImageRegistration(int itkNotUsed(argc), char * argv[])
{
  const unsigned int ImageDimension = TDimension;


  using PixelType = float;
  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;

  using ImageReaderType = itk::ImageFileReader<FixedImageType>;

  auto fixedImageReader = ImageReaderType::New();
  fixedImageReader->SetFileName(argv[2]);
  fixedImageReader->Update();
  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  fixedImage->Update();
  fixedImage->DisconnectPipeline();

  auto movingImageReader = ImageReaderType::New();
  movingImageReader->SetFileName(argv[3]);
  movingImageReader->Update();
  typename MovingImageType::Pointer movingImage = movingImageReader->GetOutput();
  movingImage->Update();
  movingImage->DisconnectPipeline();

  // Set up the centered transform initializer
  using TransformType = itk::Euler2DTransform<double>;
  auto initialTransform = TransformType::New();


  using MetricType = itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType>;
  auto metric = MetricType::New();

  using RegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, TransformType>;
  auto registration = RegistrationType::New();

  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImage);
  registration->SetMetric(metric);
  registration->SetMovingInitialTransform(initialTransform);
  registration->SetNumberOfLevels(1);

  using Optimizerv4Type = itk::ConjugateGradientLineSearchOptimizerv4;
  auto optimizer = Optimizerv4Type::New();

  optimizer->SetLearningRate(1.0);
  optimizer->SetNumberOfIterations(100);
  optimizer->SetMinimumConvergenceValue(1e-5);
  optimizer->SetConvergenceWindowSize(2);

  double                                           scaleData[] = { 200000, 1.0, 1.0 };
  typename Optimizerv4Type::ScalesType::Superclass scales(scaleData, 3);
  optimizer->SetScales(scales);

  registration->SetOptimizer(optimizer);

  using CommandType = CommandIterationUpdate<Optimizerv4Type>;
  auto observer = CommandType::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  ITK_TRY_EXPECT_NO_EXCEPTION(registration->Update());


  registration->GetTransform()->Print(std::cout);
  std::cout << optimizer->GetStopConditionDescription() << std::endl;
  typename TransformType::ParametersType results = registration->GetTransform()->GetParameters();

  std::cout << "Expecting close (+/- 0.3) to: ( 0.0, -2.8, 9.5 )" << std::endl;
  std::cout << "Parameters: " << results << std::endl;

  std::cout << "Number Of Iterations: " << optimizer->GetCurrentIteration();
  ITK_TEST_EXPECT_TRUE(optimizer->GetCurrentIteration() > 5);

  return EXIT_SUCCESS;
}
} // namespace

int
itkSimpleImageRegistrationTest4(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " imageDimension fixedImage movingImage" << std::endl;
    return EXIT_FAILURE;
  }

  switch (std::stoi(argv[1]))
  {
    case 2:
      return ImageRegistration<2>(argc, argv);

    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return EXIT_FAILURE;
  }
}
