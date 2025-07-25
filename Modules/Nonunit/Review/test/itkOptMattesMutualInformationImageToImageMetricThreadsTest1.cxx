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
#include "itkTranslationTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkTestingMacros.h"

int
itkOptMattesMutualInformationImageToImageMetricThreadsTest1(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " fixedImage movingImage [verbose(1/0)] [numberOfSamples]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "OPTIMIZED ON" << std::endl;

  const unsigned int maximumNumberOfThreads = itk::MultiThreaderBase::GetGlobalMaximumNumberOfThreads();
  const unsigned int defaultNumberOfThreads = itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads();

  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << " Global Maximum Number of Threads " << maximumNumberOfThreads << std::endl;
  std::cout << " Global Default Number of Threads " << defaultNumberOfThreads << std::endl;
  std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
  std::cout << std::endl;


  using PixelType = unsigned char;
  constexpr unsigned int Dimension = 2;

  using ImageType = itk::Image<PixelType>;

  using ImageReaderType = itk::ImageFileReader<ImageType>;

  auto fixedImageReader = ImageReaderType::New();
  auto movingImageReader = ImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  bool verbose = false;

  if (argc > 3)
  {
    verbose = std::stoi(argv[3]);
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(fixedImageReader->Update());
  ITK_TRY_EXPECT_NO_EXCEPTION(movingImageReader->Update());


  using InterpolatorType = itk::NearestNeighborInterpolateImageFunction<ImageType>;

  auto interpolator = InterpolatorType::New();

  using MetricType = itk::MattesMutualInformationImageToImageMetric<ImageType, ImageType>;
  auto metric = MetricType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(metric, MattesMutualInformationImageToImageMetric, ImageToImageMetric);


  using TranformType = itk::TranslationTransform<double, Dimension>;
  auto transform = TranformType::New();

  unsigned int numberOfSamples = 100;

  if (argc > 4)
  {
    numberOfSamples = std::stoi(argv[4]);
  }
  metric->SetNumberOfFixedImageSamples(numberOfSamples);

  metric->SetTransform(transform);
  metric->SetInterpolator(interpolator);
  metric->SetFixedImage(fixedImageReader->GetOutput());
  metric->SetMovingImage(movingImageReader->GetOutput());
  metric->SetFixedImageRegion(fixedImageReader->GetOutput()->GetBufferedRegion());

  MetricType::TransformParametersType displacement(Dimension);

  displacement[0] = 17;
  displacement[1] = 19;

  using MeasureType = MetricType::MeasureType;
  using DerivativeType = MetricType::DerivativeType;

  MeasureType    value_combined = NAN;
  DerivativeType derivative_combined;

  MeasureType    value_separate = NAN;
  DerivativeType derivative_separate;

  std::vector<MeasureType>    values;
  std::vector<DerivativeType> derivatives;

  // By now restrict the number of threads to test to the range 1 to 4.
  const unsigned int maximumNumberOfThreadsToTest = defaultNumberOfThreads;

  for (unsigned int numberOfWorkUnits = 1; numberOfWorkUnits < maximumNumberOfThreadsToTest; ++numberOfWorkUnits)
  {
    try
    {
      metric->SetNumberOfWorkUnits(numberOfWorkUnits);
      metric->ReinitializeSeed(76926294);
      metric->Initialize();


      value_separate = metric->GetValue(displacement);
      metric->GetDerivative(displacement, derivative_separate);
      metric->GetValueAndDerivative(displacement, value_combined, derivative_combined);
    }
    catch (const itk::ExceptionObject & excep)
    {
      std::cerr << excep << std::endl;
      return EXIT_FAILURE;
    }

    values.push_back(value_combined);
    derivatives.push_back(derivative_combined);

    if (verbose)
    {
      std::cout << numberOfWorkUnits;
      std::cout << " : " << value_combined;
      std::cout << " : " << value_separate;
      std::cout << " : " << derivative_combined;
      std::cout << " : " << derivative_separate << std::endl;
      std::cout << std::endl << std::endl;
    }
  }

  bool testFailed = false;

  constexpr double tolerance = 1e-7;

  for (unsigned int i = 0; i < values.size(); ++i)
  {
    for (unsigned int j = i; j < values.size(); ++j)
    {
      const double difference = values[i] - values[j];

      if (itk::Math::abs(difference) > tolerance)
      {
        if (verbose)
        {
          std::cerr << i + 1 << "thread vs. " << j + 1;
          std::cerr << " thread differ by " << difference;
          std::cerr << " from " << values[i];
          std::cerr << " to " << values[j];
          std::cerr << "   ## Derivatives " << derivatives[i] << " vs. " << derivatives[j];
          std::cerr << std::endl;
        }
        testFailed = true;
      }
    }
  }

  if (testFailed)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
