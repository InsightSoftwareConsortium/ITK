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

#include "itkMath.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkScalarImageToHistogramGenerator.h"
#include "itkOtsuThresholdCalculator.h"
#include "itkOtsuMultipleThresholdsCalculator.h"
#include "itkTestingMacros.h"


int
itkOtsuThresholdCalculatorVersusOtsuMultipleThresholdsCalculatorTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << itkNameOfTestExecutableMacro(argv) << std::endl;
    std::cerr << " inputImageFile" << std::endl;
    return EXIT_FAILURE;
  }

  // Set up an OtsuThresholdCalculator and an OtsuMultipleThresholdsCalculator.
  // Set the OtsuMultipleThresholdsCalculator to have only one threshold.
  // Then test the computed thresholds for the two filters for various numbers of
  // histogram bins.
  // The two algorithms should output the same result.

  constexpr unsigned int ImageDimension = 2;
  using InputImageType = itk::Image<unsigned short, ImageDimension>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  int numberOfThresholds = 1;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  using HistogramGeneratorType = itk::Statistics::ScalarImageToHistogramGenerator<InputImageType>;
  using HistogramType = HistogramGeneratorType::HistogramType;
  auto histogramGenerator = HistogramGeneratorType::New();
  histogramGenerator->SetInput(reader->GetOutput());

  // Compute the OtsuThreshold for the input image.
  using OtsuCalculatorType = itk::OtsuThresholdCalculator<HistogramType>;
  auto otsuCalculator = OtsuCalculatorType::New();
  otsuCalculator->SetInput(histogramGenerator->GetOutput());

  // Compute the OtsuMultipleThresholds for the input image
  using OtsuMultipleCalculatorType = itk::OtsuMultipleThresholdsCalculator<HistogramType>;
  auto otsuMultipleCalculator = OtsuMultipleCalculatorType::New();
  otsuMultipleCalculator->SetInputHistogram(histogramGenerator->GetOutput());
  otsuMultipleCalculator->SetNumberOfThresholds(numberOfThresholds);

  static constexpr int binsArray[] = { 4, 8, 16, 32, 64, 128, 256, 512, 1024 };
  std::vector<int>     binsVector(binsArray, binsArray + sizeof(binsArray) / sizeof(binsArray[0]));
  for (const auto & binsIterator : binsVector)
  {
    histogramGenerator->SetNumberOfBins(binsIterator);
    histogramGenerator->Compute();

    otsuCalculator->Compute();

    if (itk::Math::NotAlmostEquals(otsuCalculator->GetThreshold(), otsuMultipleCalculator->GetOutput()[0]))
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr
        << "Error in itk::OtsuThresholdCalculator::GetThreshold() or itk::OtsuMultipleThresholdsCalculator::GetOutput()"
        << std::endl;
      std::cout << "Computed Otsu threshold: " << otsuCalculator->GetThreshold()
                << " is different from computed Otsu multiple threshold: " << otsuMultipleCalculator->GetOutput()[0]
                << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
