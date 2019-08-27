/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImageFile ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  // Set up an OtsuThresholdCalculator and an OtsuMultipleThresholdsCalculator.
  // Set the OtsuMultipleThresholdsCalculator to have only one threshold.
  // Then test the computed thresholds for the two filters for various numbers of
  // histogram bins.
  // The two algorithms should output the same result.
  std::string inputImageName = argv[1];
  int         numberOfThresholds = 1;

  constexpr unsigned int ImageDimension = 2;
  using InputImageType = itk::Image<unsigned short, ImageDimension>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImageName);
  reader->Update();

  using HistogramGeneratorType = itk::Statistics::ScalarImageToHistogramGenerator<InputImageType>;
  using HistogramType = HistogramGeneratorType::HistogramType;
  HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();
  histogramGenerator->SetInput(reader->GetOutput());

  // Compute the OtsuThreshold for the input image.
  using OtsuCalculatorType = itk::OtsuThresholdCalculator<HistogramType>;
  OtsuCalculatorType::Pointer otsuCalculator = OtsuCalculatorType::New();
  otsuCalculator->SetInput(histogramGenerator->GetOutput());

  // Compute the OtsuMultipleThresholds for the input image
  using OtsuMultipleCalculatorType = itk::OtsuMultipleThresholdsCalculator<HistogramType>;
  OtsuMultipleCalculatorType::Pointer otsuMultipleCalculator = OtsuMultipleCalculatorType::New();
  otsuMultipleCalculator->SetInputHistogram(histogramGenerator->GetOutput());
  otsuMultipleCalculator->SetNumberOfThresholds(numberOfThresholds);

  static constexpr int binsArray[] = { 4, 8, 16, 32, 64, 128, 256, 512, 1024 };
  std::vector<int>     binsVector(binsArray, binsArray + sizeof(binsArray) / sizeof(binsArray[0]));
  for (const auto & binsIterator : binsVector)
  {
    histogramGenerator->SetNumberOfBins(binsIterator);
    histogramGenerator->Compute();

    otsuCalculator->Compute();
    otsuMultipleCalculator->Compute();
    std::cout << "Computed Otsu threshold using " << binsIterator << " bins: " << otsuCalculator->GetThreshold()
              << std::endl;
    std::cout << "Computed Otsu multiple threshold using " << binsIterator
              << " bins: " << otsuMultipleCalculator->GetOutput()[0] << std::endl;

    if (itk::Math::NotAlmostEquals(otsuCalculator->GetThreshold(), otsuMultipleCalculator->GetOutput()[0]))
    {
      std::cout << "Computed Otsu threshold (" << otsuCalculator->GetThreshold()
                << ") is different from computed Otsu multiple threshold (" << otsuMultipleCalculator->GetOutput()[0]
                << ")" << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
