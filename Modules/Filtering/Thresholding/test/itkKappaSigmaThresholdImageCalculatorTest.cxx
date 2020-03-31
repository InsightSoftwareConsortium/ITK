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

#include "itkKappaSigmaThresholdImageCalculator.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"


int
itkKappaSigmaThresholdImageCalculatorTest(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImage"
              << " maskValue"
              << " sigmaFactor"
              << " numberOfIterations"
              << " expectedThreshold" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = signed short;

  using ImageType = itk::Image<PixelType, Dimension>;
  using MaskType = itk::Image<unsigned char, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());
  // Create and initialize the calculator
  using CalculatorType = itk::KappaSigmaThresholdImageCalculator<ImageType, MaskType>;
  CalculatorType::Pointer calculator = CalculatorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(calculator, KappaSigmaThresholdImageCalculator, Object);


  auto maskValue = static_cast<CalculatorType::MaskPixelType>(std::stod(argv[3]));
  calculator->SetMaskValue(maskValue);
  ITK_TEST_SET_GET_VALUE(maskValue, calculator->GetMaskValue());

  auto sigmaFactor = static_cast<double>(std::stod(argv[4]));
  calculator->SetSigmaFactor(sigmaFactor);
  ITK_TEST_SET_GET_VALUE(sigmaFactor, calculator->GetSigmaFactor());

  auto numberOfIterations = static_cast<unsigned>(std::stoi(argv[5]));
  calculator->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, calculator->GetNumberOfIterations());

  calculator->SetImage(reader->GetOutput());

  calculator->Compute();

  // Regression test: compare computed threshold
  CalculatorType::InputPixelType expectedThreshold = std::stod(argv[5]);
  CalculatorType::InputPixelType resultThreshold = calculator->GetOutput();
  double                         tolerance = 1e-3;
  if (!itk::Math::FloatAlmostEqual((double)expectedThreshold, (double)resultThreshold, 10, tolerance))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetOutput()" << std::endl;
    std::cerr << "Expected: " << itk::NumericTraits<CalculatorType::InputPixelType>::PrintType(expectedThreshold)
              << ", but got: " << itk::NumericTraits<CalculatorType::InputPixelType>::PrintType(resultThreshold)
              << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
