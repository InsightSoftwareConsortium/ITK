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

#include "itkPeakSignalToNoiseRatioCalculator.h"
#include "itkTestingMacros.h"

int
itkPeakSignalToNoiseRatioCalculatorTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage noisyImage [expectedValue] [tolerance]"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);
  reader2->Update();

  using CalculatorType = itk::PeakSignalToNoiseRatioCalculator<ImageType>;
  auto psnr = CalculatorType::New();
  psnr->SetImage(reader->GetOutput());
  psnr->SetNoisyImage(reader2->GetOutput());
  psnr->Compute();

  if (argc >= 5)
  {
    double expectedValue = std::stod(argv[3]);
    double tolerance = std::stod(argv[4]);

    std::cout << "<DartMeasurement name=\"PSNR\" type=\"numeric/double\">";
    std::cout << psnr->GetOutput();
    std::cout << "</DartMeasurement>" << std::endl;

    if (psnr->GetOutput() - expectedValue > tolerance)
    {
      return EXIT_FAILURE;
    }
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
