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
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkSobelOperator.h"
#include "itkTestingMacros.h"
#include "itkRescaleIntensityImageFilter.h"

int
itkSobelOperatorImageFilterTest(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName direction outputFileName"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using SobelPixelType = int16_t;
  using SobelImageType = itk::Image<SobelPixelType, Dimension>;

  using InputImagePixeltype = int8_t;
  using InputImageType = itk::Image<InputImagePixeltype, Dimension>;
  using FilerType = itk::NeighborhoodOperatorImageFilter<InputImageType, SobelImageType>;
  auto filter = FilerType::New();
  {
    const auto inputImage = itk::ReadImage<InputImageType>(argv[1]);
    filter->SetInput(inputImage);
  }
  {
    using SobelOperatorType = itk::SobelOperator<SobelPixelType, Dimension>;
    SobelOperatorType sobelOperator;
    {
      auto direction = std::stoul(argv[2]);
      sobelOperator.SetDirection(direction);

      itk::Size<Dimension> radius;
      radius.Fill(1);
      sobelOperator.CreateToRadius(radius);
    }
    filter->SetOperator(sobelOperator);
  }
  filter->Update();

  using OutputImageType = itk::Image<uint8_t, Dimension>;
  // Assume min/max values are approximately +/- same magnitude so that the output images
  // to be stored in uint8_t have an implied 0 at about pixel value 128.  Many web based viewers
  // for the difference images in the testing outputs render better in this positive png range.
  using RescaleIntensityType = itk::RescaleIntensityImageFilter<SobelImageType, OutputImageType>;
  RescaleIntensityType::Pointer rescalerForVisualization = RescaleIntensityType::New();
  rescalerForVisualization->SetInput(filter->GetOutput());
  rescalerForVisualization->SetOutputMinimum(0);
  rescalerForVisualization->SetOutputMaximum(255);
  rescalerForVisualization->Update();
  itk::WriteImage(rescalerForVisualization->GetOutput(), argv[3]);
  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
