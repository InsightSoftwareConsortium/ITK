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
#include "itkRescaleIntensityImageFilter.h"
#include "itkSobelEdgeDetectionImageFilter.h"
#include "itkTestingMacros.h"


int
itkSobelEdgeDetectionImageFilterTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFilename outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using InputPixelType = unsigned char;
  using OutputPixelType = float;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  itk::SobelEdgeDetectionImageFilter<InputImageType, OutputImageType>::Pointer filter =
    itk::SobelEdgeDetectionImageFilter<InputImageType, OutputImageType>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, SobelEdgeDetectionImageFilter, ImageToImageFilter);

  InputImageType::Pointer inputImagePtr;
  ITK_TRY_EXPECT_NO_EXCEPTION(inputImagePtr = itk::ReadImage<InputImageType>(argv[1]));

  filter->SetInput(inputImagePtr);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  using RescaleIntensityImageFilterFilterType = itk::RescaleIntensityImageFilter<OutputImageType, InputImageType>;
  auto rescaler = RescaleIntensityImageFilterFilterType::New();
  rescaler->SetInput(filter->GetOutput());


  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(rescaler->GetOutput(), argv[2]));


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
