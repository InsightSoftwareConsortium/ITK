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

#include "itkLaplacianSharpeningImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


int
itkLaplacianSharpeningImageFilterTest(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFilename"
              << " outputFilename"
              << " useImageSpacing" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = float;
  constexpr unsigned int Dimension = 2;

  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using LaplacianSharpeningFilterType = itk::LaplacianSharpeningImageFilter<ImageType, ImageType>;

  auto laplacianSharpeningFilter = LaplacianSharpeningFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(laplacianSharpeningFilter, LaplacianSharpeningImageFilter, ImageToImageFilter);


  auto useImageSpacing = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(laplacianSharpeningFilter, UseImageSpacing, useImageSpacing);

  laplacianSharpeningFilter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(laplacianSharpeningFilter->Update());


  using OutputPixelType = unsigned char;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using CasterType = itk::CastImageFilter<ImageType, OutputImageType>;

  auto caster = CasterType::New();
  caster->SetInput(laplacianSharpeningFilter->GetOutput());
  caster->Update();

  itk::WriteImage(caster->GetOutput(), argv[2]);

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
