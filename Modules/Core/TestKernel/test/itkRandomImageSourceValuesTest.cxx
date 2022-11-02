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

#include "itkImageFileWriter.h"
#include "itkRandomImageSource.h"
#include "itkTestingMacros.h"


int
itkRandomImageSourceValuesTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ImageSourceType = itk::RandomImageSource<ImageType>;
  auto randomImageSource = ImageSourceType::New();

  const ImageType::SizeType size{ { 10, 10 } };
  randomImageSource->SetSize(size);

  randomImageSource->SetMin(0.0);
  randomImageSource->SetMax(20.0);

  randomImageSource->SetNumberOfWorkUnits(1);

  ITK_TRY_EXPECT_NO_EXCEPTION(randomImageSource->Update());


  itk::WriteImage(randomImageSource->GetOutput(), argv[1]);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
