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
#include "itkGridImageSource.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int
itkGridImageSourceTest2(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage"
              << " outputImage" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImageFile = argv[1];
  const char * outputImageFile = argv[2];


  constexpr unsigned int ImageDimension = 3;
  using PixelType = uint8_t;

  using ImageType = itk::Image<PixelType, ImageDimension>;

  // Instantiate the filter
  using GridSourceType = itk::GridImageSource<ImageType>;
  auto gridImage = GridSourceType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(gridImage, GridImageSource, GenerateImageSource);

  ImageType::Pointer inputImage = itk::ReadImage<ImageType>(inputImageFile);

  gridImage->SetReferenceImage(inputImage);
  gridImage->UseReferenceImageOn();

  itk::SimpleFilterWatcher watcher(gridImage, "GridImageSource");

  ITK_TRY_EXPECT_NO_EXCEPTION(gridImage->Update());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outputImageFile);
  writer->SetInput(gridImage->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
