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

#include "itkConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int
itkConvolutionImageFilterDeltaFunctionTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " kernelImage outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  // Read kernel image.
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  // Set up delta function image.
  ImageType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();
  auto                  deltaFunctionImage = ImageType::New();
  deltaFunctionImage->SetRegions(region);
  deltaFunctionImage->Allocate(true); // initialize
                                      // buffer
                                      // to zero

  // Set the middle pixel (rounded up) to 1.
  ImageType::IndexType middleIndex;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    ImageType::SizeValueType sizeInDimension = region.GetSize()[i];
    middleIndex[i] = itk::Math::Floor<ImageType::IndexValueType>(0.5 * sizeInDimension);
  }
  deltaFunctionImage->SetPixel(middleIndex, 1);

  using ConvolutionFilterType = itk::ConvolutionImageFilter<ImageType>;
  auto convolver = ConvolutionFilterType::New();
  convolver->SetInput(deltaFunctionImage);
  convolver->SetKernelImage(reader->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(convolver->GetOutput());

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
