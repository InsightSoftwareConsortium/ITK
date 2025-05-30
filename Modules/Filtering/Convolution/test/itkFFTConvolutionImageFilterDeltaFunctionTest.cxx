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

#include "itkFFTConvolutionImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

#include "itkObjectFactoryBase.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
#  include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#  include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#endif

int
itkFFTConvolutionImageFilterDeltaFunctionTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " kernelImage outputImage sizeGreatestPrimeFactor"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  // Read kernel image
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  // Set up delta function image
  const ImageType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();
  auto                        deltaFunctionImage = ImageType::New();
  deltaFunctionImage->SetRegions(region);
  deltaFunctionImage->AllocateInitialized();

  // Set the middle pixel (rounded up) to 1
  ImageType::IndexType middleIndex;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    const ImageType::SizeValueType sizeInDimension = region.GetSize()[i];
    middleIndex[i] = itk::Math::Floor<ImageType::IndexValueType>(0.5 * sizeInDimension);
  }
  deltaFunctionImage->SetPixel(middleIndex, 1);

  using ConvolutionFilterType = itk::FFTConvolutionImageFilter<ImageType>;
  auto convolver = ConvolutionFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(convolver, FFTConvolutionImageFilter, ConvolutionImageFilterBase);

  convolver->SetInput(deltaFunctionImage);
  convolver->SetKernelImage(reader->GetOutput());

  const ConvolutionFilterType::SizeValueType sizeGreatestPrimeFactor = std::stoi(argv[3]);
  if (!itk::Math::IsPrime(sizeGreatestPrimeFactor))
  {
    std::cerr << "A prime number is expected for the greatest prime factor size!" << std::endl;
    return EXIT_FAILURE;
  }

  convolver->SetSizeGreatestPrimeFactor(sizeGreatestPrimeFactor);
  ITK_TEST_SET_GET_VALUE(sizeGreatestPrimeFactor, convolver->GetSizeGreatestPrimeFactor());

  ITK_TRY_EXPECT_NO_EXCEPTION(convolver->Update());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(convolver->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
