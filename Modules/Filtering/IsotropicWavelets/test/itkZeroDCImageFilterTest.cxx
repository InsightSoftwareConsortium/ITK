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

#include "itkZeroDCImageFilter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

#include <string>

template <unsigned int VDimension>
int
runZeroDCImageFilterTest(const std::string & inputImage)
{
  const unsigned int Dimension = VDimension;

  // TODO massive difference (-1,4, 3^-10) of 0 freq pixel between (float, double)
  // I guess it is because FFT algorithm not ZeroDC. Maybe because odd size using FFTW.
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);

  using ZeroDCFilterType = itk::ZeroDCImageFilter<ImageType>;
  auto zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());
  ITK_TRY_EXPECT_NO_EXCEPTION(zeroDCFilter->Update());

  // Perform FFT on zeroDC image and check freq bin 0 has zero value.
  using FFTForwardFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftForwardFilter = FFTForwardFilterType::New();
  fftForwardFilter->SetInput(zeroDCFilter->GetOutput());
  ITK_TRY_EXPECT_NO_EXCEPTION(fftForwardFilter->Update());

  using ComplexImageType = typename FFTForwardFilterType::OutputImageType;
  typename ComplexImageType::IndexType zeroIndex;
  zeroIndex.Fill(0);

  typename ComplexImageType::Pointer filteredImg = fftForwardFilter->GetOutput();
  filteredImg->DisconnectPipeline();
  typename ComplexImageType::PixelType filteredZeroFreqPixelValue = filteredImg->GetPixel(zeroIndex);
  typename ZeroDCFilterType::RealType  mean = zeroDCFilter->GetMean();

  fftForwardFilter->SetInput(reader->GetOutput());
  fftForwardFilter->Update();
  auto                                 originalImg = fftForwardFilter->GetOutput();
  typename ComplexImageType::PixelType originalZeroFreqPixelValue = originalImg->GetPixel(zeroIndex);

  if (itk::Math::NotAlmostEquals(filteredZeroFreqPixelValue.real(), 0.0))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "DC Component (Index = " << zeroIndex << ") is not zero: " << filteredZeroFreqPixelValue.real()
              << std::endl;
    std::cerr << "DC Component (Index = " << zeroIndex
              << ") of original image is: " << originalZeroFreqPixelValue.real() << std::endl;
    std::cerr << "Mean value of original image (spatial domain) is: " << mean << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkZeroDCImageFilterTest(int argc, char * argv[])
{
  if (argc < 2 || argc > 3)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage [dimension]" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string inputImage = argv[1];
  unsigned int      dimension = 3;
  if (argc == 3)
  {
    dimension = std::stoi(argv[2]);
  }

  constexpr unsigned int ImageDimension = 3;
  using PixelType = double;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  using ZeroDCFilterType = itk::ZeroDCImageFilter<ImageType>;

  auto zeroDCFilter = ZeroDCFilterType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(zeroDCFilter, ZeroDCImageFilter, ImageToImageFilter);

  if (dimension == 2)
  {
    return runZeroDCImageFilterTest<2>(inputImage);
  }
  else if (dimension == 3)
  {
    return runZeroDCImageFilterTest<3>(inputImage);
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
