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

#include <iostream>
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "itkRGBPixel.h"
#include "itkImageRegionRange.h"
#include "itkImageScanlineIterator.h"
#include "itkImageScanlineConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkTimeProbe.h"
#include "itkNumericTraits.h"
#include "itkTestingMacros.h"
#include <iomanip>


// Helper function to initialize an image with random values
template <typename TImage>
typename TImage::Pointer
CreateAndInitializeImage(const typename TImage::SizeType & size, unsigned int numberOfComponentsPerPixel = 0)
{
  auto                        image = TImage::New();
  typename TImage::RegionType region{ size };
  image->SetRegions(region);
  if (numberOfComponentsPerPixel > 0)
  {
    image->SetNumberOfComponentsPerPixel(numberOfComponentsPerPixel);
  }
  image->Allocate();

  // Initialize with simple pattern (pixel index-based)
  using PixelType = typename TImage::PixelType;
  unsigned int count = 0;

  const unsigned int length = image->GetNumberOfComponentsPerPixel();

  itk::ImageRegionIteratorWithIndex<TImage> it(image, region);
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    PixelType pixel{ it.Get() };
    for (unsigned int k = 0; k < length; ++k)
    {
      pixel[k] = static_cast<typename itk::NumericTraits<PixelType>::ValueType>(count + k);
    }
    it.Set(pixel);
    ++count;
  }

  return image;
}


// Method 1: ImageScanlineIterator approach
template <typename TInputImage, typename TOutputImage>
void
CopyScanlineIterator(const TInputImage * inputPtr, TOutputImage * outputPtr)
{
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using ImageScanlineConstIterator = itk::ImageScanlineConstIterator<TInputImage>;
  using ImageScanlineIterator = itk::ImageScanlineIterator<TOutputImage>;

  const typename TOutputImage::RegionType outputRegion = outputPtr->GetRequestedRegion();
  typename TInputImage::RegionType        inputRegion = outputRegion;

  ImageScanlineConstIterator inputIt(inputPtr, inputRegion);
  ImageScanlineIterator      outputIt(outputPtr, outputRegion);

  const unsigned int componentsPerPixel = inputPtr->GetNumberOfComponentsPerPixel();
  while (!inputIt.IsAtEnd())
  {
    while (!inputIt.IsAtEndOfLine())
    {
      const InputPixelType & inputPixel = inputIt.Get();
      OutputPixelType        value(outputIt.Get());
      for (unsigned int k = 0; k < componentsPerPixel; ++k)
      {
        value[k] = static_cast<typename OutputPixelType::ValueType>(inputPixel[k]);
      }
      outputIt.Set(value);

      ++inputIt;
      ++outputIt;
    }
    inputIt.NextLine();
    outputIt.NextLine();
  }
}


// Method 1b: ImageScanlineIterator approach using NumericTraits::GetLength()
template <typename TInputImage, typename TOutputImage>
void
CopyScanlineIteratorNumericTraits(const TInputImage * inputPtr, TOutputImage * outputPtr)
{
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using ImageScanlineConstIterator = itk::ImageScanlineConstIterator<TInputImage>;
  using ImageScanlineIterator = itk::ImageScanlineIterator<TOutputImage>;

  const typename TOutputImage::RegionType outputRegion = outputPtr->GetRequestedRegion();
  typename TInputImage::RegionType        inputRegion = outputRegion;

  ImageScanlineConstIterator inputIt(inputPtr, inputRegion);
  ImageScanlineIterator      outputIt(outputPtr, outputRegion);

  unsigned int componentsPerPixel = itk::NumericTraits<OutputPixelType>::GetLength(outputIt.Get());
  while (!inputIt.IsAtEnd())
  {
    while (!inputIt.IsAtEndOfLine())
    {
      const InputPixelType & inputPixel = inputIt.Get();

      OutputPixelType value{ outputIt.Get() };
      for (unsigned int k = 0; k < componentsPerPixel; ++k)
      {
        value[k] = static_cast<typename OutputPixelType::ValueType>(inputPixel[k]);
      }
      outputIt.Set(value);

      ++inputIt;
      ++outputIt;
    }
    inputIt.NextLine();
    outputIt.NextLine();
  }
}


// Method 2: ImageRegionRange approach
template <typename TInputImage, typename TOutputImage>
void
CopyImageRegionRange(const TInputImage * inputPtr, TOutputImage * outputPtr)
{
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  const typename TOutputImage::RegionType outputRegion = outputPtr->GetRequestedRegion();
  typename TInputImage::RegionType        inputRegion = outputRegion;

  auto inputRange = itk::ImageRegionRange<const TInputImage>(*inputPtr, inputRegion);
  auto outputRange = itk::ImageRegionRange<TOutputImage>(*outputPtr, outputRegion);

  auto       inputIt = inputRange.begin();
  auto       outputIt = outputRange.begin();
  const auto inputEnd = inputRange.end();

  const unsigned int componentsPerPixel = inputPtr->GetNumberOfComponentsPerPixel();
  while (inputIt != inputEnd)
  {
    const InputPixelType & inputPixel = *inputIt;
    OutputPixelType        outputPixel{ *outputIt };
    for (unsigned int k = 0; k < componentsPerPixel; ++k)
    {
      outputPixel[k] = static_cast<typename OutputPixelType::ValueType>(inputPixel[k]);
    }
    *outputIt = outputPixel;

    ++inputIt;
    ++outputIt;
  }
}


// Method 2b: ImageRegionRange approach using NumericTraits::GetLength()
template <typename TInputImage, typename TOutputImage>
void
CopyImageRegionRangeNumericTraits(const TInputImage * inputPtr, TOutputImage * outputPtr)
{
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  const typename TOutputImage::RegionType outputRegion = outputPtr->GetRequestedRegion();
  typename TInputImage::RegionType        inputRegion = outputRegion;

  auto inputRange = itk::ImageRegionRange<const TInputImage>(*inputPtr, inputRegion);
  auto outputRange = itk::ImageRegionRange<TOutputImage>(*outputPtr, outputRegion);

  auto       inputIt = inputRange.begin();
  auto       outputIt = outputRange.begin();
  const auto inputEnd = inputRange.end();

  const unsigned int componentsPerPixel = itk::NumericTraits<OutputPixelType>::GetLength(*outputIt);
  while (inputIt != inputEnd)
  {
    const InputPixelType & inputPixel = *inputIt;
    OutputPixelType        outputPixel{ *outputIt };
    for (unsigned int k = 0; k < componentsPerPixel; ++k)
    {
      outputPixel[k] = static_cast<typename OutputPixelType::ValueType>(inputPixel[k]);
    }
    *outputIt = outputPixel;
    ++inputIt;
    ++outputIt;
  }
}


// Method 2c: ImageRegionRange approach - BUGGY VERSION demonstrating incorrect pixel reuse
// NOTE: This method has a BUG - outputPixel is initialized once and reused, causing incorrect results
// when the pixel type holds a reference to the image buffer (modifies first pixel repeatedly)
template <typename TInputImage, typename TOutputImage>
void
CopyImageRegionRangeNumericTraitsBuggy(const TInputImage * inputPtr, TOutputImage * outputPtr)
{
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  const typename TOutputImage::RegionType outputRegion = outputPtr->GetRequestedRegion();
  typename TInputImage::RegionType        inputRegion = outputRegion;

  auto inputRange = itk::ImageRegionRange<const TInputImage>(*inputPtr, inputRegion);
  auto outputRange = itk::ImageRegionRange<TOutputImage>(*outputPtr, outputRegion);

  auto       inputIt = inputRange.begin();
  auto       outputIt = outputRange.begin();
  const auto inputEnd = inputRange.end();

  const unsigned int componentsPerPixel = itk::NumericTraits<OutputPixelType>::GetLength(*outputIt);
  OutputPixelType    outputPixel{ *outputIt };
  while (inputIt != inputEnd)
  {
    const InputPixelType & inputPixel = *inputIt;
    for (unsigned int k = 0; k < componentsPerPixel; ++k)
    {
      outputPixel[k] = static_cast<typename OutputPixelType::ValueType>(inputPixel[k]);
    }
    *outputIt = outputPixel;
    ++inputIt;
    ++outputIt;
  }
}


// Helper function to time a single method
template <typename TInputImage, typename TOutputImage, typename TCopyFunction>
void
TimeMethod(const std::string &              methodName,
           TCopyFunction                    copyFunc,
           const TInputImage *              inputImage,
           typename TOutputImage::Pointer & outputImage,
           double                           totalPixels,
           typename TOutputImage::Pointer   referenceImage = nullptr)
{
  // Allocate output image
  outputImage = TOutputImage::New();
  outputImage->SetRegions(inputImage->GetLargestPossibleRegion());
  outputImage->SetNumberOfComponentsPerPixel(inputImage->GetNumberOfComponentsPerPixel());
  outputImage->Allocate();

  // Warm-up run
  copyFunc(inputImage, outputImage.GetPointer());

  // Timed run
  itk::TimeProbe timer;
  timer.Start();
  copyFunc(inputImage, outputImage.GetPointer());
  timer.Stop();

  const double time = timer.GetMean();
  const double mpixels = (totalPixels / 1e6) / time;
  std::cout << std::left << std::setw(23) << methodName << mpixels << " Mpixels/sec (" << time << " seconds)"
            << std::endl;

  // Verify correctness if reference provided
  if (referenceImage)
  {
    bool                                        match = true;
    itk::ImageRegionConstIterator<TOutputImage> itRef(referenceImage, referenceImage->GetLargestPossibleRegion());
    itk::ImageRegionConstIterator<TOutputImage> it(outputImage, outputImage->GetLargestPossibleRegion());
    for (itRef.GoToBegin(), it.GoToBegin(); !itRef.IsAtEnd(); ++itRef, ++it)
    {
      if (itRef.Get() != it.Get())
      {
        match = false;
        break;
      }
    }
    if (!match)
    {
      std::cout << "  WARNING: Results differ from reference!" << std::endl;
    }
  }
}


// Performance testing function
template <typename TInputImage, typename TOutputImage>
void
TimeIterationMethods(const typename TInputImage::SizeType & size, const std::string & description)
{
  std::cout << "\n=== " << description << " ===" << std::endl;

  // Create and initialize input image
  auto inputImage = CreateAndInitializeImage<TInputImage>(size, 3);

  const double totalPixels = size.CalculateProductOfElements();

  typename TOutputImage::Pointer referenceImage;
  typename TOutputImage::Pointer outputImage;

  // Test Method 1: Scanline Iterator - serves as reference
  TimeMethod<TInputImage, TOutputImage>("Scanline Iterator:",
                                        CopyScanlineIterator<TInputImage, TOutputImage>,
                                        inputImage.GetPointer(),
                                        referenceImage,
                                        totalPixels);

  // Test Method 2: ImageRegionRange
  TimeMethod<TInputImage, TOutputImage>("ImageRegionRange:",
                                        CopyImageRegionRange<TInputImage, TOutputImage>,
                                        inputImage.GetPointer(),
                                        outputImage,
                                        totalPixels,
                                        referenceImage);

  // Test Method 1b: Scanline Iterator with NumericTraits
  TimeMethod<TInputImage, TOutputImage>("Scanline NumericTraits:",
                                        CopyScanlineIteratorNumericTraits<TInputImage, TOutputImage>,
                                        inputImage.GetPointer(),
                                        outputImage,
                                        totalPixels,
                                        referenceImage);

  // Test Method 2b: ImageRegionRange with NumericTraits
  TimeMethod<TInputImage, TOutputImage>("Range NumericTraits:",
                                        CopyImageRegionRangeNumericTraits<TInputImage, TOutputImage>,
                                        inputImage.GetPointer(),
                                        outputImage,
                                        totalPixels,
                                        referenceImage);

  // Test Method 2c: ImageRegionRange with NumericTraits - buggy version
  TimeMethod<TInputImage, TOutputImage>("Range NT (BUGGY):",
                                        CopyImageRegionRangeNumericTraitsBuggy<TInputImage, TOutputImage>,
                                        inputImage.GetPointer(),
                                        outputImage,
                                        totalPixels,
                                        referenceImage);

  std::cout << "All methods validated." << std::endl;
}


int
itkImageIterationPerformanceTest(int argc, char * argv[])
{
  std::cout << "CTEST_FULL_OUTPUT" << std::endl;

  // Test different image sizes and types
  constexpr unsigned int Dimension = 3;

  // Get image size from command line or use default
  unsigned int size = 64;
  if (argc == 2)
  {
    size = std::stoi(argv[1]);
  }
  else
  {
    if (argc < 2)
    {
      std::cerr << "Missing parameters." << std::endl;
      std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " imageSize" << std::endl;
      return EXIT_FAILURE;
    }
  }

  const auto imageSize = itk::Size<Dimension>::Filled(size);

  std::ostringstream oss;
  oss << "Image Size: " << imageSize;

  std::string description = oss.str();

  // Test 1: Image<Vector> to Image<RGBPixel>
  std::cout << "\n--- Test Suite 1: Image<Vector<float,3>> to Image<RGBPixel<double>> ---" << std::endl;
  TimeIterationMethods<itk::Image<itk::Vector<float, 3>, Dimension>, itk::Image<itk::RGBPixel<double>, Dimension>>(
    imageSize, description);

  // Test 2: VectorImage to Image<Vector>
  std::cout << "\n--- Test Suite 2: VectorImage<float> to Image<Vector<double,3>> ---" << std::endl;
  TimeIterationMethods<itk::VectorImage<float, Dimension>, itk::Image<itk::Vector<double, 3>, Dimension>>(imageSize,
                                                                                                          description);

  // Test 3: Image<Vector> to VectorImage
  std::cout << "\n--- Test Suite 3: Image<Vector<float,3>> to VectorImage<double> ---" << std::endl;
  TimeIterationMethods<itk::Image<itk::Vector<float, 3>, Dimension>, itk::VectorImage<double, Dimension>>(imageSize,
                                                                                                          description);

  // Test 4: VectorImage to VectorImage
  std::cout << "\n--- Test Suite 4: VectorImage<float> to VectorImage<double> ---" << std::endl;
  TimeIterationMethods<itk::VectorImage<float, Dimension>, itk::VectorImage<double, Dimension>>(imageSize, description);
  std::cout << "\n=====================================" << std::endl;
  std::cout << "All tests completed successfully." << std::endl;

  return EXIT_SUCCESS;
}
