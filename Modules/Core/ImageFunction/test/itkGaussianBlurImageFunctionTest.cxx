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

#include "itkMath.h"
#include "itkGaussianBlurImageFunction.h"
#include "itkTestingMacros.h"

int
itkGaussianBlurImageFunctionTest(int, char *[])
{
  constexpr unsigned int Dimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using GFunctionType = itk::GaussianBlurImageFunction<ImageType>;

  // Create and allocate the image
  auto                  image = ImageType::New();
  ImageType::SizeType   size;
  ImageType::IndexType  start;
  ImageType::RegionType region;

  size[0] = 50;
  size[1] = 50;

  start.Fill(0);
  region.SetIndex(start);
  region.SetSize(size);

  image->SetRegions(region);
  image->AllocateInitialized();

  // Fill the image with a straight line
  for (unsigned int i = 0; i < 50; ++i)
  {
    ImageType::IndexType ind;
    ind[0] = i;
    ind[1] = 25;
    image->SetPixel(ind, 1);
    ind[1] = 26;
    image->SetPixel(ind, 1);
  }

  // Test the derivative of Gaussian image function
  auto gaussianFunction = GFunctionType::New();
  gaussianFunction->SetInputImage(image);
  auto index = itk::Index<2>::Filled(25);

  // Testing Set/GetVariance()
  std::cout << "Testing Set/GetVariance(): ";
  gaussianFunction->SetSigma(5.0);
  const GFunctionType::SigmaArrayType & sigma = gaussianFunction->GetSigma();

  for (unsigned int i = 0; i < Dimension; ++i)
  {
    if (sigma[i] != 5.0)
    {
      std::cerr << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED] " << std::endl;

  gaussianFunction->SetSigma(sigma);
  ITK_TEST_SET_GET_VALUE(sigma, gaussianFunction->GetSigma());

  // Testing Set/GetExtent()
  std::cout << "Testing Set/GetExtent(): ";

  gaussianFunction->SetExtent(5.0);
  const GFunctionType::ExtentArrayType & ext = gaussianFunction->GetExtent();

  for (unsigned int i = 0; i < Dimension; ++i)
  {
    if (ext[i] != 5.0)
    {
      std::cerr << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED] " << std::endl;

  gaussianFunction->SetExtent(ext);
  ITK_TEST_SET_GET_VALUE(ext, gaussianFunction->GetExtent());


  // Testing Set/GetMaximumError()
  {
    std::cout << "Testing Set/GetMaximumError(): ";
    auto setError = itk::MakeFilled<GFunctionType::ErrorArrayType>(0.05);
    gaussianFunction->SetMaximumError(setError);

    const GFunctionType::ErrorArrayType & readError = gaussianFunction->GetMaximumError();

    for (unsigned int i = 0; i < Dimension; ++i)
    {
      if (itk::Math::abs(setError[i] - readError[i]) > 1e-6)
      {
        std::cerr << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "[PASSED] " << std::endl;
  }

  // Testing Set/GetMaximumKernelWidth()
  {
    std::cout << "Testing Set/GetMaximumKernelWidth(): ";
    constexpr int setKernelWidth = 47;

    gaussianFunction->SetMaximumKernelWidth(setKernelWidth);

    const int readKernelWidth = gaussianFunction->GetMaximumKernelWidth();

    if (readKernelWidth != setKernelWidth)
    {
      std::cerr << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << "[PASSED] " << std::endl;
  }

  // Testing Set/GetUseImageSpacing()
  {
    std::cout << "Testing Set/GetUseImageSpacing(): ";
    bool useImageSpacing = true;

    gaussianFunction->SetUseImageSpacing(useImageSpacing);
    if (gaussianFunction->GetUseImageSpacing() != useImageSpacing)
    {
      std::cerr << "Set/GetUseImageSpacing() FAILED !" << std::endl;
      return EXIT_FAILURE;
    }

    useImageSpacing = false;

    gaussianFunction->SetUseImageSpacing(useImageSpacing);
    if (gaussianFunction->GetUseImageSpacing() != useImageSpacing)
    {
      std::cerr << "Set/GetUseImageSpacing() FAILED !" << std::endl;
      return EXIT_FAILURE;
    }

    gaussianFunction->UseImageSpacingOn();
    if (gaussianFunction->GetUseImageSpacing() != true)
    {
      std::cerr << "Set/GetUseImageSpacing() FAILED !" << std::endl;
      return EXIT_FAILURE;
    }

    gaussianFunction->UseImageSpacingOff();
    if (gaussianFunction->GetUseImageSpacing() != false)
    {
      std::cerr << "Set/GetUseImageSpacing() FAILED !" << std::endl;
      return EXIT_FAILURE;
    }


    gaussianFunction->UseImageSpacingOn(); // leave it ON for the next test.
    std::cout << "[PASSED] " << std::endl;
  }


  const GFunctionType::OutputType blurredvalue_index = gaussianFunction->EvaluateAtIndex(index);

  GFunctionType::PointType pt;
  pt[0] = 25.0;
  pt[1] = 25.0;
  const GFunctionType::OutputType blurredvalue_point = gaussianFunction->Evaluate(pt);


  auto                            continuousIndex = itk::MakeFilled<GFunctionType::ContinuousIndexType>(25);
  const GFunctionType::OutputType blurredvalue_continuousIndex =
    gaussianFunction->EvaluateAtContinuousIndex(continuousIndex);


  std::cout << "Testing Evaluate(), EvaluateAtIndex() and EvaluateIndex: ";
  if ((itk::Math::abs(blurredvalue_index - blurredvalue_point) > 0.01) ||
      itk::Math::NotAlmostEquals(blurredvalue_point, blurredvalue_continuousIndex))
  {
    std::cerr << "[FAILED] : " << blurredvalue_index << " : " << blurredvalue_point << " : "
              << blurredvalue_continuousIndex << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED] " << std::endl;

  std::cout << "Testing Evaluate() : ";

  if (itk::Math::abs(blurredvalue_point - 0.158) > 0.1)
  {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED] " << std::endl;

  gaussianFunction->Print(std::cout);
  std::cout << "GaussianBlurImageFunctionTest: [DONE] " << std::endl;
  return EXIT_SUCCESS;
}
