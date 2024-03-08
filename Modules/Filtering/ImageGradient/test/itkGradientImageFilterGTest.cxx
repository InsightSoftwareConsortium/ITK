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

// First include the header file to be tested:
#include "itkGradientImageFilter.h"

#include "itkDeref.h"
#include "itkImage.h"
#include "itkImageBufferRange.h"
#include "itkIndexRange.h"

#include <gtest/gtest.h>


// Tests the output for a uniform input image.
TEST(GradientImageFilter, UniformInputImage)
{
  static constexpr unsigned int Dimension{ 3 };
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ImageSizeType = itk::Size<Dimension>;
  using FilterType = itk::GradientImageFilter<ImageType>;

  for (const PixelType inputPixelValue :
       { std::numeric_limits<PixelType>::lowest(), PixelType{ 1 }, std::numeric_limits<PixelType>::max() })
  {
    const auto filter = FilterType::New();

    // Keep the image size small, in order to keep the unit test fast!
    static constexpr auto imageSize = ImageSizeType::Filled(4);

    const auto inputImage = ImageType::New();
    inputImage->SetRegions(imageSize);
    inputImage->Allocate(false);
    inputImage->FillBuffer(inputPixelValue);
    filter->SetInput(inputImage);
    filter->Update();
    const auto & output = itk::Deref(filter->GetOutput());

    const itk::ImageBufferRange outputImageBufferRange(output);

    EXPECT_EQ(outputImageBufferRange.size(), imageSize.CalculateProductOfElements());

    for (const auto & outputValue : outputImageBufferRange)
    {
      // Expect all output pixels to be zero.
      EXPECT_EQ(outputValue, FilterType::OutputPixelType{});
    }
  }
}


// Tests the output for an input image that has a constant gradient along its first dimension.
TEST(GradientImageFilter, ConstantGradientInputImage)
{
  static constexpr unsigned int Dimension{ 3 };
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using IndexType = itk::Index<Dimension>;
  using SizeType = itk::Size<Dimension>;
  using FilterType = itk::GradientImageFilter<ImageType>;
  using OutputValueType = FilterType::OutputValueType;

  for (int inputGradientValue{ -1 }; inputGradientValue <= 2; ++inputGradientValue)
  {
    const auto filter = FilterType::New();

    // Keep the image size small, in order to keep the unit test fast!
    static constexpr auto imageSize = SizeType::Filled(4);

    const auto inputImage = ImageType::New();
    inputImage->SetRegions(imageSize);
    inputImage->Allocate(false);

    for (const auto & index : itk::ZeroBasedIndexRange<Dimension>(imageSize))
    {
      // A constant gradient along the first dimension of the image.
      inputImage->SetPixel(index, static_cast<PixelType>(inputGradientValue * index[0]));
    }

    filter->SetInput(inputImage);
    filter->Update();
    const auto & output = itk::Deref(filter->GetOutput());

    ASSERT_EQ(output.GetBufferedRegion().GetSize(), imageSize);

    // Only look at the inner region, to avoid boundary effects (which are beyond the scope of this unit test).
    const itk::ImageRegion innerRegion{ IndexType::Filled(1), imageSize - SizeType::Filled(2) };

    for (const auto & index : itk::ImageRegionIndexRange<Dimension>(innerRegion))
    {
      const auto outputPixelValue = output.GetPixel(index);

      EXPECT_EQ(outputPixelValue[0], inputGradientValue);

      for (unsigned int i{ 1 }; i < Dimension; ++i)
      {
        EXPECT_EQ(outputPixelValue[i], OutputValueType{ 0 });
      }
    }
  }
}
