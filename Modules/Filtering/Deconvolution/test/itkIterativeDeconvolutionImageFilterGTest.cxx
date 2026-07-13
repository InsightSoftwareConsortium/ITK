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

#include "itkImage.h"
#include "itkLandweberDeconvolutionImageFilter.h"
#include "itkGTest.h"
#include "itkTestDriverIncludeRequiredFactories.h"

namespace
{
using ImageType = itk::Image<float, 2>;

ImageType::Pointer
MakeConstantImage(const ImageType::SizeType & size, float value)
{
  auto                        image = ImageType::New();
  const ImageType::RegionType region(size);
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(value);
  return image;
}
} // namespace

class IterativeDeconvolutionImageFilterTest : public ::testing::Test
{
protected:
  void
  SetUp() override
  {
    RegisterRequiredFactories();
  }
};

TEST_F(IterativeDeconvolutionImageFilterTest, OutputRegionModeSameMatchesInput)
{
  using FilterType = itk::LandweberDeconvolutionImageFilter<ImageType>;

  const ImageType::SizeType inputSize = { { 10, 10 } };
  const ImageType::Pointer  input = MakeConstantImage(inputSize, 1.0f);
  const ImageType::SizeType kernelSize = { { 3, 3 } };
  const ImageType::Pointer  kernel = MakeConstantImage(kernelSize, 1.0f / 9.0f);

  auto filter = FilterType::New();
  filter->SetInput(input);
  filter->SetKernelImage(kernel);
  filter->SetNumberOfIterations(2);
  filter->Update();

  EXPECT_EQ(filter->GetOutput()->GetLargestPossibleRegion(), input->GetLargestPossibleRegion());
  EXPECT_EQ(filter->GetOutput()->GetBufferedRegion(), input->GetLargestPossibleRegion());
}

TEST_F(IterativeDeconvolutionImageFilterTest, OutputRegionModeValidShrinksOutputRegion)
{
  using FilterType = itk::LandweberDeconvolutionImageFilter<ImageType>;

  const ImageType::SizeType inputSize = { { 10, 10 } };
  const ImageType::Pointer  input = MakeConstantImage(inputSize, 1.0f);
  const ImageType::SizeType kernelSize = { { 3, 3 } };
  const ImageType::Pointer  kernel = MakeConstantImage(kernelSize, 1.0f / 9.0f);

  auto filter = FilterType::New();
  filter->SetInput(input);
  filter->SetKernelImage(kernel);
  filter->SetNumberOfIterations(2);
  filter->SetOutputRegionModeToValid();
  filter->Update();

  ImageType::IndexType expectedValidIndex;
  ImageType::SizeType  expectedValidSize;
  for (unsigned int i = 0; i < ImageType::ImageDimension; ++i)
  {
    const ImageType::SizeValueType radius = kernelSize[i] / 2;
    expectedValidIndex[i] = static_cast<ImageType::IndexValueType>(radius);
    expectedValidSize[i] = inputSize[i] - 2 * radius;
  }
  const ImageType::RegionType expectedValidRegion(expectedValidIndex, expectedValidSize);

  EXPECT_EQ(filter->GetOutput()->GetRequestedRegion(), expectedValidRegion);
  EXPECT_EQ(filter->GetOutput()->GetBufferedRegion(), expectedValidRegion);
  EXPECT_EQ(filter->GetOutput()->GetLargestPossibleRegion(), expectedValidRegion);
}
