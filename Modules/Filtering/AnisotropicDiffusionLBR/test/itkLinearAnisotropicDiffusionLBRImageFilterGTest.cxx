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

#include "itkLinearAnisotropicDiffusionLBRImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkGTest.h"

TEST(LinearAnisotropicDiffusionLBRImageFilter, SmokeTest)
{
  using ImageType = itk::Image<float, 2>;
  using FilterType = itk::LinearAnisotropicDiffusionLBRImageFilter<ImageType>;

  auto filter = FilterType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(filter, LinearAnisotropicDiffusionLBRImageFilter, ImageToImageFilter);

  ImageType::SizeType size;
  size.Fill(16);
  ImageType::RegionType region;
  region.SetSize(size);

  auto image = ImageType::New();
  image->SetRegions(region);
  image->Allocate();
  itk::ImageRegionIterator<ImageType> it(image, region);
  for (float value = 0.0F; !it.IsAtEnd(); ++it, value += 1.0F)
  {
    it.Set(value);
  }

  using TensorImageType = FilterType::TensorImageType;
  using TensorType = FilterType::TensorType;
  TensorType identityTensor;
  identityTensor.SetIdentity();

  auto tensorImage = TensorImageType::New();
  tensorImage->SetRegions(region);
  tensorImage->Allocate();
  tensorImage->FillBuffer(identityTensor);

  filter->SetInputImage(image);
  filter->SetInputTensor(tensorImage);
  filter->SetMaxDiffusionTime(1.0);

  ASSERT_NO_THROW(filter->Update());

  const ImageType * const output = filter->GetOutput();
  EXPECT_EQ(output->GetLargestPossibleRegion(), image->GetLargestPossibleRegion());
  EXPECT_EQ(output->GetSpacing(), image->GetSpacing());
  EXPECT_EQ(output->GetOrigin(), image->GetOrigin());
  EXPECT_NE(output->GetBufferPointer(), nullptr);
}
