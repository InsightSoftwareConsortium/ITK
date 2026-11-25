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

#include "itkBoxSigmaImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkGTest.h"

// A radius-0 window holds a single sample, so the (unbiased) standard deviation must be
// zero, not sqrt(0/0) = NaN (issue #6575, B20).
TEST(BoxSigmaImageFilter, RadiusZeroYieldsZeroNotNaN)
{
  using ImageType = itk::Image<float, 2>;
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(8) });
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    it.Set(static_cast<float>(it.GetIndex()[0] + 10 * it.GetIndex()[1]));
  }

  using FilterType = itk::BoxSigmaImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetRadius(0);
  filter->Update();

  itk::ImageRegionConstIteratorWithIndex<ImageType> it(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    EXPECT_EQ(it.Get(), 0.0F) << " at index " << it.GetIndex();
  }
}
