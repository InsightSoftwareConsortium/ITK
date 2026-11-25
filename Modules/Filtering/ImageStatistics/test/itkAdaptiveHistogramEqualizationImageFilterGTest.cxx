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

#include "itkAdaptiveHistogramEqualizationImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkGTest.h"

// A constant image has zero intensity range; equalization must be an identity mapping
// instead of dividing by zero and producing NaN everywhere (issue #6575, B9).
TEST(AdaptiveHistogramEqualizationImageFilter, ConstantImageIsIdentity)
{
  using ImageType = itk::Image<float, 2>;
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(16) });
  image->Allocate();
  image->FillBuffer(7.0F);

  using FilterType = itk::AdaptiveHistogramEqualizationImageFilter<ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetRadius(2);
  filter->Update();

  itk::ImageRegionConstIterator<ImageType> it(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    ASSERT_EQ(it.Get(), 7.0F);
  }
}
