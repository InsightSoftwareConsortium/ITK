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

#include "itkSTAPLEImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkGTest.h"

// All-background segmentations zero the weight image, so the sensitivity denominator is
// zero; the estimates must stay finite instead of becoming NaN (issue #6575, B11).
TEST(STAPLEImageFilter, AllBackgroundInputsStayFinite)
{
  using InputImageType = itk::Image<unsigned char, 2>;
  using OutputImageType = itk::Image<double, 2>;

  const auto makeImage = [] {
    auto image = InputImageType::New();
    image->SetRegions(InputImageType::RegionType{ InputImageType::IndexType{}, InputImageType::SizeType::Filled(16) });
    image->Allocate();
    image->FillBuffer(0);
    return image;
  };

  using FilterType = itk::STAPLEImageFilter<InputImageType, OutputImageType>;
  auto filter = FilterType::New();
  filter->SetInput(0, makeImage());
  filter->SetInput(1, makeImage());
  filter->SetForegroundValue(1);
  filter->SetMaximumIterations(5);
  filter->Update();

  for (unsigned int i = 0; i < 2; ++i)
  {
    EXPECT_TRUE(std::isfinite(filter->GetSensitivity(i))) << " input " << i;
    EXPECT_TRUE(std::isfinite(filter->GetSpecificity(i))) << " input " << i;
  }
  itk::ImageRegionConstIterator<OutputImageType> it(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    ASSERT_TRUE(std::isfinite(it.Get()));
  }
}
