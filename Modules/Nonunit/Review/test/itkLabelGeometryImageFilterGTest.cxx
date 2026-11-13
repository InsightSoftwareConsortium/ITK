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

#include "itkLabelGeometryImageFilter.h"
#include "itkGTest.h"

// A box of integer size must keep all of its pixels, however long it is.
TEST(LabelGeometryImageFilter, OrientedIntensityImageKeepsAllPixelsOfLongRegion)
{
  using ImageType = itk::Image<unsigned char, 2>;
  constexpr ImageType::SizeType size{ 1048576, 2 };

  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ size });
  image->Allocate();
  image->FillBuffer(1);

  auto filter = itk::LabelGeometryImageFilter<ImageType>::New();
  filter->SetInput(image);
  filter->SetIntensityInput(image);
  filter->CalculateOrientedIntensityRegionsOn();
  filter->Update();

  EXPECT_EQ(filter->GetOrientedIntensityImage(1)->GetLargestPossibleRegion().GetNumberOfPixels(), size[0] * size[1]);
}
