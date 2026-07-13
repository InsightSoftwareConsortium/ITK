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

#include "itkGTest.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkScalarImageKmeansImageFilter.h"

namespace
{
itk::Image<unsigned char, 2>::Pointer
CreateTestImage()
{
  using ImageType = itk::Image<unsigned char, 2>;

  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(itk::MakeSize(4u, 4u)));
  image->AllocateInitialized();
  return image;
}
} // namespace

TEST(ScalarImageKmeansImageFilter, NonContiguousLabelsTooManyClassesThrows)
{
  using FilterType = itk::ScalarImageKmeansImageFilter<itk::Image<unsigned char, 2>>;

  auto filter = FilterType::New();
  filter->SetInput(CreateTestImage());
  filter->UseNonContiguousLabelsOn();

  for (unsigned int i = 0; i < 300; ++i)
  {
    filter->AddClassWithInitialMean(static_cast<double>(i));
  }

  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(ScalarImageKmeansImageFilter, ContiguousLabelsTooManyClassesThrows)
{
  using FilterType = itk::ScalarImageKmeansImageFilter<itk::Image<unsigned char, 2>>;

  auto filter = FilterType::New();
  filter->SetInput(CreateTestImage());

  for (unsigned int i = 0; i < 300; ++i)
  {
    filter->AddClassWithInitialMean(static_cast<double>(i));
  }

  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(ScalarImageKmeansImageFilter, NonContiguousLabelsValidClassCountProducesDistinctSpreadLabels)
{
  using ImageType = itk::Image<unsigned char, 2>;
  using FilterType = itk::ScalarImageKmeansImageFilter<ImageType>;

  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(itk::MakeSize(4u, 4u)));
  image->AllocateInitialized();

  // Two well-separated intensity clusters make the assignment deterministic.
  unsigned int linearIndex = 0;
  for (itk::ImageRegionIterator<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it, ++linearIndex)
  {
    it.Set(linearIndex < 8 ? 10 : 200);
  }

  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->UseNonContiguousLabelsOn();
  filter->AddClassWithInitialMean(10.0);
  filter->AddClassWithInitialMean(200.0);

  EXPECT_NO_THROW(filter->Update());

  // 2 classes on unsigned char spread to { 0, max()/2 - 1 } == { 0, 126 }; both must appear.
  constexpr unsigned char lowLabel = 0;
  constexpr unsigned char highLabel = 126;
  bool                    sawLow = false;
  bool                    sawHigh = false;
  for (itk::ImageRegionConstIterator<ImageType> it(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
       !it.IsAtEnd();
       ++it)
  {
    const unsigned char value = it.Get();
    EXPECT_TRUE(value == lowLabel || value == highLabel);
    sawLow = sawLow || value == lowLabel;
    sawHigh = sawHigh || value == highLabel;
  }
  EXPECT_TRUE(sawLow);
  EXPECT_TRUE(sawHigh);
}
