/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/


#include "itkPrintHelper.h"
using itk::print_helper::operator<<;

#include "itkGTest.h"
#include "itkImage.h"
#include "itkRelabelComponentImageFilter.h"

#include "itkSimpleFilterWatcher.h"
#include "itkRandomImageSource.h"

namespace
{


typename itk::Image<unsigned, 2>::Pointer
CreateTestImageA()
{

  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using PixelType = unsigned;
  using ImageType = itk::Image<PixelType, Dimension>;

  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(MakeSize(3u, 3u)));
  image->Allocate();
  image->FillBuffer(0);

  image->SetPixel({ { 0, 0 } }, 1);

  image->SetPixel({ { 0, 1 } }, 2);
  image->SetPixel({ { 1, 1 } }, 2);

  image->SetPixel({ { 0, 2 } }, 3);
  image->SetPixel({ { 1, 2 } }, 3);
  image->SetPixel({ { 2, 2 } }, 3);

  return image;
}
} // namespace

TEST(RelabelComponentImageFilter, nosort_nosize)
{
  auto image = CreateTestImageA();
  using ImageType = decltype(image)::ObjectType;

  auto filter = itk::RelabelComponentImageFilter<ImageType, ImageType>::New();
  filter->SetInput(image);
  filter->SortByObjectSizeOff();
  filter->SetMinimumObjectSize(0);
  filter->Update();

  EXPECT_EQ(filter->GetNumberOfObjects(), 3);
  std::vector<unsigned> expected({ 1u, 2u, 3u });
  ITK_EXPECT_VECTOR_NEAR(filter->GetSizeOfObjectsInPixels(), expected, 0);
  EXPECT_EQ(filter->GetOutput()->GetPixel({ { 2, 2 } }), 3u);
}


TEST(RelabelComponentImageFilter, nosort_size)
{
  auto image = CreateTestImageA();
  using ImageType = decltype(image)::ObjectType;

  auto filter = itk::RelabelComponentImageFilter<ImageType, ImageType>::New();
  filter->SetInput(image);
  filter->SortByObjectSizeOff();
  filter->SetMinimumObjectSize(2);
  filter->Update();

  EXPECT_EQ(filter->GetNumberOfObjects(), 2);
  std::vector<unsigned> expected({ 2u, 3u });
  ITK_EXPECT_VECTOR_NEAR(filter->GetSizeOfObjectsInPixels(), expected, 0);
  EXPECT_EQ(filter->GetOutput()->GetPixel({ { 2, 2 } }), 2u);
}

TEST(RelabelComponentImageFilter, sort_size)
{
  auto image = CreateTestImageA();
  using ImageType = decltype(image)::ObjectType;

  auto filter = itk::RelabelComponentImageFilter<ImageType, ImageType>::New();
  filter->SetInput(image);
  filter->SortByObjectSizeOn();
  filter->SetMinimumObjectSize(2);
  filter->Update();

  EXPECT_EQ(filter->GetNumberOfObjects(), 2u);
  std::vector<unsigned> expected({ 3u, 2u });
  ITK_EXPECT_VECTOR_NEAR(filter->GetSizeOfObjectsInPixels(), expected, 0);
  EXPECT_EQ(filter->GetOutput()->GetPixel({ { 2, 2 } }), 1u);
}


TEST(RelabelComponentImageFilter, sort_nosize)
{
  auto image = CreateTestImageA();
  using ImageType = decltype(image)::ObjectType;

  auto filter = itk::RelabelComponentImageFilter<ImageType, ImageType>::New();
  filter->SetInput(image);
  filter->SortByObjectSizeOn();
  filter->SetMinimumObjectSize(0);
  filter->Update();

  EXPECT_EQ(filter->GetNumberOfObjects(), 3u);
  std::vector<unsigned> expected({ 3u, 2u, 1u });
  ITK_EXPECT_VECTOR_NEAR(filter->GetSizeOfObjectsInPixels(), expected, 0);
  EXPECT_EQ(filter->GetOutput()->GetPixel({ { 2, 2 } }), 1u);
}


TEST(RelabelComponentImageFilter, big_zero)
{

  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using PixelType = unsigned short;
  using ImageType = itk::Image<PixelType, Dimension>;

  auto                           image = ImageType::New();
  typename ImageType::RegionType region;
  region.SetSize({ { 512, 512, 512 } });
  image->SetRegions(region);
  image->Allocate(true);

  auto filter = itk::RelabelComponentImageFilter<ImageType, ImageType>::New();
  filter->SetInput(image);

  itk::SimpleFilterWatcher watcher1(filter, "relabeler");

  filter->Update();
}


TEST(RelabelComponentImageFilter, big_random)
{

  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using PixelType = unsigned short;
  using ImageType = itk::Image<PixelType, Dimension>;


  auto randomSource = itk::RandomImageSource<ImageType>::New();
  randomSource->SetSize({ { 512, 512, 512 } });
  randomSource->SetMin(0);
  randomSource->Update();

  auto filter = itk::RelabelComponentImageFilter<ImageType, ImageType>::New();
  filter->SetInput(randomSource->GetOutput());

  itk::SimpleFilterWatcher watcher1(filter, "relabeler");

  filter->Update();
}
