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
#include "itkConnectedComponentImageFilter.h"

#include <bitset>

namespace
{
typename itk::Image<unsigned char, 3>::Pointer
CreateTestImageA()
{
  std::bitset<8> bits(105); // 3D Checkerboard: 01101001

  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(MakeSize(2u, 2u, 2u)));
  image->Allocate(true);

  for (size_t i = 0; i < 8; ++i)
  {
    std::bitset<3> idx(i);
    image->SetPixel({ { idx[0], idx[1], idx[2] } }, bits[i]);
  }

  return image;
}
} // namespace


TEST(ConnectedComponentImageFilter, checkerboard_3D)
{
  auto image = CreateTestImageA();
  using ImageType = decltype(image)::ObjectType;

  auto connected = itk::ConnectedComponentImageFilter<ImageType, ImageType>::New();
  connected->SetInput(image);
  connected->FullyConnectedOff();
  connected->Update();

  itk::ImageRegionConstIterator<ImageType> it(connected->GetOutput(),
                                              connected->GetOutput()->GetLargestPossibleRegion());

  EXPECT_EQ(it.Get(), 1);
  ++it;
  EXPECT_EQ(it.Get(), 0);
  ++it;
  EXPECT_EQ(it.Get(), 0);
  ++it;
  EXPECT_EQ(it.Get(), 2);
  ++it;
  EXPECT_EQ(it.Get(), 0);
  ++it;
  EXPECT_EQ(it.Get(), 3);
  ++it;
  EXPECT_EQ(it.Get(), 4);
  ++it;
  EXPECT_EQ(it.Get(), 0);
  ++it;
  EXPECT_TRUE(it.IsAtEnd());
}
