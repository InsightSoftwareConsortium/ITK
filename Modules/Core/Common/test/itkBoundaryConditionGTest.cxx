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

#include "itkImageRegionIterator.h"
#include "itkConstantBoundaryCondition.h"
#include "itkNeighborhoodIterator.h"
#include "itkGTest.h"

namespace
{
void
println(const char * c)
{
  std::cout << std::endl << c << std::endl;
}

template <typename TPixel>
void
printn(itk::NeighborhoodAllocator<TPixel> & n, const itk::Size<2> & sz)
{
  unsigned int k = 0;
  for (unsigned int j = 0; j < sz[1]; ++j)
  {
    for (unsigned int i = 0; i < sz[0]; ++i, ++k)
    {
      std::cout << n[k] << ' ';
    }
    std::cout << std::endl;
  }
}

void
filln(itk::Image<float, 2> * img)
{
  float i = 0.0f;
  float j = 0.0f;

  itk::ImageRegionIterator<itk::Image<float, 2>> it(img, img->GetRequestedRegion());

  while (!it.IsAtEnd())
  {
    it.Set(100.0 * j + i);
    ++it;
    i = i + 1.0f;
    if (static_cast<unsigned long>(i) % img->GetRequestedRegion().GetSize()[0] == 0)
    {
      j = j + 1.0f;
      i = 0.0f;
    }
  }
}
} // namespace

TEST(BoundaryCondition, ConstantBoundaryAtImageEdge)
{
  using ImageType2D = itk::Image<float, 2>;

  println("Creating some images");

  itk::ImageRegion<2> Region2D;
  itk::Size<2>        size2D;
  size2D[0] = 30;
  size2D[1] = 15;
  itk::Index<2> orig2D;
  orig2D[0] = 0;
  orig2D[1] = 0;
  Region2D.SetSize(size2D);
  Region2D.SetIndex(orig2D);

  auto image2D = ImageType2D::New();
  image2D->SetRegions(Region2D);
  image2D->Allocate();

  println("Initializing some images");
  filln(image2D);

  println("Initializing smart neighborhood iterators");
  itk::Size<2> sz2;
  sz2[0] = 2;
  sz2[1] = 1;

  using SmartIteratorType = itk::NeighborhoodIterator<ImageType2D, itk::ConstantBoundaryCondition<ImageType2D>>;

  SmartIteratorType it2d(sz2, image2D, image2D->GetRequestedRegion());

  itk::ConstantBoundaryCondition<ImageType2D> cbc;
  cbc.SetConstant(0.0f);
  it2d.OverrideBoundaryCondition(&cbc);

  SmartIteratorType::NeighborhoodType tempN;
  SmartIteratorType::NeighborhoodType temp2N;
  temp2N = it2d.GetNeighborhood(); // initialize

  it2d.GoToEnd();
  --it2d;
  tempN = it2d.GetNeighborhood();

  printn(tempN.GetBufferReference(), tempN.GetSize());

  // The 2D image is 30x15, filled with 100*j + i.
  // The last pixel is at (29, 14): value = 100*14 + 29 = 1429.
  // With radius {2,1} and ConstantBoundaryCondition(0):
  // Row 0 (j=13): pixels at x=27,28,29,30(OOB),31(OOB) -> 1327, 1328, 1329, 0, 0
  // Row 1 (j=14): pixels at x=27,28,29,30(OOB),31(OOB) -> 1427, 1428, 1429, 0, 0
  // Row 2 (j=15, OOB): all 0 -> 0, 0, 0, 0, 0
  const auto & buf = tempN.GetBufferReference();
  EXPECT_EQ(buf[0], 1327.0f);
  EXPECT_EQ(buf[1], 1328.0f);
  EXPECT_EQ(buf[2], 1329.0f);
  EXPECT_EQ(buf[3], 0.0f);
  EXPECT_EQ(buf[4], 0.0f);
  EXPECT_EQ(buf[5], 1427.0f);
  EXPECT_EQ(buf[6], 1428.0f);
  EXPECT_EQ(buf[7], 1429.0f);
  EXPECT_EQ(buf[8], 0.0f);
  EXPECT_EQ(buf[9], 0.0f);
  EXPECT_EQ(buf[10], 0.0f);
  EXPECT_EQ(buf[11], 0.0f);
  EXPECT_EQ(buf[12], 0.0f);
  EXPECT_EQ(buf[13], 0.0f);
  EXPECT_EQ(buf[14], 0.0f);

  std::cout << " ________________________________________ " << std::endl;
}

TEST(BoundaryCondition, ZeroFluxNeumannBoundaryTraversal)
{
  using ImageType2D = itk::Image<float, 2>;

  itk::ImageRegion<2> Region2D;
  itk::Size<2>        size2D;
  size2D[0] = 30;
  size2D[1] = 15;
  itk::Index<2> orig2D;
  orig2D[0] = 0;
  orig2D[1] = 0;
  Region2D.SetSize(size2D);
  Region2D.SetIndex(orig2D);

  auto image2D = ImageType2D::New();
  image2D->SetRegions(Region2D);
  image2D->Allocate();
  filln(image2D);

  itk::Size<2> sz2;
  sz2[0] = 2;
  sz2[1] = 1;

  using SmartIteratorType = itk::NeighborhoodIterator<ImageType2D, itk::ConstantBoundaryCondition<ImageType2D>>;

  SmartIteratorType it2d(sz2, image2D, image2D->GetRequestedRegion());

  itk::ConstantBoundaryCondition<ImageType2D> cbc;
  cbc.SetConstant(0.0f);
  it2d.OverrideBoundaryCondition(&cbc);

  SmartIteratorType::NeighborhoodType temp2N;
  temp2N = it2d.GetNeighborhood(); // initialize

  itk::ZeroFluxNeumannBoundaryCondition<ImageType2D> neumann;
  for (int yak = 0; yak < 2; ++yak)
  {
    for (it2d.GoToBegin(); !it2d.IsAtEnd(); ++it2d)
    {
      for (unsigned int ii = 0; ii < temp2N.Size(); ++ii)
      {
        temp2N[ii] = it2d.GetPixel(ii);
      }
    }

    it2d.OverrideBoundaryCondition(&neumann);
  }
  // If we reach here without crashing, the test passes
  EXPECT_TRUE(it2d.IsAtEnd());
}
