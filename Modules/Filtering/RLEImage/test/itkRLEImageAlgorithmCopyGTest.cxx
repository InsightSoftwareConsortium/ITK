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

#include "itkImageAlgorithm.h"
#include "itkRLEImage.h"

#include <gtest/gtest.h>

// Locks in correct iterator deduction in ImageAlgorithm::Copy when the
// source or destination is a partially-specialized Image-like template.
namespace
{
constexpr unsigned int Dimension = 3;
using PixelType = short;
using RLEType = itk::RLEImage<PixelType, Dimension>;
using ImageType = itk::Image<PixelType, Dimension>;

RLEType::RegionType
MakeRegion()
{
  RLEType::SizeType  size = { { 4, 4, 4 } };
  RLEType::IndexType start = { { 0, 0, 0 } };
  return RLEType::RegionType{ start, size };
}
} // namespace

TEST(RLEImageAlgorithmCopy, FromRLEToImage)
{
  const auto region = MakeRegion();

  auto rle = RLEType::New();
  rle->SetRegions(region);
  rle->Allocate();
  rle->FillBuffer(7);

  auto dst = ImageType::New();
  dst->SetRegions(region);
  dst->Allocate();
  dst->FillBuffer(0);

  itk::ImageAlgorithm::Copy<RLEType, ImageType>(rle.GetPointer(), dst.GetPointer(), region, region);

  for (itk::ImageRegionConstIterator<ImageType> it(dst, region); !it.IsAtEnd(); ++it)
  {
    ASSERT_EQ(it.Get(), 7) << "Mismatch at " << it.GetIndex();
  }
}

TEST(RLEImageAlgorithmCopy, FromImageToRLE)
{
  const auto region = MakeRegion();

  auto src = ImageType::New();
  src->SetRegions(region);
  src->Allocate();
  src->FillBuffer(11);

  auto rleDst = RLEType::New();
  rleDst->SetRegions(region);
  rleDst->Allocate();
  rleDst->FillBuffer(0);

  itk::ImageAlgorithm::Copy<ImageType, RLEType>(src.GetPointer(), rleDst.GetPointer(), region, region);

  for (itk::ImageRegionConstIterator<RLEType> it(rleDst, region); !it.IsAtEnd(); ++it)
  {
    ASSERT_EQ(it.Get(), 11) << "Mismatch at " << it.GetIndex();
  }
}
