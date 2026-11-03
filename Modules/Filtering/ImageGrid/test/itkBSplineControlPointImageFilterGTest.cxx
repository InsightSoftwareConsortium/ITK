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

#include "itkBSplineControlPointImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkVector.h"
#include "itkGTest.h"

// The output size differs from the control point lattice size, so a second Update() must not request the output
// region from the lattice.
TEST(BSplineControlPointImageFilter, UpdateTwiceWithOutputSizeDifferentFromLattice)
{
  constexpr unsigned int Dimension = 3;
  using VectorType = itk::Vector<float, Dimension>;
  using ImageType = itk::Image<VectorType, Dimension>;
  using FilterType = itk::BSplineControlPointImageFilter<ImageType>;

  auto lattice = ImageType::New();
  lattice->SetRegions(ImageType::SizeType::Filled(8));
  lattice->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(lattice, lattice->GetLargestPossibleRegion()); !it.IsAtEnd();
       ++it)
  {
    const ImageType::IndexType index = it.GetIndex();
    it.Set(itk::MakeVector(static_cast<float>(index[0]), static_cast<float>(index[1]), static_cast<float>(index[2])));
  }

  const auto outputSize = ImageType::SizeType::Filled(20);
  auto       filter = FilterType::New();
  filter->SetInput(lattice);
  filter->SetSize(outputSize);
  filter->SetSpacing(itk::MakeFilled<ImageType::SpacingType>(0.5));
  filter->SetOrigin(itk::MakeFilled<ImageType::PointType>(-1.0));
  filter->SetSplineOrder(3);

  ASSERT_NO_THROW(filter->Update());
  const ImageType::Pointer first = filter->GetOutput();
  first->DisconnectPipeline();

  filter->Modified();
  ASSERT_NO_THROW(filter->Update());
  const ImageType * second = filter->GetOutput();

  EXPECT_EQ(second->GetLargestPossibleRegion().GetSize(), outputSize);
  EXPECT_EQ(second->GetBufferedRegion(), first->GetBufferedRegion());
  EXPECT_EQ(second->GetSpacing(), first->GetSpacing());
  EXPECT_EQ(second->GetOrigin(), first->GetOrigin());
  for (itk::ImageRegionConstIteratorWithIndex<ImageType> it(second, second->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    ASSERT_EQ(it.Get(), first->GetPixel(it.GetIndex())) << "at index " << it.GetIndex();
  }

  const ImageType::RegionType subregion({ { 5, 6, 7 } }, ImageType::SizeType::Filled(4));
  filter->Modified();
  filter->GetOutput()->SetRequestedRegion(subregion);
  ASSERT_NO_THROW(filter->GetOutput()->Update());
  const ImageType * streamed = filter->GetOutput();

  EXPECT_EQ(streamed->GetBufferedRegion(), subregion);
  for (itk::ImageRegionConstIteratorWithIndex<ImageType> it(streamed, subregion); !it.IsAtEnd(); ++it)
  {
    ASSERT_EQ(it.Get(), first->GetPixel(it.GetIndex())) << "at index " << it.GetIndex();
  }
}
