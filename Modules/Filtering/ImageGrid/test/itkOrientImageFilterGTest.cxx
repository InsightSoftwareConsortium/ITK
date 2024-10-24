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

#include <gtest/gtest.h>

#include "itkGTest.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOrientImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkImage.h"
#include "itkAnatomicalOrientation.h"
#include <sstream>

namespace
{

template <typename TPixelType, unsigned int D>
void
CheckImage(const itk::Image<TPixelType, D> * inputImage, const itk::Image<TPixelType, D> * resultsImage)
{
  using ImageType = itk::Image<TPixelType, D>;

  itk::ImageRegionConstIteratorWithIndex<ImageType> iter(resultsImage, resultsImage->GetBufferedRegion());

  while (!iter.IsAtEnd())
  {
    auto resultsIdx = iter.GetIndex();
    auto point = resultsImage->template TransformIndexToPhysicalPoint<double>(resultsIdx);
    auto inputIdx = inputImage->TransformPhysicalPointToIndex(point);

    EXPECT_TRUE(inputImage->GetBufferedRegion().IsInside(inputIdx));

    auto resultValue = iter.Value();
    auto inputValue = inputImage->GetPixel(inputIdx);

    EXPECT_EQ(resultValue, inputValue) << " result idx: " << resultsIdx << " input index: " << inputIdx;

    ++iter;
  }
}

} // namespace


TEST(OrientImageFilter, NoOp)
{
  using ImageType = itk::Image<float, 3>;
  auto filter = itk::OrientImageFilter<ImageType, ImageType>::New();
}


TEST(OrientImageFilter, Streaming)
{
  using ImageType = itk::Image<uint32_t, 3>;

  auto sourceFilter = itk::RandomImageSource<ImageType>::New();
  sourceFilter->SetMin(-1000);
  sourceFilter->SetMax(1000);
  sourceFilter->SetSize({ 5, 6, 7 });
  sourceFilter->UpdateLargestPossibleRegion();

  ImageType::DirectionType direction;
  direction.Fill(0.0);
  direction(0, 0) = -1.0;
  direction(1, 1) = -1.0;
  direction(2, 2) = 1.0;
  sourceFilter->SetDirection(direction);


  using OrientFilterType = itk::OrientImageFilter<ImageType, ImageType>;
  using PermuteArrayType = OrientFilterType::PermuteOrderArrayType;
  using FlipArrayType = OrientFilterType::FlipAxesArrayType;
  auto filter = OrientFilterType::New();
  filter->UseImageDirectionOn();
  filter->SetInput(sourceFilter->GetOutput());
  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::LPS);
  filter->GetOutput()->UpdateOutputInformation();
  filter->GetOutput()->SetRequestedRegion(itk::Size<3>({ { 1, 2, 3 } }));
  filter->Update();


  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetOutput()->GetBufferedRegion().GetSize(), itk::Size<3>({ { 1, 2, 3 } }));
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::RAS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 1, 1, 0 }));
  {
    ImageType::DirectionType d;
    d.SetIdentity();
    EXPECT_EQ(filter->GetOutput()->GetDirection(), d);
  }
}

TEST(OrientImageFilter, Values)
{
  using ImageType = itk::Image<uint32_t, 3>;

  auto sourceFilter = itk::RandomImageSource<ImageType>::New();
  sourceFilter->SetMin(-1000);
  sourceFilter->SetMax(1000);
  sourceFilter->SetSize({ 5, 6, 7 });


  using OrientFilterType = itk::OrientImageFilter<ImageType, ImageType>;
  using PermuiteArrayType = OrientFilterType::PermuteOrderArrayType;
  using FlipArrayType = OrientFilterType::FlipAxesArrayType;
  auto filter = OrientFilterType::New();
  filter->SetInput(sourceFilter->GetOutput());
  filter->UseImageDirectionOn();
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());

  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LSA);
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 2, 1 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 0, 0, 1 }));
  {
    ImageType::DirectionType d;
    d.Fill(0.0);
    d(0, 0) = 1.0;
    d(1, 2) = -1.0;
    d(2, 1) = 1;
    EXPECT_EQ(filter->GetOutput()->GetDirection(), d);
  }


  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::RAS);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());

  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::RAS);
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 1, 1, 0 }));
  {
    ImageType::DirectionType d;
    d.Fill(0.0);
    d(0, 0) = -1.0;
    d(1, 1) = -1.0;
    d(2, 2) = 1;
    EXPECT_EQ(filter->GetOutput()->GetDirection(), d);
  }


  {
    ImageType::DirectionType d;
    d.SetIdentity();
    filter->SetDesiredCoordinateDirection(d);
    filter->UpdateLargestPossibleRegion();
    EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
  }


  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::RIP);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::RIP);
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::LIA);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LIA);
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 2, 1 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 0, 1, 1 }));
  {
    ImageType::DirectionType d;
    d.Fill(0.0);
    d(0, 0) = 1.0;
    d(1, 2) = -1.0;
    d(2, 1) = -1.0;
    EXPECT_EQ(filter->GetOutput()->GetDirection(), d);
  }

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::IRP);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::IRP);
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 2, 0, 1 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 1, 1, 0 }));

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::SLA);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::SLA);
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 2, 0, 1 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 0, 0, 1 }));

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::RPI);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 1, 0, 1 }));

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::LAI);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 0, 1, 1 }));

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::LAS);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::PRI);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::PLS);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::ALS);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::IPR);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::SAL);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::PIR);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::ASR);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::ASL);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetGivenCoordinateOrientation(), itk::AnatomicalOrientation::PositiveEnum::LPS);
}


TEST(OrientImageFilter, DirectionFromString)
{
  using ImageType = itk::Image<float, 3>;
  using OrientFilterType = itk::OrientImageFilter<ImageType, ImageType>;

  auto filter = OrientFilterType::New();

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::CreateFromPositiveStringEncoding("LPS"));
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::LPS, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::CreateFromPositiveStringEncoding("PsL"));
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::PSL, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::CreateFromPositiveStringEncoding("SLP"));
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::SLP, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::CreateFromPositiveStringEncoding("rAI"));
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::RAI, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::CreateFromPositiveStringEncoding("AIR"));
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::AIR, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::CreateFromPositiveStringEncoding("IRA"));
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::IRA, filter->GetDesiredCoordinateOrientation());
}


TEST(OrientImageFilter, InvalidOrientation)
{

  using ImageType = itk::Image<int16_t, 3>;
  using OrientFilterType = itk::OrientImageFilter<ImageType, ImageType>;

  ImageType::DirectionType dRAI;
  dRAI.SetIdentity();

  dRAI.Fill(0.0);
  dRAI(0, 0) = -1.0;
  dRAI(1, 1) = -1.0;
  dRAI(2, 2) = -1.0;

  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::RAI, itk::AnatomicalOrientation(dRAI));

  auto sourceFilter = itk::RandomImageSource<ImageType>::New();
  sourceFilter->SetMin(-1000);
  sourceFilter->SetMax(1000);
  sourceFilter->SetSize({ 5, 6, 7 });
  sourceFilter->SetDirection(dRAI);
  sourceFilter->UpdateLargestPossibleRegion();

  auto filter = OrientFilterType::New();
  filter->SetInput(sourceFilter->GetOutput());

  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::PositiveEnum::INVALID);
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::INVALID, filter->GetDesiredCoordinateOrientation());
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::LSA, filter->GetGivenCoordinateOrientation());

  filter = OrientFilterType::New();
  filter->SetInput(sourceFilter->GetOutput());
  filter->SetDesiredCoordinateOrientation(itk::AnatomicalOrientation::CreateFromPositiveStringEncoding(""));
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::INVALID, filter->GetDesiredCoordinateOrientation());
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
  EXPECT_EQ(itk::AnatomicalOrientation::PositiveEnum::LSA, filter->GetGivenCoordinateOrientation());
}
