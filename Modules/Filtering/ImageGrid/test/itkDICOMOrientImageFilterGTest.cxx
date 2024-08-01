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
#include "itkDICOMOrientImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkImage.h"
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


TEST(DICOMOrientImageFilter, NoOp)
{
  using ImageType = itk::Image<float, 3>;
  auto filter = itk::DICOMOrientImageFilter<ImageType>::New();
}


TEST(DICOMOrientImageFilter, Streaming)
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


  using OrientFilterType = itk::DICOMOrientImageFilter<ImageType>;
  using PermuteArrayType = OrientFilterType::PermuteOrderArrayType;
  using FlipArrayType = OrientFilterType::FlipAxesArrayType;
  auto filter = OrientFilterType::New();
  filter->SetInput(sourceFilter->GetOutput());

  filter->GetOutput()->UpdateOutputInformation();
  filter->GetOutput()->SetRequestedRegion(itk::Size<3>({ { 1, 2, 3 } }));
  filter->Update();


  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetOutput()->GetBufferedRegion().GetSize(), itk::Size<3>({ { 1, 2, 3 } }));
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::RAS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 1, 1, 0 }));
  {
    ImageType::DirectionType d;
    d.SetIdentity();
    EXPECT_EQ(filter->GetOutput()->GetDirection(), d);
  }
}

TEST(DICOMOrientImageFilter, Values)
{
  using ImageType = itk::Image<uint32_t, 3>;

  auto sourceFilter = itk::RandomImageSource<ImageType>::New();
  sourceFilter->SetMin(-1000);
  sourceFilter->SetMax(1000);
  sourceFilter->SetSize({ 5, 6, 7 });


  using OrientFilterType = itk::DICOMOrientImageFilter<ImageType>;
  using PermuiteArrayType = OrientFilterType::PermuteOrderArrayType;
  using FlipArrayType = OrientFilterType::FlipAxesArrayType;
  auto filter = OrientFilterType::New();
  filter->SetInput(sourceFilter->GetOutput());

  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());

  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 0, 0, 0 }));
  {
    ImageType::DirectionType d;
    d.SetIdentity();
    EXPECT_EQ(filter->GetOutput()->GetDirection(), d);
  }


  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::RAS);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());

  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), OrientFilterType::OrientationEnum::RAS);
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
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
    EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  }


  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::RIP);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), OrientFilterType::OrientationEnum::RIP);
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::LIA);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), OrientFilterType::OrientationEnum::LIA);
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
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

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::IRP);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), OrientFilterType::OrientationEnum::IRP);
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 2, 0, 1 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 1, 1, 0 }));

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::SLA);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), OrientFilterType::OrientationEnum::SLA);
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 2, 0, 1 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 0, 0, 1 }));

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::RPI);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 1, 0, 1 }));

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::LAI);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 0, 1, 1 }));

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::LAS);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::PRI);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::PLS);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::ALS);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::IPR);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::SAL);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::PIR);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::ASR);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::ASL);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
}


TEST(DICOMOrientImageFilter, DirectionFromString)
{
  using ImageType = itk::Image<float, 3>;
  using OrientFilterType = itk::DICOMOrientImageFilter<ImageType>;

  auto filter = OrientFilterType::New();

  filter->SetDesiredCoordinateOrientation("LPS");
  EXPECT_EQ(OrientFilterType::OrientationEnum::LPS, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation("PsL");
  EXPECT_EQ(OrientFilterType::OrientationEnum::PSL, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation("SLP");
  EXPECT_EQ(OrientFilterType::OrientationEnum::SLP, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation("rAI");
  EXPECT_EQ(OrientFilterType::OrientationEnum::RAI, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation("AIR");
  EXPECT_EQ(OrientFilterType::OrientationEnum::AIR, filter->GetDesiredCoordinateOrientation());

  filter->SetDesiredCoordinateOrientation("IRA");
  EXPECT_EQ(OrientFilterType::OrientationEnum::IRA, filter->GetDesiredCoordinateOrientation());
}


TEST(DICOMOrientImageFilter, InvalidOrientation)
{

  using ImageType = itk::Image<int16_t, 3>;
  using OrientFilterType = itk::DICOMOrientImageFilter<ImageType>;

  ImageType::DirectionType dRAI;
  dRAI.SetIdentity();

  dRAI.Fill(0.0);
  dRAI(0, 0) = -1.0;
  dRAI(1, 1) = -1.0;
  dRAI(2, 2) = -1.0;

  EXPECT_EQ(OrientFilterType::OrientationEnum::RAI, itk::DICOMOrientation::DirectionCosinesToOrientation(dRAI));

  auto sourceFilter = itk::RandomImageSource<ImageType>::New();
  sourceFilter->SetMin(-1000);
  sourceFilter->SetMax(1000);
  sourceFilter->SetSize({ 5, 6, 7 });
  sourceFilter->SetDirection(dRAI);
  sourceFilter->UpdateLargestPossibleRegion();

  auto filter = OrientFilterType::New();
  filter->SetInput(sourceFilter->GetOutput());

  filter->SetDesiredCoordinateOrientation(OrientFilterType::OrientationEnum::INVALID);
  EXPECT_EQ(OrientFilterType::OrientationEnum::INVALID, filter->GetDesiredCoordinateOrientation());
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
  EXPECT_EQ(OrientFilterType::OrientationEnum::INVALID, filter->GetInputCoordinateOrientation());

  filter = OrientFilterType::New();
  filter->SetInput(sourceFilter->GetOutput());
  filter->SetDesiredCoordinateOrientation("");
  EXPECT_EQ(OrientFilterType::OrientationEnum::INVALID, filter->GetDesiredCoordinateOrientation());
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
  EXPECT_EQ(OrientFilterType::OrientationEnum::INVALID, filter->GetInputCoordinateOrientation());
}


#ifndef ITK_FUTURE_LEGACY_REMOVE
#include "itkSpatialOrientation.h"

TEST(DICOMOrientImageFilter, LegacyInteroperability)
{
  using ImageType = itk::Image<uint32_t, 3>;


  auto sourceFilter = itk::RandomImageSource<ImageType>::New();
  sourceFilter->SetMin(-1000);
  sourceFilter->SetMax(1000);
  sourceFilter->SetSize({ 5, 6, 7 });
  sourceFilter->UpdateLargestPossibleRegion();

  using OrientFilterType = itk::DICOMOrientImageFilter<ImageType>;
  using PermuiteArrayType = OrientFilterType::PermuteOrderArrayType;
  using FlipArrayType = OrientFilterType::FlipAxesArrayType;
  auto filter = OrientFilterType::New();
  filter->SetInput(sourceFilter->GetOutput());

  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());

  // Legacy compatibility for setting the desired orientation for SpatialOrientationEnums
  // This is done through implicit conversion to the DICOMOrientation class
  filter->SetDesiredCoordinateOrientation(itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI);
  filter->UpdateLargestPossibleRegion();
  CheckImage(sourceFilter->GetOutput(), filter->GetOutput());
  EXPECT_EQ(filter->GetDesiredCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  EXPECT_EQ(filter->GetInputCoordinateOrientation(), OrientFilterType::OrientationEnum::LPS);
  EXPECT_EQ(filter->GetPermuteOrder(), PermuiteArrayType({ 0, 1, 2 }));
  EXPECT_EQ(filter->GetFlipAxes(), FlipArrayType({ 0, 0, 0 }));
}
#endif
