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

#include "itkMultiLabelSTAPLEImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkGTest.h"

#include <array>
#include <chrono>
#include <cstdlib>
#include <future>
#include <iostream>
#include <thread>

namespace
{
using ImageType = itk::Image<unsigned char, 2>;
using FilterType = itk::MultiLabelSTAPLEImageFilter<ImageType>;

ImageType::Pointer
MakeRaterImage(const std::array<unsigned char, 4> & labels)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::SizeType{ 4, 1 } });
  image->Allocate();
  itk::ImageRegionIterator<ImageType> it(image, image->GetBufferedRegion());
  for (const unsigned char label : labels)
  {
    it.Set(label);
    ++it;
  }
  return image;
}

using WideImageType = itk::Image<unsigned short, 2>;
using WideFilterType = itk::MultiLabelSTAPLEImageFilter<WideImageType>;

template <typename TImage>
typename TImage::Pointer
MakeUniformImage(typename TImage::PixelType value)
{
  auto image = TImage::New();
  image->SetRegions(TImage::SizeType::Filled(2));
  image->Allocate();
  image->FillBuffer(value);
  return image;
}
} // namespace

// Voting-tie pixels must be skipped when seeding, not written past the matrix row (issue #6575).
TEST(MultiLabelSTAPLEImageFilter, VotingUndecidedPixelsDoNotCorruptSeededConfusionMatrix)
{
  auto filter = FilterType::New();
  filter->SetInput(0, MakeRaterImage({ 0, 1, 0, 1 }));
  filter->SetInput(1, MakeRaterImage({ 0, 1, 1, 0 }));
  // Zero EM iterations: the confusion matrices stay exactly as seeded by voting.
  filter->SetMaximumNumberOfIterations(0);
  filter->Update();
  EXPECT_EQ(filter->GetElapsedNumberOfIterations(), 0u);

  for (unsigned int rater = 0; rater < 2; ++rater)
  {
    const FilterType::ConfusionMatrixType & cm = filter->GetConfusionMatrix(rater);
    ASSERT_EQ(cm.rows(), 3u);
    ASSERT_EQ(cm.cols(), 2u);
    const double expected[3][2] = { { 1.0, 0.0 }, { 0.0, 1.0 }, { 0.0, 0.0 } };
    for (unsigned int r = 0; r < 3; ++r)
    {
      for (unsigned int c = 0; c < 2; ++c)
      {
        EXPECT_NEAR(cm(r, c), expected[r][c], 1e-6) << "rater " << rater << " (" << r << ',' << c << ')';
      }
    }
  }

  const unsigned char                 expectedOutput[4] = { 0, 1, 2, 2 };
  itk::ImageRegionIterator<ImageType> out(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  for (const unsigned char expectedLabel : expectedOutput)
  {
    EXPECT_EQ(out.Get(), expectedLabel);
    ++out;
  }
}

TEST(MultiLabelSTAPLEImageFilter, ThrowsWhenUndecidedLabelDoesNotFitOutputType)
{
  auto filter = FilterType::New();
  filter->SetInput(0, MakeUniformImage<ImageType>(0));
  filter->SetInput(1, MakeUniformImage<ImageType>(255));

  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(MultiLabelSTAPLEImageFilter, DefaultUndecidedLabelWhenRepresentable)
{
  auto filter = WideFilterType::New();
  filter->SetInput(0, MakeUniformImage<WideImageType>(0));
  filter->SetInput(1, MakeUniformImage<WideImageType>(255));
  filter->SetMaximumNumberOfIterations(0);

  EXPECT_NO_THROW(filter->Update());
  EXPECT_EQ(256, filter->GetLabelForUndecidedPixels());
}

// Loop counters bounded by m_TotalLabelCount must not be pixel-typed: a saturated
// unsigned char label space (issue #6575, B84) wraps such a counter and never terminates.
TEST(MultiLabelSTAPLEImageFilter, SaturatedUnsignedCharLabelCountDoesNotHang)
{
  auto filter = FilterType::New();
  filter->SetInput(0, MakeRaterImage({ 255, 0, 0, 0 }));
  filter->SetInput(1, MakeRaterImage({ 255, 1, 1, 1 }));
  filter->SetLabelForUndecidedPixels(0);
  filter->SetMaximumNumberOfIterations(1);

  auto              done = std::make_shared<std::promise<void>>();
  std::future<void> future = done->get_future();
  std::thread       worker([filter, done]() mutable {
    filter->Update();
    done->set_value();
  });

  const bool finished = (future.wait_for(std::chrono::seconds(30)) == std::future_status::ready);
  if (!finished)
  {
    // A stuck Update() cannot be killed portably; abort rather than detach and
    // leak a CPU-spinning thread that would corrupt later tests in this binary.
    std::cerr << "Update() did not terminate for a saturated unsigned char label count" << std::endl;
    std::abort();
  }
  worker.join();
}

// A saturated label space leaves no representable undecided label, so the filter
// must refuse rather than silently wrap the default onto real label 0 (issue #6575).
TEST(MultiLabelSTAPLEImageFilter, SaturatedLabelsWithoutExplicitUndecidedLabelThrows)
{
  auto filter = FilterType::New();
  filter->SetInput(0, MakeRaterImage({ 255, 0, 0, 0 }));
  filter->SetInput(1, MakeRaterImage({ 255, 1, 1, 1 }));
  filter->SetMaximumNumberOfIterations(1);
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

// Saturated label space (max label 255 -> m_TotalLabelCount 256) with the caller
// mapping undecided pixels to real label 0: genuine label-0 consensus must still
// seed the matrices. The voting-undecided sentinel must not collide with label 0.
TEST(MultiLabelSTAPLEImageFilter, SaturatedConsensusOnLabelZeroSeedsMatrixWhenUndecidedLabelIsZero)
{
  auto filter = FilterType::New();
  // Pixel 0: unanimous label 255 -> saturates unsigned char (m_TotalLabelCount = 256).
  // Pixels 1-3: unanimous label 0 (genuine consensus, no ties).
  filter->SetInput(0, MakeRaterImage({ 255, 0, 0, 0 }));
  filter->SetInput(1, MakeRaterImage({ 255, 0, 0, 0 }));
  filter->SetLabelForUndecidedPixels(0);
  filter->SetMaximumNumberOfIterations(0);
  filter->Update();

  for (unsigned int rater = 0; rater < 2; ++rater)
  {
    const FilterType::ConfusionMatrixType & cm = filter->GetConfusionMatrix(rater);
    EXPECT_NEAR(cm(0, 0), 1.0, 1e-6) << "rater " << rater << ": label-0 consensus excluded from seeding";
    EXPECT_NEAR(cm(255, 255), 1.0, 1e-6) << "rater " << rater << ": label-255 consensus";
  }
}

// Consensus on label 0 must seed the matrices even when the caller maps undecided
// pixels to 0; only genuine ties are excluded (issue #6575).
TEST(MultiLabelSTAPLEImageFilter, ConsensusOnLabelZeroSeedsMatrixWhenUndecidedLabelIsZero)
{
  auto filter = FilterType::New();
  filter->SetInput(0, MakeRaterImage({ 0, 1, 0, 0 }));
  filter->SetInput(1, MakeRaterImage({ 0, 0, 0, 0 }));
  filter->SetLabelForUndecidedPixels(0);
  filter->SetMaximumNumberOfIterations(0);
  filter->Update();

  // Pixels 0, 2, 3 are label-0 consensus and must be counted; pixel 1 is a tie and is skipped.
  for (unsigned int rater = 0; rater < 2; ++rater)
  {
    const FilterType::ConfusionMatrixType & cm = filter->GetConfusionMatrix(rater);
    ASSERT_EQ(cm.rows(), 3u);
    ASSERT_EQ(cm.cols(), 2u);
    const double expected[3][2] = { { 1.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 } };
    for (unsigned int r = 0; r < 3; ++r)
    {
      for (unsigned int c = 0; c < 2; ++c)
      {
        EXPECT_NEAR(cm(r, c), expected[r][c], 1e-6) << "rater " << rater << " (" << r << ',' << c << ')';
      }
    }
  }
}
