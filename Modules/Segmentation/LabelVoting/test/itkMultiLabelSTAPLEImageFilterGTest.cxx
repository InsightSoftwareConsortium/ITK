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
#include <future>
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

// Voting-tie pixels carry the undecided label (one past the confusion matrix's last column)
// and must be skipped when seeding, not written past the row (issue #6575, B10).
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
  std::thread([filter, done]() mutable {
    filter->Update();
    done->set_value();
  }).detach();

  ASSERT_EQ(future.wait_for(std::chrono::seconds(2)), std::future_status::ready)
    << "Update() did not terminate for a saturated unsigned char label count";
}
