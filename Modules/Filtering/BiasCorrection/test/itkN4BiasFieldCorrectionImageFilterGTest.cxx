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

#include "itkN4BiasFieldCorrectionImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGTest.h"

#include <cmath>
#include <utility>
#include <vector>

// A constant image has a degenerate histogram range; sharpening must degrade to an
// identity mapping instead of producing NaN bin indices (issue #6575, B2).
TEST(N4BiasFieldCorrectionImageFilter, ConstantImageProducesFiniteOutput)
{
  using ImageType = itk::Image<float, 2>;
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(32) });
  image->Allocate();
  image->FillBuffer(1.0F);

  using FilterType = itk::N4BiasFieldCorrectionImageFilter<ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetMaximumNumberOfIterations(FilterType::VariableSizeArrayType(1, 2));
  filter->Update();

  itk::ImageRegionConstIterator<ImageType> it(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    ASSERT_TRUE(std::isfinite(it.Get()));
    ASSERT_NEAR(it.Get(), 1.0F, 1e-2) << " constant input must be (nearly) unchanged";
  }
}

namespace
{
using N4ImageType = itk::Image<float, 2>;
using N4FilterType = itk::N4BiasFieldCorrectionImageFilter<N4ImageType>;
using N4MaskType = N4FilterType::MaskImageType;
using N4ConfidenceType = N4FilterType::RealImageType;

constexpr itk::SizeValueType n4Size = 48;

template <typename TImage, typename TFunction>
typename TImage::Pointer
MakeN4Image(TFunction function)
{
  auto image = TImage::New();
  image->SetRegions(TImage::SizeType::Filled(n4Size));
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<TImage> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    it.Set(function(it.GetIndex()[0], it.GetIndex()[1]));
  }
  return image;
}

// Label 0 is a border, label 2 a central disk, and label 1 the rest.
N4MaskType::Pointer
MakeLabelMask()
{
  return MakeN4Image<N4MaskType>([](double x, double y) {
    const double r = std::hypot(x - 24.0, y - 24.0);
    return (x < 3 || y < 3 || x > 44 || y > 44) ? 0 : (r < 10.0 ? 2 : 1);
  });
}

// Weights in (0, 1] that grow along y, and 0 for x > 40.
N4ConfidenceType::Pointer
MakeConfidence()
{
  return MakeN4Image<N4ConfidenceType>([](double x, double y) { return x > 40 ? 0.0 : 0.25 + 0.75 * y / 47.0; });
}

struct N4Inputs
{
  const N4MaskType *       mask{ nullptr };
  bool                     useMaskLabel{ false };
  N4MaskType::PixelType    maskLabel{ 1 };
  const N4ConfidenceType * confidence{ nullptr };
};

std::vector<float>
RunN4(const N4Inputs & inputs, double * convergenceMeasurement = nullptr)
{
  const auto image = MakeN4Image<N4ImageType>([](double x, double y) {
    const double tissue = (std::hypot(x - 24.0, y - 24.0) < 10.0 ? 140.0 : 100.0) + 20.0 * std::sin(0.3 * x);
    return tissue * std::exp(0.3 * (x - 24.0) / 24.0 + 0.2 * std::pow((y - 24.0) / 24.0, 2));
  });

  auto filter = N4FilterType::New();
  filter->SetInput(image);
  if (inputs.mask != nullptr)
  {
    filter->SetMaskImage(inputs.mask);
  }
  filter->SetUseMaskLabel(inputs.useMaskLabel);
  filter->SetMaskLabel(inputs.maskLabel);
  if (inputs.confidence != nullptr)
  {
    filter->SetConfidenceImage(inputs.confidence);
  }
  filter->SetNumberOfFittingLevels(2);
  filter->SetMaximumNumberOfIterations(N4FilterType::VariableSizeArrayType(2, 5));
  filter->SetConvergenceThreshold(0.0);
  filter->Update();
  if (convergenceMeasurement != nullptr)
  {
    *convergenceMeasurement = filter->GetCurrentConvergenceMeasurement();
  }

  const N4ImageType * output = filter->GetOutput();
  return { output->GetBufferPointer(), output->GetBufferPointer() + output->GetBufferedRegion().GetNumberOfPixels() };
}
} // namespace

TEST(N4BiasFieldCorrectionImageFilter, MaskLabelMatchesBinaryMask)
{
  const auto labelMask = MakeLabelMask();
  const auto binaryMask = MakeN4Image<N4MaskType>([&labelMask](double x, double y) {
    return labelMask->GetPixel({ { static_cast<itk::IndexValueType>(x), static_cast<itk::IndexValueType>(y) } }) == 1;
  });
  EXPECT_EQ(RunN4({ labelMask, true, 1 }), RunN4({ binaryMask, false }));
}

TEST(N4BiasFieldCorrectionImageFilter, BinaryConfidenceMatchesMask)
{
  const auto labelMask = MakeLabelMask();
  const auto binaryConfidence = MakeN4Image<N4ConfidenceType>([&labelMask](double x, double y) {
    return labelMask->GetPixel({ { static_cast<itk::IndexValueType>(x), static_cast<itk::IndexValueType>(y) } }) == 2
             ? 1.0
             : 0.0;
  });
  EXPECT_EQ(RunN4({ nullptr, false, 1, binaryConfidence }), RunN4({ labelMask, true, 2 }));
}

TEST(N4BiasFieldCorrectionImageFilter, MaskLabelAndConfidenceMatchBaseline)
{
  const auto labelMask = MakeLabelMask();
  const auto confidence = MakeConfidence();
  double     convergenceMeasurement = 0.0;
  const auto output = RunN4({ labelMask, true, 1, confidence }, &convergenceMeasurement);

  // Regression baseline.
  const std::vector<std::pair<itk::SizeValueType, double>> baseline{ { 0, 88.56924438 },
                                                                     { 5 * n4Size + 5, 105.4799347 },
                                                                     { 24 * n4Size + 8, 94.64616394 },
                                                                     { 40 * n4Size + 30, 119.6012878 },
                                                                     { 47 * n4Size + 47, 174.3816833 } };
  for (const auto & [offset, expected] : baseline)
  {
    EXPECT_NEAR(output[offset], expected, 1e-4 * expected) << "offset " << offset;
  }
  // From ITK main with one thread; other thread counts change it by up to 4e-6 relative through the B-spline fit.
  constexpr double expectedConvergenceMeasurement = 0.0029679266735911369;
  EXPECT_NEAR(convergenceMeasurement, expectedConvergenceMeasurement, 1e-4 * expectedConvergenceMeasurement);
  EXPECT_EQ(RunN4({ labelMask, true, 1, confidence }), output);
}

namespace
{
using N4ReleaseImageType = itk::Image<float, 2>;
using N4ReleaseFilterType = itk::N4BiasFieldCorrectionImageFilter<N4ReleaseImageType>;

class GlobalReleaseDataFlagGuard
{
public:
  GlobalReleaseDataFlagGuard()
    : m_Previous(itk::DataObject::GetGlobalReleaseDataFlag())
  {
    itk::DataObject::SetGlobalReleaseDataFlag(true);
  }
  ~GlobalReleaseDataFlagGuard() { itk::DataObject::SetGlobalReleaseDataFlag(m_Previous); }
  ITK_DISALLOW_COPY_AND_MOVE(GlobalReleaseDataFlagGuard);

private:
  bool m_Previous;
};

std::vector<float>
RunN4WithMaskAndConfidence()
{
  constexpr itk::SizeValueType size = 40;
  auto                         image = N4ReleaseImageType::New();
  image->SetRegions(N4ReleaseImageType::SizeType::Filled(size));
  image->Allocate();
  auto mask = N4ReleaseFilterType::MaskImageType::New();
  mask->SetRegions(image->GetLargestPossibleRegion());
  mask->Allocate();
  auto confidence = N4ReleaseFilterType::RealImageType::New();
  confidence->SetRegions(image->GetLargestPossibleRegion());
  confidence->Allocate();
  for (itk::ImageRegionIteratorWithIndex<N4ReleaseImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const double x = it.GetIndex()[0];
    const double y = it.GetIndex()[1];
    const double tissue = (std::hypot(x - 20.0, y - 20.0) < 8.0 ? 140.0 : 100.0) + 20.0 * std::sin(0.3 * x);
    it.Set(static_cast<float>(tissue * std::exp(0.3 * (x - 20.0) / 20.0 + 0.2 * std::pow((y - 20.0) / 20.0, 2))));
    mask->SetPixel(it.GetIndex(), (x < 2 || y < 2 || x > 37 || y > 37) ? 0 : 1);
    confidence->SetPixel(it.GetIndex(), x > 33 ? 0.0F : static_cast<float>(0.25 + 0.75 * y / (size - 1)));
  }

  auto filter = N4ReleaseFilterType::New();
  filter->SetInput(image);
  filter->SetMaskImage(mask);
  filter->SetConfidenceImage(confidence);
  filter->SetNumberOfFittingLevels(2);
  filter->SetMaximumNumberOfIterations(N4ReleaseFilterType::VariableSizeArrayType(2, 4));
  filter->SetConvergenceThreshold(0.0);
  filter->Update();

  const N4ReleaseImageType * output = filter->GetOutput();
  return { output->GetBufferPointer(), output->GetBufferPointer() + output->GetBufferedRegion().GetNumberOfPixels() };
}
} // namespace

TEST(N4BiasFieldCorrectionImageFilter, GlobalReleaseDataFlagDoesNotChangeOutput)
{
  const std::vector<float>         expected = RunN4WithMaskAndConfidence();
  const GlobalReleaseDataFlagGuard releaseData;
  EXPECT_EQ(RunN4WithMaskAndConfidence(), expected);
}
