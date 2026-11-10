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

#include "itkAdaptiveNonLocalMeansDenoisingImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMultiThreaderBase.h"
#include "itkGTest.h"

#include <cmath>
#include <optional>
#include <random>
#include <vector>

namespace
{
using ImageType = itk::Image<float, 3>;
using FilterType = itk::AdaptiveNonLocalMeansDenoisingImageFilter<ImageType, ImageType>;
using MaskType = FilterType::MaskImageType;

const ImageType::SizeType           imageSize{ { 20, 16, 30 } };
constexpr std::mt19937::result_type noiseSeed = 42;

ImageType::Pointer
MakeNoisyImage()
{
  auto image = ImageType::New();
  image->SetRegions(imageSize);
  image->Allocate();

  // mt19937 output is specified by the standard, unlike the distributions, so the image is the same everywhere.
  std::mt19937 generator(noiseSeed);
  const auto   noise = [&generator] { return static_cast<float>(static_cast<int>(generator() % 801) - 400) / 100.0F; };
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const auto & index = it.GetIndex();
    it.Set(std::abs(50.0F + 2.0F * index[0] + 3.0F * index[2] + noise()));
  }
  return image;
}

MaskType::Pointer
MakeMask()
{
  auto mask = MaskType::New();
  mask->SetRegions(imageSize);
  mask->Allocate();
  for (itk::ImageRegionIteratorWithIndex<MaskType> it(mask, mask->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const auto & index = it.GetIndex();
    it.Set(index[0] >= 4 && index[0] < 15 && index[2] >= 6 && index[2] < 25 ? 1 : 0);
  }
  return mask;
}

std::vector<float>
Denoise(const ImageType *                            image,
        bool                                         useRicianNoiseModel,
        itk::ThreadIdType                            numberOfWorkUnits,
        const MaskType *                             mask,
        const std::optional<ImageType::RegionType> & requestedRegion = std::nullopt)
{
  auto filter = FilterType::New();
  filter->SetInput(image);
  if (mask != nullptr)
  {
    filter->SetMaskImage(mask);
  }
  filter->SetUseRicianNoiseModel(useRicianNoiseModel);
  filter->SetNeighborhoodPatchRadius(FilterType::NeighborhoodRadiusType::Filled(1));
  filter->SetNeighborhoodSearchRadius(FilterType::NeighborhoodRadiusType::Filled(2));
  filter->SetNumberOfWorkUnits(numberOfWorkUnits);
  if (requestedRegion)
  {
    filter->GetOutput()->SetRequestedRegion(*requestedRegion);
  }
  filter->Update();

  const ImageType * output = filter->GetOutput();
  return { output->GetBufferPointer(), output->GetBufferPointer() + output->GetBufferedRegion().GetNumberOfPixels() };
}

// The values at the given indices of an output buffered on the given region.
std::vector<double>
ValuesAt(const std::vector<float> &                output,
         const ImageType::RegionType &             region,
         const std::vector<ImageType::IndexType> & indices)
{
  const auto layout = ImageType::New();
  layout->SetRegions(region);
  std::vector<double> values;
  for (const auto & index : indices)
  {
    values.push_back(output.at(layout->ComputeOffset(index)));
  }
  return values;
}
} // namespace

TEST(AdaptiveNonLocalMeansDenoisingImageFilter, MatchesBaseline)
{
  const auto image = MakeNoisyImage();
  const auto mask = MakeMask();
  // The last two voxels depend on which covering center the Rician bias gather takes.
  const std::vector<ImageType::IndexType> indices{ { { 0, 0, 0 } },    { { 7, 5, 3 } },    { { 10, 8, 15 } },
                                                   { { 14, 15, 24 } }, { { 19, 15, 29 } }, { { 5, 12, 5 } },
                                                   { { 3, 12, 6 } } };

  // Regression baseline: one work unit of main, checked at every number of work units.
  const std::vector<std::vector<double>> baseline{
    { 30.9319801, 73.2338638, 116.319756, 124.369247, 117.350815, 73.175293, 73.8802185 },
    { 50.5600014, 74.2700119, 116.319756, 138.078995, 175.670013, 72.4503555, 75.1968307 },
    { 40.0166969, 73.3192902, 116.381752, 136.766998, 142.077423, 73.2446442, 73.9816513 },
    { 50.5600014, 74.269989, 116.381752, 143.695862, 175.670013, 72.4758453, 75.2052612 }
  };
  // The slab summation order, contraction to FMA and the libm exp each move outputs by under 1e-6 relative.
  constexpr double relativeTolerance = 2e-6;

  size_t c = 0;
  for (const bool useRicianNoiseModel : { false, true })
  {
    for (const MaskType * maskImage : std::vector<const MaskType *>{ nullptr, mask })
    {
      for (const itk::ThreadIdType numberOfWorkUnits : { 1, 3, 8 })
      {
        const std::vector<double> values = ValuesAt(Denoise(image, useRicianNoiseModel, numberOfWorkUnits, maskImage),
                                                    image->GetLargestPossibleRegion(),
                                                    indices);
        for (size_t k = 0; k < indices.size(); ++k)
        {
          EXPECT_NEAR(values[k], baseline[c][k], relativeTolerance * std::abs(baseline[c][k]))
            << "useRicianNoiseModel: " << useRicianNoiseModel << ", mask: " << (maskImage != nullptr)
            << ", numberOfWorkUnits: " << numberOfWorkUnits << ", index: " << indices[k];
        }
      }
      ++c;
    }
  }
}

TEST(AdaptiveNonLocalMeansDenoisingImageFilter, OutputDoesNotDependOnNumberOfWorkUnits)
{
  // Work units must run concurrently for a race between them to show.
  if (itk::MultiThreaderBase::GetGlobalDefaultNumberOfThreads() < 2)
  {
    GTEST_SKIP() << "Needs at least 2 threads to run work units concurrently.";
  }
  const auto image = MakeNoisyImage();
  const auto mask = MakeMask();
  for (const bool useRicianNoiseModel : { false, true })
  {
    for (const MaskType * maskImage : std::vector<const MaskType *>{ nullptr, mask })
    {
      const std::vector<float> reference = Denoise(image, useRicianNoiseModel, 1, maskImage);
      for (const itk::ThreadIdType numberOfWorkUnits : { 3, 8 })
      {
        EXPECT_EQ(Denoise(image, useRicianNoiseModel, numberOfWorkUnits, maskImage), reference)
          << "useRicianNoiseModel: " << useRicianNoiseModel << ", mask: " << (maskImage != nullptr)
          << ", numberOfWorkUnits: " << numberOfWorkUnits;
      }
    }
  }
  // A requested region off the origin, Gaussian model only: the Rician partial-region output is not reproducible.
  const ImageType::RegionType requestedRegion({ { 3, 2, 5 } }, { { 12, 10, 17 } });
  EXPECT_EQ(Denoise(image, false, 8, mask, requestedRegion), Denoise(image, false, 1, mask, requestedRegion))
    << "requested region";
}

TEST(AdaptiveNonLocalMeansDenoisingImageFilter, AbortStopsTheSlabPass)
{
  // Many thin slabs, each smaller than the progress update interval of the slab pass.
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType{ { 16, 16, 512 } });
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion()); !it.IsAtEnd(); ++it)
  {
    const auto & index = it.GetIndex();
    it.Set(static_cast<float>(50 + (7 * index[0] + 3 * index[1] + 5 * index[2]) % 11));
  }
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetNeighborhoodPatchRadius(FilterType::NeighborhoodRadiusType::Filled(1));
  filter->SetNeighborhoodSearchRadius(FilterType::NeighborhoodRadiusType::Filled(1));
  // Two work units, so that each has many slabs left when the abort is requested.
  filter->SetNumberOfWorkUnits(2);
  // Progress events come from the updating thread, which also runs the first work unit; the preprocessing ends at 5 %.
  filter->AddObserver(itk::ProgressEvent(), [&filter](const itk::EventObject &) {
    if (filter->GetProgress() > 0.1F)
    {
      filter->AbortGenerateDataOn();
    }
  });
  EXPECT_THROW(filter->Update(), itk::ProcessAborted);
}
