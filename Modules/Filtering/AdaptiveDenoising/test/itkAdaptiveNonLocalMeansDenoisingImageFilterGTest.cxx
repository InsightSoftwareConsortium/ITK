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
  const auto                              image = MakeNoisyImage();
  const auto                              mask = MakeMask();
  const ImageType::RegionType             fullRegion = image->GetLargestPossibleRegion();
  const std::vector<ImageType::IndexType> fullIndices{
    { { 0, 0, 0 } }, { { 7, 5, 3 } }, { { 10, 8, 15 } }, { { 14, 15, 24 } }, { { 19, 15, 29 } }
  };
  // Search positions two voxels outside this region have no patch voxel inside it.
  const ImageType::RegionType             partialRegion{ { { 5, 4, 10 } }, { { 10, 8, 8 } } };
  const std::vector<ImageType::IndexType> partialIndices{ { { 5, 4, 10 } }, { { 9, 7, 13 } }, { { 14, 11, 17 } } };

  // Regression baseline: one work unit.
  const std::vector<std::vector<double>> fullBaseline{ { 30.9319801, 73.2338638, 116.319756, 124.369247, 117.350815 },
                                                       { 50.5600014, 74.2700119, 116.319756, 138.078995, 175.670013 },
                                                       { 40.0166969, 73.3192902, 116.381752, 136.766998, 142.077423 },
                                                       { 50.5600014, 74.269989, 116.381752, 143.695862, 175.670013 } };
  // The partial-region output is out of range; these values pin it as it is, not as it should be.
  const std::vector<std::vector<double>> partialBaseline{ { 214.356491, 2852.40698, 862.973755 },
                                                          { 214.356491, 2852.40698, 862.973755 },
                                                          { 19795.2949, 301765.156, 112713.0 },
                                                          { 19795.2949, 301765.156, 112713.0 } };
  // Contraction to FMA and the libm exp move outputs by a few float ulps between platforms.
  constexpr double relativeTolerance = 1e-6;

  size_t c = 0;
  for (const bool useRicianNoiseModel : { false, true })
  {
    for (const MaskType * maskImage : std::vector<const MaskType *>{ nullptr, mask })
    {
      for (const bool partial : { false, true })
      {
        const ImageType::RegionType               region = partial ? partialRegion : fullRegion;
        const std::vector<ImageType::IndexType> & indices = partial ? partialIndices : fullIndices;
        const std::vector<double> &               baseline = partial ? partialBaseline[c] : fullBaseline[c];
        const std::vector<double>                 values =
          ValuesAt(Denoise(image, useRicianNoiseModel, 1, maskImage, region), region, indices);
        for (size_t k = 0; k < indices.size(); ++k)
        {
          EXPECT_NEAR(values[k], baseline[k], relativeTolerance * std::abs(baseline[k]))
            << "useRicianNoiseModel: " << useRicianNoiseModel << ", mask: " << (maskImage != nullptr)
            << ", requested region: " << region << ", index: " << indices[k];
        }
      }
      ++c;
    }
  }
}
