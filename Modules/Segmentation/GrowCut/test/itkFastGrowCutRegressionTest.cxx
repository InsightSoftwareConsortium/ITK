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

#include "itkFastGrowCut.h"

#include "itkImageDuplicator.h"
#include "itkImageRegionIterator.h"
#include "itkTestingMacros.h"

namespace
{
constexpr unsigned int Dimension = 3;
using PixelType = short;
using ImageType = itk::Image<PixelType, Dimension>;
using LabelType = itk::Image<unsigned char, Dimension>;
using FGCType = itk::FastGrowCut<ImageType, LabelType>;

ImageType::Pointer
MakeImage(short background)
{
  auto                  image = ImageType::New();
  ImageType::RegionType region;
  region.SetSize({ { 30, 5, 3 } });
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(background);
  return image;
}

LabelType::Pointer
MakeSeeds()
{
  auto                  seeds = LabelType::New();
  LabelType::RegionType region;
  region.SetSize({ { 30, 5, 3 } });
  seeds->SetRegions(region);
  seeds->Allocate();
  seeds->FillBuffer(0);
  seeds->SetPixel({ { 1, 2, 1 } }, 1);
  seeds->SetPixel({ { 28, 1, 1 } }, 2);
  return seeds;
}

itk::SizeValueType
CountDifferingVoxels(const LabelType * a, const LabelType * b)
{
  itk::SizeValueType                       differing = 0;
  itk::ImageRegionConstIterator<LabelType> ita(a, a->GetBufferedRegion());
  itk::ImageRegionConstIterator<LabelType> itb(b, b->GetBufferedRegion());
  for (; !ita.IsAtEnd(); ++ita, ++itb)
  {
    if (ita.Get() != itb.Get())
    {
      ++differing;
    }
  }
  return differing;
}

// Low-intensity channel along x at (y=2, z=1) joins seed 1 to the far end.
// With DistancePenalty 0 the probe voxel near the far end follows the
// same-intensity channel back to seed 1; with a large penalty the spatial
// cost makes the adjacent seed 2 win instead, so the segmentation differs.
ImageType::Pointer
MakeChannelImage()
{
  ImageType::Pointer image = MakeImage(200);
  for (unsigned int x = 0; x < 30; ++x)
  {
    image->SetPixel({ { x, 2, 1 } }, 0);
  }
  return image;
}

LabelType::Pointer
DeepCopy(const LabelType * source)
{
  auto duplicator = itk::ImageDuplicator<LabelType>::New();
  duplicator->SetInputImage(source);
  duplicator->Update();
  return duplicator->GetOutput();
}

LabelType::Pointer
RunFromScratch(const ImageType * image, const LabelType * seeds, double penalty)
{
  auto filter = FGCType::New();
  filter->SetInput(image);
  filter->SetSeedImage(seeds);
  filter->SetDistancePenalty(penalty);
  filter->Update();
  return DeepCopy(filter->GetOutput());
}
} // namespace

int
itkFastGrowCutRegressionTest(int, char *[])
{
  LabelType::Pointer seeds = MakeSeeds();

  // --- Regression for ITKGrowCut#18: changing DistancePenalty after an
  //     initial Update() must recompute, not reuse the cached distance
  //     volume. ---
  {
    ImageType::Pointer image = MakeChannelImage();

    LabelType::Pointer refLowPenalty = RunFromScratch(image, seeds, 0.0);
    LabelType::Pointer refHighPenalty = RunFromScratch(image, seeds, 10.0);

    // The phantom is designed so the two penalties disagree; otherwise the
    // test below cannot detect a missing recomputation.
    ITK_TEST_EXPECT_TRUE(CountDifferingVoxels(refLowPenalty, refHighPenalty) > 0);

    auto filter = FGCType::New();
    filter->SetInput(image);
    filter->SetSeedImage(seeds);
    filter->SetDistancePenalty(0.0);
    filter->Update();

    filter->SetDistancePenalty(10.0);
    filter->Update();

    const itk::SizeValueType staleVoxels = CountDifferingVoxels(filter->GetOutput(), refHighPenalty);
    if (staleVoxels != 0)
    {
      std::cerr << "FAILED: after SetDistancePenalty()+Update() the result "
                << "differs from a from-scratch high-penalty run in " << staleVoxels
                << " voxels; the cached distance volume was not recomputed "
                << "(ITKGrowCut#18)." << std::endl;
      return EXIT_FAILURE;
    }
  }

  // --- Changing the intensity image and calling Reset() before Update()
  //     must recompute from scratch (documented API contract). ---
  {
    // image1 has a low-intensity channel that lets seed 1 flood it;
    // image2 is uniform so the segmentation is purely distance-driven.
    // The two therefore produce materially different labelings.
    ImageType::Pointer image1 = MakeChannelImage();
    ImageType::Pointer image2 = MakeImage(200);

    auto filter = FGCType::New();
    filter->SetInput(image1);
    filter->SetSeedImage(seeds);
    filter->SetDistancePenalty(0.0);
    filter->Update();
    LabelType::Pointer out1 = DeepCopy(filter->GetOutput());

    filter->SetInput(image2);
    filter->Reset();
    filter->Update();

    LabelType::Pointer       reference = RunFromScratch(image2, seeds, 0.0);
    const itk::SizeValueType staleVoxels = CountDifferingVoxels(filter->GetOutput(), reference);
    if (staleVoxels != 0)
    {
      std::cerr << "FAILED: after SetInput()+Reset()+Update() the result "
                << "differs from a from-scratch run in " << staleVoxels
                << " voxels; Reset() did not force recomputation." << std::endl;
      return EXIT_FAILURE;
    }
    ITK_TEST_EXPECT_TRUE(CountDifferingVoxels(out1, reference) > 0);
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
