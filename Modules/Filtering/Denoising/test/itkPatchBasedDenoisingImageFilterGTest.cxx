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

#include "itkPatchBasedDenoisingImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkGTest.h"

namespace
{
template <typename TImage>
typename TImage::Pointer
RunPoissonDenoising(const TImage * input, double fidelityWeight)
{
  using FilterType = itk::PatchBasedDenoisingImageFilter<TImage, TImage>;
  auto filter = FilterType::New();
  filter->SetInput(input);
  filter->SetNoiseModel(FilterType::NoiseModelEnum::POISSON);
  filter->SetNoiseModelFidelityWeight(fidelityWeight);
  filter->SetNumberOfIterations(5);
  filter->SetPatchRadius(2);
  filter->Update();

  typename TImage::Pointer output = filter->GetOutput();
  output->DisconnectPipeline();
  return output;
}
} // namespace

// For integer pixels, the POISSON fidelity step size must not truncate to 0 (issue #6575, B26).
TEST(PatchBasedDenoisingImageFilter, PoissonFidelityAffectsIntegerPixelOutput)
{
  constexpr unsigned int Dimension = 2;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;

  constexpr unsigned int size = 24;
  auto                   image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(size) });
  image->Allocate();

  itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    const auto index = it.GetIndex();
    const bool block = ((index[0] / 4) + (index[1] / 4)) % 2 == 0;
    it.Set(block ? 10 : 90);
  }

  const ImageType::Pointer withoutFidelity = RunPoissonDenoising<ImageType>(image, 0.0);
  const ImageType::Pointer withFidelity = RunPoissonDenoising<ImageType>(image, 1.0);

  itk::ImageRegionConstIterator<ImageType> itA(withoutFidelity, withoutFidelity->GetBufferedRegion());
  itk::ImageRegionConstIterator<ImageType> itB(withFidelity, withFidelity->GetBufferedRegion());
  long long                                sumAbsDiff = 0;
  for (itA.GoToBegin(), itB.GoToBegin(); !itA.IsAtEnd(); ++itA, ++itB)
  {
    sumAbsDiff += std::abs(itA.Get() - itB.Get());
  }

  EXPECT_GT(sumAbsDiff, 400);
}
