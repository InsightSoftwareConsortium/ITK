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

#include "itkFlatStructuringElement.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkGrayscaleMorphologicalOpeningImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkGTest.h"

namespace
{
using ImageType = itk::Image<float, 2>;
using KernelType = itk::FlatStructuringElement<2>;
using AlgorithmEnum = itk::MathematicalMorphologyEnums::Algorithm;

ImageType::Pointer
MakeImage(const ImageType::SizeType & size)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(size));
  image->Allocate();
  float value = 0.0f;
  for (float *pixel = image->GetBufferPointer(), *end = pixel + image->GetBufferedRegion().GetNumberOfPixels();
       pixel != end;
       ++pixel)
  {
    *pixel = value;
    value = (value >= 4.0f) ? 0.0f : value + 1.0f;
  }
  return image;
}

template <typename TFilter>
ImageType::Pointer
Filter(const ImageType * input, const KernelType::RadiusType & radius, AlgorithmEnum algorithm)
{
  auto filter = TFilter::New();
  filter->SetInput(input);
  filter->SetKernel(KernelType::Box(radius));
  filter->SetAlgorithm(algorithm);
  filter->Update();
  return filter->GetOutput();
}

// Regions with size 1 along a dimension arise from thin images and from per-thread regions.
template <typename TFilter>
void
ExpectSameAsBasicAlgorithm()
{
  const ImageType::SizeType    sizes[] = { { { 16, 1 } }, { { 1, 16 } }, { { 16, 3 } }, { { 3, 16 } } };
  const KernelType::RadiusType radii[] = { { { 1, 1 } }, { { 1, 0 } }, { { 0, 1 } } };
  for (const auto algorithm : { AlgorithmEnum::ANCHOR, AlgorithmEnum::VHGW })
  {
    for (const auto & size : sizes)
    {
      for (const auto & radius : radii)
      {
        SCOPED_TRACE(testing::Message() << algorithm << " size " << size << " radius " << radius);
        const auto input = MakeImage(size);
        const auto actual = Filter<TFilter>(input, radius, algorithm);
        const auto expected = Filter<TFilter>(input, radius, AlgorithmEnum::BASIC);
        for (itk::ImageRegionConstIteratorWithIndex<ImageType> it(expected, expected->GetBufferedRegion());
             !it.IsAtEnd();
             ++it)
        {
          EXPECT_EQ(actual->GetPixel(it.GetIndex()), it.Get()) << "at " << it.GetIndex();
        }
      }
    }
  }
}
} // namespace

TEST(FlatMorphologyDegenerateRegion, DilateMatchesBasicAlgorithm)
{
  ExpectSameAsBasicAlgorithm<itk::GrayscaleDilateImageFilter<ImageType, ImageType, KernelType>>();
}

TEST(FlatMorphologyDegenerateRegion, OpeningMatchesBasicAlgorithm)
{
  ExpectSameAsBasicAlgorithm<itk::GrayscaleMorphologicalOpeningImageFilter<ImageType, ImageType, KernelType>>();
}
