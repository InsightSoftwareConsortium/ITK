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

#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkVectorImage.h"

#include <gtest/gtest.h>

namespace
{
using ImageType = itk::Image<float, 3>;
using FilterType = itk::SmoothingRecursiveGaussianImageFilter<ImageType>;

ImageType::Pointer
MakeImage(const ImageType::SizeType & size)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType({ { -2, 3, 5 } }, size));
  image->SetOrigin(ImageType::PointType{ { 7.0, -11.0, 2.0 } });
  image->SetSpacing(ImageType::SpacingType{ { 0.7, 1.3, 2.1 } });
  ImageType::DirectionType direction;
  direction.SetIdentity();
  direction[0][0] = 0;
  direction[0][1] = -1;
  direction[1][0] = 1;
  direction[1][1] = 0;
  image->SetDirection(direction);
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
  {
    const auto index = it.GetIndex();
    it.Set(static_cast<float>((index[0] * index[0] + 3 * index[1] + index[2] * index[2]) % 17));
  }
  return image;
}

ImageType::Pointer
SmoothEligibleAxes(const ImageType * input, const FilterType::SigmaArrayType & sigma)
{
  ImageType::ConstPointer previous = input;
  ImageType::Pointer      output;
  const auto              size = input->GetLargestPossibleRegion().GetSize();
  unsigned int            firstDirection = 0;
  bool                    hasActiveDirection = false;
  for (unsigned int d = 0; d < 3; ++d)
  {
    if (size[d] >= 4)
    {
      firstDirection = d;
      hasActiveDirection = true;
    }
  }
  if (!hasActiveDirection)
  {
    auto copy = itk::CastImageFilter<ImageType, ImageType>::New();
    copy->SetInput(input);
    copy->Update();
    output = copy->GetOutput();
    output->DisconnectPipeline();
    return output;
  }
  for (const unsigned int d : { firstDirection, 0u, 1u })
  {
    if (size[d] < 4 || (d == firstDirection && output.IsNotNull()))
    {
      continue;
    }
    auto recursive = itk::RecursiveGaussianImageFilter<ImageType>::New();
    recursive->SetInput(previous);
    recursive->SetDirection(d);
    recursive->SetSigma(sigma[d]);
    recursive->Update();
    output = recursive->GetOutput();
    output->DisconnectPipeline();
    previous = output;
  }
  return output;
}

void
ExpectSameImage(const ImageType * actual, const ImageType * expected)
{
  ASSERT_EQ(actual->GetLargestPossibleRegion(), expected->GetLargestPossibleRegion());
  EXPECT_EQ(actual->GetOrigin(), expected->GetOrigin());
  EXPECT_EQ(actual->GetSpacing(), expected->GetSpacing());
  EXPECT_EQ(actual->GetDirection(), expected->GetDirection());
  itk::ImageRegionConstIterator<ImageType> actualIt(actual, actual->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<ImageType> expectedIt(expected, expected->GetLargestPossibleRegion());
  for (; !actualIt.IsAtEnd(); ++actualIt, ++expectedIt)
  {
    EXPECT_FLOAT_EQ(actualIt.Get(), expectedIt.Get());
  }
}
} // namespace

TEST(SmoothingRecursiveGaussianImageFilter, ShortAxesAndRepeatedUpdates)
{
  const FilterType::SigmaArrayType sigma{ { 0.8, 1.7, 2.5 } };
  for (const bool inPlace : { false, true })
  {
    auto filter = FilterType::New();
    filter->SetSigmaArray(sigma);
    filter->SetInPlace(inPlace);
    for (const unsigned int x : { 1u, 2u, 3u, 4u, 9u })
    {
      for (const unsigned int y : { 1u, 2u, 3u, 4u, 9u })
      {
        for (const unsigned int z : { 1u, 2u, 3u, 4u, 9u })
        {
          const ImageType::SizeType size{ { x, y, z } };
          SCOPED_TRACE(::testing::Message() << "size=" << size << ", inPlace=" << inPlace);
          const auto image = MakeImage(size);
          const auto expected = SmoothEligibleAxes(image, sigma);
          filter->SetInput(image);
          ASSERT_NO_THROW(filter->Update());
          ExpectSameImage(filter->GetOutput(), expected);
        }
      }
    }
  }
}

TEST(SmoothingRecursiveGaussianImageFilter, AllShortAxesPreserveDoublePrecisionAndCast)
{
  using DoubleImageType = itk::Image<double, 3>;
  auto image = DoubleImageType::New();
  image->SetRegions(DoubleImageType::SizeType{ { 1, 2, 3 } });
  image->Allocate();
  constexpr double value = 1.0000000000000002;
  image->FillBuffer(value);
  for (const bool inPlace : { false, true })
  {
    auto filter = itk::SmoothingRecursiveGaussianImageFilter<DoubleImageType>::New();
    filter->SetInput(image);
    filter->SetInPlace(inPlace);
    ASSERT_NO_THROW(filter->Update());
    EXPECT_EQ(filter->GetOutput()->GetPixel({ { 0, 0, 0 } }), value);
  }

  using OutputImageType = itk::Image<unsigned char, 3>;
  const auto input = MakeImage({ { 2, 3, 1 } });
  auto       filter = itk::SmoothingRecursiveGaussianImageFilter<ImageType, OutputImageType>::New();
  filter->SetInput(input);
  ASSERT_NO_THROW(filter->Update());
  itk::ImageRegionConstIterator<ImageType>       inputIt(input, input->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<OutputImageType> outputIt(filter->GetOutput(), input->GetLargestPossibleRegion());
  for (; !inputIt.IsAtEnd(); ++inputIt, ++outputIt)
  {
    EXPECT_EQ(outputIt.Get(), static_cast<unsigned char>(inputIt.Get()));
  }
}

TEST(SmoothingRecursiveGaussianImageFilter, VectorImageShortAxes)
{
  using VectorImageType = itk::VectorImage<float, 3>;
  for (const ImageType::SizeType size : { ImageType::SizeType{ { 9, 2, 1 } }, ImageType::SizeType{ { 1, 2, 3 } } })
  {
    const auto scalarImage = MakeImage(size);
    auto       vectorImage = VectorImageType::New();
    vectorImage->CopyInformation(scalarImage);
    vectorImage->SetRegions(scalarImage->GetLargestPossibleRegion());
    vectorImage->SetNumberOfComponentsPerPixel(2);
    vectorImage->Allocate();
    itk::ImageRegionConstIterator<ImageType>  scalarIt(scalarImage, scalarImage->GetLargestPossibleRegion());
    itk::ImageRegionIterator<VectorImageType> vectorIt(vectorImage, vectorImage->GetLargestPossibleRegion());
    for (; !scalarIt.IsAtEnd(); ++scalarIt, ++vectorIt)
    {
      VectorImageType::PixelType pixel(2);
      pixel[0] = scalarIt.Get();
      pixel[1] = 2 * scalarIt.Get();
      vectorIt.Set(pixel);
    }
    auto filter = itk::SmoothingRecursiveGaussianImageFilter<VectorImageType>::New();
    filter->SetInput(vectorImage);
    ASSERT_NO_THROW(filter->Update());
    const auto expected = SmoothEligibleAxes(scalarImage, FilterType::SigmaArrayType(1.0));
    itk::ImageRegionConstIterator<ImageType>       expectedIt(expected, expected->GetLargestPossibleRegion());
    itk::ImageRegionConstIterator<VectorImageType> outputIt(filter->GetOutput(), expected->GetLargestPossibleRegion());
    for (; !expectedIt.IsAtEnd(); ++expectedIt, ++outputIt)
    {
      ASSERT_EQ(outputIt.Get().GetSize(), 2u);
      EXPECT_FLOAT_EQ(outputIt.Get()[0], expectedIt.Get());
      EXPECT_FLOAT_EQ(outputIt.Get()[1], 2 * expectedIt.Get());
    }
  }
}

TEST(SmoothingRecursiveGaussianImageFilter, IntegerPixelsWithShortFirstPipelineAxis)
{
  using IntegerImageType = itk::Image<unsigned char, 3>;
  const auto                       image = MakeImage({ { 9, 9, 1 } });
  const FilterType::SigmaArrayType sigma{ { 0.8, 1.7, 2.5 } };
  const auto                       expected = SmoothEligibleAxes(image, sigma);
  for (const bool inPlace : { false, true })
  {
    auto cast = itk::CastImageFilter<ImageType, IntegerImageType>::New();
    cast->SetInput(image);
    cast->Update();
    auto filter = itk::SmoothingRecursiveGaussianImageFilter<IntegerImageType>::New();
    filter->SetInput(cast->GetOutput());
    filter->SetSigmaArray(sigma);
    filter->SetInPlace(inPlace);
    ASSERT_NO_THROW(filter->Update());
    itk::ImageRegionConstIterator<ImageType>        expectedIt(expected, expected->GetLargestPossibleRegion());
    itk::ImageRegionConstIterator<IntegerImageType> outputIt(filter->GetOutput(), expected->GetLargestPossibleRegion());
    for (; !expectedIt.IsAtEnd(); ++expectedIt, ++outputIt)
    {
      EXPECT_EQ(outputIt.Get(), static_cast<unsigned char>(expectedIt.Get()));
    }
  }
}

TEST(SmoothingRecursiveGaussianImageFilter, InvalidSigmaAndEmptyAxesStillThrow)
{
  for (const ImageType::SizeType size : { ImageType::SizeType{ { 9, 9, 1 } }, ImageType::SizeType{ { 1, 2, 3 } } })
  {
    const auto image = MakeImage(size);
    for (const double sigma : { 0.0, -1.0 })
    {
      auto filter = FilterType::New();
      filter->SetInput(image);
      filter->SetSigmaArray(FilterType::SigmaArrayType{ { 1.0, 1.0, sigma } });
      EXPECT_THROW(filter->Update(), itk::ExceptionObject);
    }
  }
  auto filter = FilterType::New();
  filter->SetInput(MakeImage({ { 0, 9, 9 } }));
  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}
