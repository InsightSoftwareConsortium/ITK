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
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkVectorImage.h"

#include <gtest/gtest.h>
#include <limits>

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
Smooth(const ImageType * input, const FilterType::SigmaArrayType & sigma)
{
  auto filter = FilterType::New();
  filter->SetInput(input);
  filter->SetSigmaArray(sigma);
  filter->Update();
  ImageType::Pointer output = filter->GetOutput();
  output->DisconnectPipeline();
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

TEST(SmoothingRecursiveGaussianImageFilter, ShortAxisIsNotSmoothed)
{
  // Each image is constant along its long axes, with value 10 + 10 * (index along the short axis).
  for (unsigned int shortAxis = 0; shortAxis < 3; ++shortAxis)
  {
    for (const itk::SizeValueType shortLength : { 1, 2, 3 })
    {
      auto size = ImageType::SizeType::Filled(9);
      size[shortAxis] = shortLength;
      SCOPED_TRACE(::testing::Message() << "size=" << size);
      auto image = ImageType::New();
      image->SetRegions(size);
      image->Allocate();
      for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd();
           ++it)
      {
        it.Set(10.0f + 10.0f * static_cast<float>(it.GetIndex()[shortAxis]));
      }

      auto filter = FilterType::New();
      filter->SetInput(image);
      filter->SetSigma(2.0);
      ASSERT_NO_THROW(filter->Update());
      itk::ImageRegionConstIterator<ImageType> inputIt(image, image->GetLargestPossibleRegion());
      itk::ImageRegionConstIterator<ImageType> outputIt(filter->GetOutput(), image->GetLargestPossibleRegion());
      for (; !inputIt.IsAtEnd(); ++inputIt, ++outputIt)
      {
        EXPECT_NEAR(outputIt.Get(), inputIt.Get(), 1e-3);
      }
    }
  }
}

TEST(SmoothingRecursiveGaussianImageFilter, FourPixelAxisIsSmoothed)
{
  // Values 10, 20, 30, 40 along the 4-pixel axis; smoothing pulls both ends toward the middle.
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType{ { 9, 9, 4 } });
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
  {
    it.Set(10.0f + 10.0f * static_cast<float>(it.GetIndex()[2]));
  }
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetSigma(2.0);
  ASSERT_NO_THROW(filter->Update());
  EXPECT_GT(filter->GetOutput()->GetPixel({ { 4, 4, 0 } }), 11.0f);
  EXPECT_LT(filter->GetOutput()->GetPixel({ { 4, 4, 3 } }), 39.0f);
}

TEST(SmoothingRecursiveGaussianImageFilter, SingletonSliceMatchesTwoDimensionalImage)
{
  using Image2DType = itk::Image<float, 2>;
  const auto image = MakeImage({ { 9, 7, 1 } });
  auto       image2D = Image2DType::New();
  image2D->SetRegions(Image2DType::SizeType{ { 9, 7 } });
  image2D->SetSpacing(Image2DType::SpacingType{ { image->GetSpacing()[0], image->GetSpacing()[1] } });
  image2D->Allocate();
  itk::ImageRegionConstIterator<ImageType> it3D(image, image->GetLargestPossibleRegion());
  itk::ImageRegionIterator<Image2DType>    it2D(image2D, image2D->GetLargestPossibleRegion());
  for (; !it3D.IsAtEnd(); ++it3D, ++it2D)
  {
    it2D.Set(it3D.Get());
  }

  using Filter2DType = itk::SmoothingRecursiveGaussianImageFilter<Image2DType>;
  auto filter2D = Filter2DType::New();
  filter2D->SetInput(image2D);
  filter2D->SetSigmaArray(Filter2DType::SigmaArrayType{ { 0.8, 1.7 } });
  filter2D->Update();

  const auto output = Smooth(image, FilterType::SigmaArrayType{ { 0.8, 1.7, 2.5 } });
  EXPECT_EQ(output->GetOrigin(), image->GetOrigin());
  EXPECT_EQ(output->GetSpacing(), image->GetSpacing());
  EXPECT_EQ(output->GetDirection(), image->GetDirection());
  itk::ImageRegionConstIterator<ImageType>   outputIt(output, output->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<Image2DType> expectedIt(filter2D->GetOutput(), image2D->GetLargestPossibleRegion());
  for (; !expectedIt.IsAtEnd(); ++expectedIt, ++outputIt)
  {
    EXPECT_FLOAT_EQ(outputIt.Get(), expectedIt.Get());
  }
}

TEST(SmoothingRecursiveGaussianImageFilter, RepeatedUpdatesWithChangingSizes)
{
  const FilterType::SigmaArrayType sigma{ { 0.8, 1.7, 2.5 } };
  for (const bool inPlace : { false, true })
  {
    auto filter = FilterType::New();
    filter->SetSigmaArray(sigma);
    filter->SetInPlace(inPlace);
    for (const ImageType::SizeType size : { ImageType::SizeType{ { 9, 9, 9 } },
                                            ImageType::SizeType{ { 9, 9, 1 } },
                                            ImageType::SizeType{ { 1, 2, 3 } },
                                            ImageType::SizeType{ { 9, 1, 9 } },
                                            ImageType::SizeType{ { 2, 9, 9 } },
                                            ImageType::SizeType{ { 9, 9, 9 } } })
    {
      SCOPED_TRACE(::testing::Message() << "size=" << size << ", inPlace=" << inPlace);
      const auto expected = Smooth(MakeImage(size), sigma);
      filter->SetInput(MakeImage(size));
      ASSERT_NO_THROW(filter->Update());
      ExpectSameImage(filter->GetOutput(), expected);
    }
  }
}

TEST(SmoothingRecursiveGaussianImageFilter, AllShortAxesCopyInput)
{
  using DoubleImageType = itk::Image<double, 3>;
  auto image = DoubleImageType::New();
  image->SetRegions(DoubleImageType::SizeType{ { 1, 2, 3 } });
  image->Allocate();
  constexpr double value = 1.0 + std::numeric_limits<double>::epsilon();
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

TEST(SmoothingRecursiveGaussianImageFilter, VectorImageMatchesScalarImage)
{
  using VectorImageType = itk::VectorImage<float, 3>;
  for (const ImageType::SizeType size : { ImageType::SizeType{ { 9, 2, 1 } }, ImageType::SizeType{ { 1, 2, 3 } } })
  {
    SCOPED_TRACE(::testing::Message() << "size=" << size);
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
    const auto expected = Smooth(scalarImage, FilterType::SigmaArrayType::Filled(1.0));

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

TEST(SmoothingRecursiveGaussianImageFilter, IntegerPixelsMatchFloatPixels)
{
  using IntegerImageType = itk::Image<unsigned char, 3>;
  const FilterType::SigmaArrayType sigma{ { 0.8, 1.7, 2.5 } };
  const auto                       expected = Smooth(MakeImage({ { 9, 9, 1 } }), sigma);
  for (const bool inPlace : { false, true })
  {
    auto cast = itk::CastImageFilter<ImageType, IntegerImageType>::New();
    cast->SetInput(MakeImage({ { 9, 9, 1 } }));
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
