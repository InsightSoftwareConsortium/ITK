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

// First include the header file to be tested:
#include "itkImageBase.h"

#include "itkCovariantVector.h"
#include "itkImage.h"
#include "itkImageAdaptor.h"
#include "itkImageRegionIterator.h"
#include "itkIndexRange.h"
#include "itkVector.h"

#include <gtest/gtest.h>
#include <type_traits> // For is_same.


namespace
{
template <typename TPixelType>
class DummyPixelAccessor
{
public:
  using ExternalType = TPixelType;
  using InternalType = TPixelType;

  static void
  Set(TPixelType & output, const TPixelType & input)
  {
    output = input;
  }

  static TPixelType
  Get(const TPixelType & input)
  {
    return input;
  }
};


template <typename T1, typename T2>
void
Expect_same_type_and_equal_value(T1 && value1, T2 && value2)
{
  static_assert(std::is_same_v<T1, T2>, "Expect the same type!");
  EXPECT_EQ(value1, value2);
}


template <typename TImage, typename TAccessor, typename TCoordinate>
void
Expect_TransformPhysicalPoint_member_functions_return_the_same_for_ImageAdapter_as_for_image(
  const itk::ImageAdaptor<TImage, TAccessor> &            imageAdaptor,
  const TImage &                                          image,
  const itk::Point<TCoordinate, TImage::ImageDimension> & point)
{
  Expect_same_type_and_equal_value(imageAdaptor.TransformPhysicalPointToIndex(point),
                                   image.TransformPhysicalPointToIndex(point));
  Expect_same_type_and_equal_value(imageAdaptor.template TransformPhysicalPointToContinuousIndex<float>(point),
                                   image.template TransformPhysicalPointToContinuousIndex<float>(point));
  Expect_same_type_and_equal_value(imageAdaptor.template TransformPhysicalPointToContinuousIndex<double>(point),
                                   image.template TransformPhysicalPointToContinuousIndex<double>(point));
}


template <typename TImage, typename TAccessor, typename TIndexRep>
void
Expect_TransformContinuousIndexToPhysicalPoint_returns_the_same_for_ImageAdapter_as_for_image(
  const itk::ImageAdaptor<TImage, TAccessor> &                    imageAdaptor,
  const TImage &                                                  image,
  const itk::ContinuousIndex<TIndexRep, TImage::ImageDimension> & continuousIndex)
{
  Expect_same_type_and_equal_value(
    imageAdaptor.template TransformContinuousIndexToPhysicalPoint<float>(continuousIndex),
    image.template TransformContinuousIndexToPhysicalPoint<float>(continuousIndex));
  Expect_same_type_and_equal_value(
    imageAdaptor.template TransformContinuousIndexToPhysicalPoint<double>(continuousIndex),
    image.template TransformContinuousIndexToPhysicalPoint<double>(continuousIndex));
}


template <typename TImage, typename TAccessor>
void
Expect_TransformIndexToPhysicalPoint_returns_the_same_for_ImageAdapter_as_for_image(
  const itk::ImageAdaptor<TImage, TAccessor> & imageAdaptor,
  const TImage &                               image,
  const itk::Index<TImage::ImageDimension> &   index)
{
  Expect_same_type_and_equal_value(imageAdaptor.template TransformIndexToPhysicalPoint<float>(index),
                                   image.template TransformIndexToPhysicalPoint<float>(index));
  Expect_same_type_and_equal_value(imageAdaptor.template TransformIndexToPhysicalPoint<double>(index),
                                   image.template TransformIndexToPhysicalPoint<double>(index));
}


template <typename TImage, typename TAccessor, typename TVector>
void
Expect_TransformLocalVectorToPhysicalVector_returns_the_same_for_ImageAdapter_as_for_image(
  const itk::ImageAdaptor<TImage, TAccessor> & imageAdaptor,
  const TImage &                               image,
  const TVector &                              localVector)
{
  Expect_same_type_and_equal_value(imageAdaptor.TransformLocalVectorToPhysicalVector(localVector),
                                   image.TransformLocalVectorToPhysicalVector(localVector));
}


template <typename TImage>
void
Expect_Transform_member_functions_return_the_same_for_an_ImageAdapter_as_for_its_image(TImage & image)
{
  const auto ImageDimension = TImage::ImageDimension;
  using IndexType = itk::Index<ImageDimension>;

  const auto imageAdaptor = itk::ImageAdaptor<TImage, DummyPixelAccessor<typename TImage::PixelType>>::New();
  imageAdaptor->SetImage(&image);

  for (const auto & point :
       { itk::Point<double, ImageDimension>(), itk::MakeFilled<itk::Point<double, ImageDimension>>(1.0) })
  {
    Expect_TransformPhysicalPoint_member_functions_return_the_same_for_ImageAdapter_as_for_image(
      *imageAdaptor, image, point);
  }

  for (const auto & continuousIndex : { itk::ContinuousIndex<double, ImageDimension>(),
                                        itk::ContinuousIndex<double, ImageDimension>(IndexType::Filled(1)) })
  {
    Expect_TransformContinuousIndexToPhysicalPoint_returns_the_same_for_ImageAdapter_as_for_image(
      *imageAdaptor, image, continuousIndex);
  }

  for (const auto & index : { IndexType(), IndexType::Filled(1) })
  {
    Expect_TransformIndexToPhysicalPoint_returns_the_same_for_ImageAdapter_as_for_image(*imageAdaptor, image, index);
  }

  for (const auto & fixedArray :
       { itk::FixedArray<float, ImageDimension>(), itk::FixedArray<float, ImageDimension>::Filled(1.0f) })
  {
    Expect_TransformLocalVectorToPhysicalVector_returns_the_same_for_ImageAdapter_as_for_image(
      *imageAdaptor, image, fixedArray);
  }

  for (const auto & fixedArray :
       { itk::FixedArray<double, ImageDimension>(), itk::FixedArray<double, ImageDimension>::Filled(1.0) })
  {
    Expect_TransformLocalVectorToPhysicalVector_returns_the_same_for_ImageAdapter_as_for_image(
      *imageAdaptor, image, fixedArray);
  }

  Expect_TransformLocalVectorToPhysicalVector_returns_the_same_for_ImageAdapter_as_for_image(
    *imageAdaptor, image, itk::Vector<double, ImageDimension>());
  Expect_TransformLocalVectorToPhysicalVector_returns_the_same_for_ImageAdapter_as_for_image(
    *imageAdaptor, image, itk::CovariantVector<double, ImageDimension>());
}

} // end namespace


// Tests that any single parameter Transform member function of ImageAdapter return the same as the corresponding
// function call on its image.
TEST(ImageAdapter, TransformMemberFunctionsReturnSameAsForImage)
{
  // Test both 2D and 3D, for different pixel types, origins and spacings:
  const auto image2D = itk::Image<double>::New();

  image2D->SetSpacing(itk::MakeVector(2.0, 3.0));
  image2D->SetOrigin(itk::MakePoint(-1.0, 1.0));

  Expect_Transform_member_functions_return_the_same_for_an_ImageAdapter_as_for_its_image(*image2D);

  const auto image3D = itk::Image<unsigned char, 3>::New();
  Expect_Transform_member_functions_return_the_same_for_an_ImageAdapter_as_for_its_image(*image3D);
}


// Tests that ComputeOffset(index) returns the same value for an adaptor as for its internal image.
TEST(ImageAdaptor, ComputeOffset)
{
  constexpr unsigned int dimension{ 2 };
  using PixelType = int;
  using ImageType = itk::Image<PixelType, dimension>;

  const auto image = ImageType::New();
  const auto adaptor = itk::ImageAdaptor<ImageType, DummyPixelAccessor<PixelType>>::New();

  // Use a very small image region, to speed up the unit test.
  const itk::ImageRegion imageRegion{ itk::Index<dimension>::Filled(1), itk::Size<dimension>::Filled(2) };

  adaptor->SetImage(image);
  adaptor->SetRegions(imageRegion);
  adaptor->AllocateInitialized();

  for (const auto & index : itk::MakeIndexRange(imageRegion))
  {
    EXPECT_EQ(adaptor->ComputeOffset(index), image->ComputeOffset(index));
  }
}


// Tests that ImageAdaptor supports iteration by means of ImageRegionIterator.
TEST(ImageAdaptor, SupportsRegionIterator)
{
  constexpr unsigned int dimension{ 2 };
  using PixelType = int;
  using ImageType = itk::Image<PixelType, dimension>;

  const auto image = ImageType::New();
  const auto adaptor = itk::ImageAdaptor<ImageType, DummyPixelAccessor<PixelType>>::New();

  adaptor->SetImage(image);
  adaptor->SetRegions(itk::Size<dimension>::Filled(4));
  adaptor->AllocateInitialized();

  constexpr auto         regionIndex = itk::Index<dimension>::Filled(1);
  constexpr auto         regionSize = itk::Size<dimension>::Filled(2);
  const itk::ImageRegion region{ regionIndex, regionSize };

  // Set the pixel values by means of the adaptor, for the specified region.
  PixelType           pixelValue{};
  constexpr PixelType maxPixelValue{ regionSize.CalculateProductOfElements() };

  for (itk::ImageRegionIterator iterator(adaptor, region); !iterator.IsAtEnd(); ++iterator)
  {
    ++pixelValue;
    ASSERT_LE(pixelValue, maxPixelValue);
    iterator.Set(pixelValue);
    EXPECT_EQ(iterator.Get(), pixelValue);
  }

  // Now check if the intenal image has got the expected pixel values, in the specified region.
  PixelType expectedPixelValue{};

  for (itk::ImageRegionConstIterator iterator(image, region); !iterator.IsAtEnd(); ++iterator)
  {
    ++expectedPixelValue;
    EXPECT_EQ(iterator.Get(), expectedPixelValue);
  }
}
