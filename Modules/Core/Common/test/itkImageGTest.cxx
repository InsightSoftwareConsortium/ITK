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
#include "itkImage.h"
#include <gtest/gtest.h>

namespace
{
template <typename T>
void
Expect_equal(const T & object1, const T & object2)
{
  // Test that equal objects can be used as arguments to GoogleTest EXPECT_EQ.
  EXPECT_EQ(object1, object2);
  EXPECT_EQ(object2, object1);

  // Test symmetry, as well as consistency between equal and unequal.
  EXPECT_TRUE(object1 == object2);
  EXPECT_TRUE(object2 == object1);
  EXPECT_FALSE(object1 != object2);
  EXPECT_FALSE(object2 != object1);
}


template <typename T>
void
Expect_unequal(const T & object1, const T & object2)
{
  // Test that unequal objects can be used as arguments to GoogleTest EXPECT_NE.
  EXPECT_NE(object1, object2);
  EXPECT_NE(object2, object1);

  // Test symmetry, as well as consistency between equal and unequal.
  EXPECT_TRUE(object1 != object2);
  EXPECT_TRUE(object2 != object1);
  EXPECT_FALSE(object1 == object2);
  EXPECT_FALSE(object2 == object1);
}


template <typename T>
void
Expect_equal_to_itself(const T & object)
{
  Expect_equal(object, object);
}


template <typename T>
void
Expect_new_objects_equal()
{
  Expect_equal(*T::New(), *T::New());
}


template <typename T>
void
Expect_new_object_equal_to_itself()
{
  Expect_equal_to_itself(*T::New());
}


template <typename TImage>
void
Expect_allocated_initialized_image_equal_to_itself()
{
  using SizeType = typename TImage::SizeType;

  const auto image = TImage::New();
  image->SetRegions(SizeType::Filled(2));

  // Allocate and initialize the image:
  image->Allocate(true);

  Expect_equal_to_itself(*image);
}


template <typename TImage>
void
Expect_unequal_when_sizes_differ()
{
  using SizeType = typename TImage::SizeType;

  const auto image1 = TImage::New();
  image1->SetRegions(SizeType::Filled(2));
  image1->Allocate(true);

  const auto image2 = TImage::New();
  image2->SetRegions(SizeType::Filled(3));
  image2->Allocate(true);

  Expect_unequal(*image1, *image2);
}


template <typename TImage>
void
Expect_unequal_when_pixel_values_differ()
{
  using SizeType = typename TImage::SizeType;

  const auto imageSize = SizeType::Filled(2);

  const auto image1 = TImage::New();
  image1->SetRegions(imageSize);
  image1->Allocate();
  image1->FillBuffer(1);

  const auto image2 = TImage::New();
  image2->SetRegions(imageSize);
  image2->Allocate();
  image2->FillBuffer(2);

  Expect_unequal(*image1, *image2);
}


// An example of a type that does not support `x == y`, for instances `x`, `y`.
// (Neither does it support `x != y`).
struct NonEqualityComparableType
{
  int data;
};

} // namespace


// Test template instantiations for int, and for a non-EqualityComparable
// pixel type.
template class itk::Image<int>;
template class itk::Image<NonEqualityComparableType>;

// Tests that for any ImageType, objects constructed by ImageType::New()
// compare equal, using operator==(const Image &, const Image &).
TEST(Image, NewObjectsEqual)
{
  Expect_new_objects_equal<itk::Image<int>>();
  Expect_new_objects_equal<itk::Image<double, 3>>();
}


// Tests that an image compares equal to itself,
// using operator==(const Image &, const Image &).
TEST(Image, EqualToItself)
{
  // Tests an object when it is newly created:
  Expect_new_object_equal_to_itself<itk::Image<int>>();
  Expect_new_object_equal_to_itself<itk::Image<double, 3>>();

  // Tests an object that is allocated and initialized.
  Expect_allocated_initialized_image_equal_to_itself<itk::Image<int>>();
  Expect_allocated_initialized_image_equal_to_itself<itk::Image<double, 3>>();
}


// Tests that two image compare unequal when their sizes differ,
// using operator!=(const Image &, const Image &).
TEST(Image, UnequalWhenSizesDiffer)
{
  Expect_unequal_when_sizes_differ<itk::Image<int>>();
  Expect_unequal_when_sizes_differ<itk::Image<double, 3>>();
}


// Tests that two image compare unequal when their pixel values differ,
// using operator!=(const Image &, const Image &).
TEST(Image, UnequalWhenPixelValuesDiffer)
{
  Expect_unequal_when_pixel_values_differ<itk::Image<int>>();
  Expect_unequal_when_pixel_values_differ<itk::Image<double, 3>>();
}


// Tests `FillBuffer` for pixels of type `NonEqualityComparableType`. Aims to
// suppress Linux (Ubuntu 7.5.0-3ubuntu1~18.04) GCC GNU 7.5.0 warning:
// 'FillBuffer(const TPixel&)' defined but not used [-Wunused-function]
TEST(Image, FillBufferOfNonEqualityComparableType)
{
  const auto ImageDimagion = 2U;
  const auto image = itk::Image<NonEqualityComparableType, ImageDimagion>::New();
  image->SetRegions(itk::Size<ImageDimagion>::Filled(1));
  image->Allocate();

  for (const auto i : { 0, 1 })
  {
    image->FillBuffer({ i });
    EXPECT_EQ(image->GetPixel({}).data, i);
  }
}
