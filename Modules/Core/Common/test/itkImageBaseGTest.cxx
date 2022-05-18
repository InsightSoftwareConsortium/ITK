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
#include "itkVector.h"

#include <gtest/gtest.h>
#include <type_traits> // For is_same.


namespace
{
template <typename T1, typename T2>
void
Expect_same_type_and_equal_value(T1 && value1, T2 && value2)
{
  static_assert(std::is_same<T1, T2>::value, "Expect the same type!");
  EXPECT_EQ(value1, value2);
}


template <typename TImage>
void
Expect_by_default_Transform_result_equals_default_constructed_value(const typename TImage::SizeType & imageSize)
{
  const auto ImageDimension = TImage::ImageDimension;
  const auto image = TImage::New();
  image->SetRegions(imageSize);
  image->Allocate();

  using ImageBaseType = itk::ImageBase<ImageDimension>;
  using IndexType = typename ImageBaseType::IndexType;

  using PointType = itk::Point<double, ImageDimension>;
  using ContinuousIndexType = itk::ContinuousIndex<double, ImageDimension>;

  using FloatArrayType = itk::FixedArray<float, ImageDimension>;
  using DoubleArrayType = itk::FixedArray<double, ImageDimension>;
  using VectorType = itk::Vector<double, ImageDimension>;
  using CovariantVectorType = itk::CovariantVector<double, ImageDimension>;

  const ImageBaseType & imageBase = *image;

  Expect_same_type_and_equal_value(imageBase.TransformPhysicalPointToIndex(PointType()), IndexType());

  // For the three member functions TransformPhysicalPointToContinuousIndex,
  // TransformContinuousIndexToPhysicalPoint, and TransformIndexToPhysicalPoint,
  // the first template argument of the return type is expected to
  // correspond with the first template argument of the member function.
  Expect_same_type_and_equal_value(imageBase.template TransformPhysicalPointToContinuousIndex<float>(PointType()),
                                   itk::ContinuousIndex<float, ImageDimension>());
  Expect_same_type_and_equal_value(imageBase.template TransformPhysicalPointToContinuousIndex<double>(PointType()),
                                   itk::ContinuousIndex<double, ImageDimension>());
  Expect_same_type_and_equal_value(
    imageBase.template TransformContinuousIndexToPhysicalPoint<float>(ContinuousIndexType()),
    itk::Point<float, ImageDimension>());
  Expect_same_type_and_equal_value(
    imageBase.template TransformContinuousIndexToPhysicalPoint<double>(ContinuousIndexType()),
    itk::Point<double, ImageDimension>());
  Expect_same_type_and_equal_value(imageBase.template TransformIndexToPhysicalPoint<float>(IndexType()),
                                   itk::Point<float, ImageDimension>());
  Expect_same_type_and_equal_value(imageBase.template TransformIndexToPhysicalPoint<double>(IndexType()),
                                   itk::Point<double, ImageDimension>());

  // The two member functions TransformLocalVectorToPhysicalVector and
  // TransformPhysicalVectorToLocalVector are expected to return an
  // array or vector of the same type as their first function argument.
  Expect_same_type_and_equal_value(imageBase.TransformLocalVectorToPhysicalVector(FloatArrayType()), FloatArrayType());
  Expect_same_type_and_equal_value(imageBase.TransformLocalVectorToPhysicalVector(DoubleArrayType()),
                                   DoubleArrayType());
  Expect_same_type_and_equal_value(imageBase.TransformLocalVectorToPhysicalVector(VectorType()), VectorType());
  Expect_same_type_and_equal_value(imageBase.TransformLocalVectorToPhysicalVector(CovariantVectorType()),
                                   CovariantVectorType());
  Expect_same_type_and_equal_value(imageBase.TransformPhysicalVectorToLocalVector(FloatArrayType()), FloatArrayType());
  Expect_same_type_and_equal_value(imageBase.TransformPhysicalVectorToLocalVector(DoubleArrayType()),
                                   DoubleArrayType());
  Expect_same_type_and_equal_value(imageBase.TransformPhysicalVectorToLocalVector(VectorType()), VectorType());
  Expect_same_type_and_equal_value(imageBase.TransformPhysicalVectorToLocalVector(CovariantVectorType()),
                                   CovariantVectorType());
}


template <unsigned int VImageDimension>
void
Check_New_ImageBase()
{
  const auto imageBase = itk::ImageBase<VImageDimension>::New();
  ASSERT_NE(imageBase, nullptr);

  for (const auto spacingValue : imageBase->GetSpacing())
  {
    EXPECT_FLOAT_EQ(spacingValue, 1.0);
  }
  for (const auto originValue : imageBase->GetOrigin())
  {
    EXPECT_FLOAT_EQ(originValue, 0.0);
  }

  EXPECT_TRUE(imageBase->GetDirection().GetVnlMatrix().is_identity());
  EXPECT_TRUE(imageBase->GetInverseDirection().GetVnlMatrix().is_identity());
}

template <unsigned int VImageDimension>
void
CheckInvalidSpacingExceptions()
{
  using SpacingType = typename itk::ImageBase<VImageDimension>::SpacingType;
  using DirectionType = typename itk::ImageBase<VImageDimension>::DirectionType;

  const auto imageBase = itk::ImageBase<VImageDimension>::New();

  // Test exceptions
  const SpacingType initialSpacing = imageBase->GetSpacing();
  const auto        negativeSpacing = itk::MakeFilled<SpacingType>(-1.0);
  const SpacingType zeroSpacing{};

#if !defined(ITK_LEGACY_REMOVE)
  // Only a warning is displayed
  imageBase->SetSpacing(negativeSpacing);
  EXPECT_EQ(imageBase->GetSpacing(), negativeSpacing);

  // Set the spacing value back to its default value
  imageBase->SetSpacing(initialSpacing);
#else
  EXPECT_THROW(imageBase->SetSpacing(negativeSpacing), itk::ExceptionObject);
  EXPECT_EQ(imageBase->GetSpacing(), initialSpacing);
#endif

  EXPECT_THROW(imageBase->SetSpacing(zeroSpacing), itk::ExceptionObject);
  EXPECT_EQ(imageBase->GetSpacing(), initialSpacing);

  const DirectionType initialDirection = imageBase->GetDirection();
  const DirectionType zeroDirection{};

  EXPECT_THROW(imageBase->SetDirection(zeroDirection), itk::ExceptionObject);

  // The direction should be kept unmodified after the attempt
  EXPECT_EQ(imageBase->GetDirection(), initialDirection);
}

} // end namespace


// Tests that "by default" (when the image has a default origin and a default
// direction matrix, and the function argument is just default-constructed), the
// return value of a single-parameter Transform member function is equal to a
// default-constructed value. This is tested for all six single-parameter
// Transform member functions of ImageBase.
TEST(ImageBase, ByDefaultTransformResultEqualsDefaultConstructedValue)
{
  // Test both 2D and 3D, for different pixel types and sizes:
  Expect_by_default_Transform_result_equals_default_constructed_value<itk::Image<double>>({ { 2, 2 } });
  Expect_by_default_Transform_result_equals_default_constructed_value<itk::Image<unsigned char, 3>>({ { 2, 3, 4 } });
}

TEST(ImageBase, New)
{
  Check_New_ImageBase<2>();
  Check_New_ImageBase<3>();
}

TEST(ImageBase, InvalidSpacingExceptions)
{
  CheckInvalidSpacingExceptions<2>();
  CheckInvalidSpacingExceptions<3>();
}
