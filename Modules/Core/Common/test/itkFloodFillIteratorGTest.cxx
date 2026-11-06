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

#include "itkImageRegionIterator.h"
#include "itkSphereSpatialFunction.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkShapedFloodFilledImageFunctionConditionalIterator.h"
#include "itkGTest.h"

#include <iostream>
#include <type_traits>

namespace
{
// HasSet tells whether `Set(pixel)` can be called on the specified iterator type.
template <typename TIterator, typename = void>
struct HasSet : std::false_type
{};

template <typename TIterator>
struct HasSet<TIterator,
              std::void_t<decltype(std::declval<TIterator &>().Set(std::declval<typename TIterator::PixelType>()))>>
  : std::true_type
{};
} // namespace


TEST(FloodFillIterator, SphereFloodFill)
{
  constexpr unsigned int dim{ 3 };

  using TImageType = itk::Image<int, dim>;

  TImageType::SizeValueType    sourceImageSize[] = { 20, 20, 20 };
  TImageType::SpacingValueType sourceImageSpacing[] = { 1.0, 1.0, 1.0 };
  TImageType::PointValueType   sourceImageOrigin[] = { 0, 0, 0 };

  auto sourceImage = TImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  TImageType::SizeType sourceImageSizeObject;
  sourceImageSizeObject.SetSize(sourceImageSize);
  TImageType::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize(sourceImageSizeObject);
  sourceImage->SetRegions(largestPossibleRegion);
  sourceImage->AllocateInitialized();

  std::cout << "New sourceImage created and allocated" << std::endl;

  using TFunctionType = itk::SphereSpatialFunction<dim>;
  using TFunctionPositionType = TFunctionType::InputType;

  auto spatialFunc = TFunctionType::New();
  spatialFunc->SetRadius(5);

  TFunctionPositionType center;
  center[0] = 10;
  center[1] = 10;
  center[2] = 10;
  spatialFunc->SetCenter(center);

  TImageType::IndexType                seedPos;
  constexpr TImageType::IndexValueType pos[]{ 10, 10, 10 };
  seedPos.SetIndex(pos);

  using TItType = itk::FloodFilledSpatialFunctionConditionalIterator<TImageType, TFunctionType>;
  TItType sfi(sourceImage, spatialFunc, seedPos);

  for (; !(sfi.IsAtEnd()); ++sfi)
  {

    std::cout << sfi.GetIndex() << ": " << sfi.Get() << std::endl;
    sfi.Set(255);
  }

  std::cout << "Spatial function iterator created, sphere drawn" << std::endl;
}


// Checks that only the writable flood-filled iterators provide Set().
TEST(FloodFillIterator, OnlyWritableIteratorsSet)
{
  using ImageType = itk::Image<int, 2>;
  using SpatialFunctionType = itk::SphereSpatialFunction<2>;
  using ImageFunctionType = itk::BinaryThresholdImageFunction<ImageType>;
  static_assert(
    HasSet<itk::FloodFilledSpatialFunctionConditionalIterator<ImageType, SpatialFunctionType>>::value &&
    !HasSet<itk::FloodFilledSpatialFunctionConditionalConstIterator<ImageType, SpatialFunctionType>>::value);
  static_assert(HasSet<itk::FloodFilledImageFunctionConditionalIterator<ImageType, ImageFunctionType>>::value &&
                !HasSet<itk::FloodFilledImageFunctionConditionalConstIterator<ImageType, ImageFunctionType>>::value);
  static_assert(
    HasSet<itk::ShapedFloodFilledImageFunctionConditionalIterator<ImageType, ImageFunctionType>>::value &&
    !HasSet<itk::ShapedFloodFilledImageFunctionConditionalConstIterator<ImageType, ImageFunctionType>>::value);
}
