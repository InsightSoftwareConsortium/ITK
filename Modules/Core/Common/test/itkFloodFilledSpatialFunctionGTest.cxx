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

/*
 * Tests the various inclusion strategies available
 * to itkFloodFilledSpatialFunctionConditionalIterator.
 */

#include "itkImageRegionIterator.h"
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkGTest.h"


TEST(FloodFilledSpatialFunction, InclusionStrategies)
{
  constexpr unsigned int dim{ 2 };

  using ImageType = itk::Image<bool, dim>;

  ImageType::SizeValueType    sourceImageSize[] = { 5, 5 };
  ImageType::SpacingValueType sourceImageSpacing[] = { 1.0, 1.0 };
  ImageType::PointValueType   sourceImageOrigin[] = { 0, 0 };

  auto sourceImage = ImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  ImageType::SizeType sourceImageSizeObject;
  sourceImageSizeObject.SetSize(sourceImageSize);
  ImageType::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize(sourceImageSizeObject);
  sourceImage->SetRegions(largestPossibleRegion);
  sourceImage->Allocate();

  for (int strat = 0; strat < 4; ++strat)
  {
    // Initialize the image to all false
    itk::ImageRegionIterator<ImageType> it(sourceImage, largestPossibleRegion);
    for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
      it.Set(false);
    }

    using FunctionType = itk::SphereSpatialFunction<dim>;
    using FunctionPositionType = FunctionType::InputType;

    auto spatialFunc = FunctionType::New();
    spatialFunc->SetRadius(1.0);

    FunctionPositionType center;
    center[0] = 2.5;
    center[1] = 2.5;
    spatialFunc->SetCenter(center);

    ImageType::IndexType                seedPos;
    constexpr ImageType::IndexValueType pos[]{ 2, 2 };
    seedPos.SetIndex(pos);

    using ItType = itk::FloodFilledSpatialFunctionConditionalIterator<ImageType, FunctionType>;
    ItType sfi(sourceImage, spatialFunc, seedPos);

    switch (strat)
    {
      case 0:
        sfi.SetOriginInclusionStrategy();
        break;
      case 1:
        sfi.SetCenterInclusionStrategy();
        break;
      case 2:
        sfi.SetCompleteInclusionStrategy();
        break;
      case 3:
        sfi.SetIntersectInclusionStrategy();
        break;
    }

    unsigned int setCount{ 0 };
    for (sfi.GoToBegin(); !(sfi.IsAtEnd()); ++sfi)
    {
      sfi.Set(true);
      ++setCount;
    }

    EXPECT_GT(setCount, 0u) << "Strategy " << strat << " set no pixels";
  }
}
