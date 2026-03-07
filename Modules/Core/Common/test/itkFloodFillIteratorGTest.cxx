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
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkGTest.h"

#include <iostream>


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

  unsigned int pixelCount{ 0 };
  for (; !(sfi.IsAtEnd()); ++sfi)
  {
    std::cout << sfi.GetIndex() << ": " << sfi.Get() << std::endl;
    sfi.Set(255);
    ++pixelCount;
  }

  std::cout << "Sphere drawn with " << pixelCount << " pixels" << std::endl;
  EXPECT_GT(pixelCount, 0u);
}
