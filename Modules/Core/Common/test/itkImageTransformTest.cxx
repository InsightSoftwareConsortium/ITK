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

#include "itkImage.h"

#include <iostream>
#include <fstream>

template <unsigned int TDimension>
void
TestTransform()
{
  using ImageType = itk::Image<float, TDimension>;

  auto image = ImageType::New();
  auto orientedImage = ImageType::New();

  typename ImageType::PointType origin;

  for (unsigned int i = 0; i < TDimension; ++i)
  {
    origin[i] = static_cast<double>(i * 100);
  }
  image->SetOrigin(origin);
  orientedImage->SetOrigin(origin);

  using RegionType = itk::ImageRegion<TDimension>;
  using IndexType = typename RegionType::IndexType;
  using SizeType = typename RegionType::SizeType;

  typename ImageType::PointType point;
  RegionType                    region;

  auto size = SizeType::Filled(10);
  region.SetSize(size);

  auto index = IndexType::Filled(5);

  std::cout << "TransformIndexToPhysicalPoint..." << '\n';
  orientedImage->TransformIndexToPhysicalPoint(index, point);
  std::cout << "    Image: " << index << " -> " << point << '\n';

  image->TransformIndexToPhysicalPoint(index, point);
  std::cout << "    Image:         " << index << " -> " << point << '\n';

  std::cout << "TransformPhysicalPointToIndex..." << '\n';
  const bool isInsideOrientedImage = orientedImage->TransformPhysicalPointToIndex(point, index);
  std::cout << "    Image: " << point << " -> " << index << (isInsideOrientedImage ? " inside" : " outside") << '\n';

  const bool isInsideImage = image->TransformPhysicalPointToIndex(point, index);
  std::cout << "    Image:         " << point << " -> " << index << (isInsideImage ? " inside" : " outside") << '\n';
}

int
itkImageTransformTest(int, char *[])
{
  TestTransform<8>();
  TestTransform<3>();
  TestTransform<2>();
  TestTransform<1>();

  return EXIT_SUCCESS;
}
