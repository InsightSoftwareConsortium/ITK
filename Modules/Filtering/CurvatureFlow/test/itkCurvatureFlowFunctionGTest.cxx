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

#include "itkCurvatureFlowFunction.h"
#include "itkImage.h"
#include "itkMath.h"
#include "itkGTest.h"

namespace
{
using ImageType = itk::Image<double, 3>;
using FunctionType = itk::CurvatureFlowFunction<ImageType>;

FunctionType::PixelType
ComputeUpdateAtRadius(FunctionType * function, const ImageType * image, unsigned int radiusValue)
{
  FunctionType::RadiusType radius;
  radius.Fill(radiusValue);
  function->SetRadius(radius);

  FunctionType::NeighborhoodType it(radius, image, image->GetBufferedRegion());
  it.SetLocation(ImageType::IndexType{ { 7, 5, 6 } });

  return function->ComputeUpdate(it, nullptr);
}
} // namespace

TEST(CurvatureFlowFunction, ComputeUpdateIsIndependentOfRadius)
{
  auto                  image = ImageType::New();
  ImageType::RegionType region;
  region.SetSize(ImageType::SizeType::Filled(13));
  image->SetRegions(region);
  image->Allocate();

  ImageType::IndexType index;
  for (index[0] = 0; index[0] < 13; ++index[0])
  {
    for (index[1] = 0; index[1] < 13; ++index[1])
    {
      for (index[2] = 0; index[2] < 13; ++index[2])
      {
        const double x = index[0];
        const double y = index[1];
        const double z = index[2];
        const double value = 0.5 * (itk::Math::sqr(x - 5.0) + itk::Math::sqr(y - 5.0) + itk::Math::sqr(z - 5.0));
        image->SetPixel(index, value);
      }
    }
  }

  auto function = FunctionType::New();

  const FunctionType::PixelType resultRadius1 = ComputeUpdateAtRadius(function, image, 1);
  const FunctionType::PixelType resultRadius2 = ComputeUpdateAtRadius(function, image, 2);

  EXPECT_DOUBLE_EQ(resultRadius1, 2.0);
  EXPECT_DOUBLE_EQ(resultRadius2, 2.0);
}
