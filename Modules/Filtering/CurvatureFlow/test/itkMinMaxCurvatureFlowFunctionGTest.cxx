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

#include "itkMinMaxCurvatureFlowFunction.h"
#include "itkImage.h"
#include "itkMath.h"
#include "itkGTest.h"

namespace
{
using ImageType = itk::Image<double, 3>;
using FunctionType = itk::MinMaxCurvatureFlowFunction<ImageType>;
} // namespace

TEST(MinMaxCurvatureFlowFunction, ComputeUpdateUsesCorrectPolarAngleAtRadiusTwo)
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
        const double value = 0.5 * (itk::Math::sqr(x - 5.0) + itk::Math::sqr(y - 5.0) + itk::Math::sqr(z - 5.0)) +
                             5.0 * itk::Math::sqr(x - 7.0);
        image->SetPixel(index, value);
      }
    }
  }

  auto function = FunctionType::New();
  function->SetStencilRadius(2);

  FunctionType::NeighborhoodType::RadiusType radius;
  radius.Fill(2);
  FunctionType::NeighborhoodType it(radius, image, image->GetBufferedRegion());
  it.SetLocation(ImageType::IndexType{ { 7, 5, 6 } });

  const FunctionType::PixelType result = function->ComputeUpdate(it, nullptr);

  EXPECT_DOUBLE_EQ(result, 0.0);
}
