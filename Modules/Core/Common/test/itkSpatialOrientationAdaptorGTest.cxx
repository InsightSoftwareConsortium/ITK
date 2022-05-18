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


#include "itkGTest.h"
#include "itkImage.h"
#include "itkSpatialOrientationAdapter.h"

TEST(SpatialOrientationAdaptor, test1)
{
  using ImageType = itk::Image<float, 3>;
  using DirectionType = typename ImageType::DirectionType;

  itk::SpatialOrientationAdapter adapter;

  DirectionType d1;
  d1.SetIdentity();

  EXPECT_EQ(itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI,
            adapter.FromDirectionCosines(d1));

  const double  data[] = { 0.5986634407395047, 0.22716302314740483, -0.768113953548866,
                          0.5627936241740271, 0.563067040943212,   0.6051601804419384,
                          0.5699696670095713, -0.794576911518317,  0.20924175102261847 };
  DirectionType d2{ DirectionType::InternalMatrixType{ data } };
  EXPECT_EQ(itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASL,
            adapter.FromDirectionCosines(d2));
}
