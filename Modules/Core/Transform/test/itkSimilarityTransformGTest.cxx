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
#include "itkSimilarity2DTransform.h"
#include "itkSimilarity3DTransform.h"

#include <gtest/gtest.h>


namespace
{

template <typename TTransform>
void
Test_SetCenterAndScale()
{
  using PointType = typename TTransform::InputPointType;

  const auto transform = TTransform::New();

  for (double center{ -1.0 }; center <= 1.0; ++center)
  {
    transform->SetCenter(itk::MakeFilled<PointType>(center));

    for (double scale{ 0.5 }; scale <= 2.0; scale *= 2.0)
    {
      transform->SetScale(scale);

      for (double input{ -1.0 }; input <= 1.0; ++input)
      {
        EXPECT_EQ(transform->TransformPoint(itk::MakeFilled<PointType>(input)),
                  itk::MakeFilled<PointType>(center + (scale * (input - center))));
      }
    }
  }
}

} // namespace


// Tests that `transform->TransformPoint` yields the expected result, when
// setting both the center and the scale (in that specific order), of the
// similarity transform. Note that in previous versions of ITK (including
// ITK 5.2.0), Similarity3DTransform did sometimes produce an incorrect result,
// as reported at https://github.com/InsightSoftwareConsortium/ITK/issues/2629
// "Similarity3DTransform::SetScale should recompute m_Offset"
TEST(SimilarityTransform, SetCenterAndScale)
{
  Test_SetCenterAndScale<itk::Similarity2DTransform<>>();
  Test_SetCenterAndScale<itk::Similarity3DTransform<>>();
}
