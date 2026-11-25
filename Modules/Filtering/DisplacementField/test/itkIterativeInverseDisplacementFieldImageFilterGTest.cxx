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

#include "itkIterativeInverseDisplacementFieldImageFilter.h"
#include "itkMath.h"
#include "itkGTest.h"

namespace
{
using VectorType = itk::Vector<float, 2>;
using FieldType = itk::Image<VectorType, 2>;
using FilterType = itk::IterativeInverseDisplacementFieldImageFilter<FieldType, FieldType>;

// Zero field except pixel (20,20), whose first inverse guess maps outside the buffer.
FieldType::Pointer
MakeForwardField()
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ FieldType::IndexType{}, FieldType::SizeType::Filled(24) });
  field->Allocate();
  field->FillBuffer(VectorType{});
  field->SetPixel({ { 20, 20 } }, itk::MakeVector(0.0F, 2.0F));
  field->SetPixel({ { 20, 18 } }, itk::MakeVector(0.0F, -4.0F));
  return field;
}

VectorType
InverseAt(const FieldType::IndexType & index, const unsigned int iterations)
{
  auto filter = FilterType::New();
  filter->SetInput(MakeForwardField());
  filter->SetNumberOfIterations(iterations);
  filter->SetStopValue(0.0);
  filter->UpdateLargestPossibleRegion();
  return filter->GetOutput()->GetPixel(index);
}
} // namespace

// Refinement must not compare probes against the previous pixel's residual (issue #6575, B31).
TEST(IterativeInverseDisplacementFieldImageFilter, RefinesPixelWhoseFirstGuessMapsOutsideBuffer)
{
  const FieldType::IndexType index{ { 20, 20 } };

  const VectorType guess = InverseAt(index, 0);
  ASSERT_NEAR(guess[0], 0.0, 1e-4);
  ASSERT_NEAR(guess[1], 4.0, 1e-4);

  const VectorType refined = InverseAt(index, 5);
  EXPECT_GT((refined - guess).GetNorm(), 0.5);
}

TEST(IterativeInverseDisplacementFieldImageFilter, ZeroFieldPixelStaysNearZero)
{
  const VectorType inverse = InverseAt({ { 4, 4 } }, 5);
  for (unsigned int j = 0; j < 2; ++j)
  {
    EXPECT_TRUE(itk::Math::isfinite(static_cast<double>(inverse[j])));
  }
  EXPECT_LE(inverse.GetNorm(), 0.5);
}
