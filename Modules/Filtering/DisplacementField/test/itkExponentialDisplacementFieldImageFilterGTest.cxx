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

#include "itkExponentialDisplacementFieldImageFilter.h"
#include "itkGTest.h"

namespace
{
using VectorType = itk::Vector<float, 2>;
using FieldType = itk::Image<VectorType, 2>;
using FilterType = itk::ExponentialDisplacementFieldImageFilter<FieldType, FieldType>;

FieldType::Pointer
MakeField(const VectorType & value)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ FieldType::IndexType{}, FieldType::SizeType::Filled(16) });
  field->Allocate();
  field->FillBuffer(value);
  return field;
}
} // namespace

// The first-order scale factor is 2^numiter; computing it with an integer shift is
// undefined behavior for 31 or more iterations (issue #6575, B35).
TEST(ExponentialDisplacementFieldImageFilter, ThirtyOneIterationsScaleCorrectly)
{
  auto filter = FilterType::New();
  filter->SetInput(MakeField(itk::MakeVector(0.5F, 0.25F)));
  filter->AutomaticNumberOfIterationsOff();
  filter->SetMaximumNumberOfIterations(31);
  filter->UpdateLargestPossibleRegion();

  // The exponential of a constant field is the same constant translation.
  const auto pixel = filter->GetOutput()->GetPixel(itk::MakeIndex(8, 8));
  EXPECT_NEAR(pixel[0], 0.5, 1e-3);
  EXPECT_NEAR(pixel[1], 0.25, 1e-3);
}

// A zero field must select zero squaring iterations and stay exactly zero
// (issue #6575, Q31).
TEST(ExponentialDisplacementFieldImageFilter, ZeroFieldStaysZero)
{
  auto filter = FilterType::New();
  filter->SetInput(MakeField(itk::MakeVector(0.0F, 0.0F)));
  filter->AutomaticNumberOfIterationsOn();
  filter->UpdateLargestPossibleRegion();

  const auto pixel = filter->GetOutput()->GetPixel(itk::MakeIndex(8, 8));
  EXPECT_EQ(pixel[0], 0.0F);
  EXPECT_EQ(pixel[1], 0.0F);
}
