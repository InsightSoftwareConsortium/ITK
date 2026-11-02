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

#include "itkTimeVaryingVelocityFieldTransform.h"
#include "itkGTest.h"

namespace
{
using TransformType = itk::TimeVaryingVelocityFieldTransform<double, 2>;
using FieldType = TransformType::DisplacementFieldType;

FieldType::Pointer
MakeConstantField(double x, double y)
{
  auto field = FieldType::New();
  field->SetRegions(FieldType::SizeType{ { 8, 8 } });
  field->Allocate();
  FieldType::PixelType displacement;
  displacement[0] = x;
  displacement[1] = y;
  field->FillBuffer(displacement);
  return field;
}
} // namespace

TEST(VelocityFieldTransform, SetDisplacementFieldUpdatesTheInterpolator)
{
  auto transform = TransformType::New();
  transform->SetDisplacementField(MakeConstantField(1.0, 2.0));
  ASSERT_EQ(transform->GetInterpolator()->GetInputImage(), transform->GetDisplacementField());

  const TransformType::InputPointType point{ { 3.0, 4.0 } };
  const auto                          transformed = transform->TransformPoint(point);
  EXPECT_EQ(transformed[0], 4.0);
  EXPECT_EQ(transformed[1], 6.0);
}

TEST(VelocityFieldTransform, ReplacingTheDisplacementFieldUpdatesTheInterpolator)
{
  auto       transform = TransformType::New();
  const auto first = MakeConstantField(1.0, 2.0);
  transform->SetDisplacementField(first);
  transform->GetModifiableInterpolator()->SetInputImage(first);

  transform->SetDisplacementField(MakeConstantField(-1.0, 0.5));
  const TransformType::InputPointType point{ { 3.0, 4.0 } };
  const auto                          transformed = transform->TransformPoint(point);
  EXPECT_EQ(transformed[0], 2.0);
  EXPECT_EQ(transformed[1], 4.5);
}
