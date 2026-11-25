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

#include "itkWarpImageFilter.h"
#include "itkGTest.h"

// An output grid sharing origin/spacing/direction with a smaller displacement field must
// take the general (clamped) path instead of iterating the field over the larger output
// region, which throws an internal iterator error (issue #6575, B34).
TEST(WarpImageFilter, LargerOutputThanFieldDoesNotThrow)
{
  using ImageType = itk::Image<float, 2>;
  using VectorType = itk::Vector<float, 2>;
  using FieldType = itk::Image<VectorType, 2>;

  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(16) });
  image->Allocate();
  image->FillBuffer(5.0F);

  auto field = FieldType::New();
  field->SetRegions(FieldType::RegionType{ FieldType::IndexType{}, FieldType::SizeType::Filled(16) });
  field->Allocate();
  field->FillBuffer(itk::MakeVector(0.0F, 0.0F));

  using FilterType = itk::WarpImageFilter<ImageType, ImageType, FieldType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->SetDisplacementField(field);
  filter->SetOutputParametersFromImage(field.GetPointer());
  filter->SetOutputSize(ImageType::SizeType::Filled(20));
  ASSERT_NO_THROW(filter->UpdateLargestPossibleRegion());

  const ImageType * output = filter->GetOutput();
  EXPECT_EQ(output->GetBufferedRegion().GetSize(), ImageType::SizeType::Filled(20));
  // Inside the field and input domains the identity warp reproduces the input.
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(4, 4)), 5.0F);
}
