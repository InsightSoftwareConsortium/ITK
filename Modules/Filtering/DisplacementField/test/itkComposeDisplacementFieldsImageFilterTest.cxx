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

#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkImageRegionIterator.h"

int
itkComposeDisplacementFieldsImageFilterTest(int, char *[])
{
  constexpr unsigned int ImageDimension = 2;

  using VectorType = itk::Vector<float, ImageDimension>;
  using DisplacementFieldType = itk::Image<VectorType, ImageDimension>;

  // Create a displacement field
  DisplacementFieldType::PointType     origin;
  DisplacementFieldType::SpacingType   spacing;
  DisplacementFieldType::SizeType      size;
  DisplacementFieldType::DirectionType direction;

  direction.SetIdentity();
  origin.Fill(0.0);
  spacing.Fill(0.5);
  size.Fill(100);

  auto ones = itk::MakeFilled<VectorType>(1);

  auto field = DisplacementFieldType::New();
  field->SetOrigin(origin);
  field->SetSpacing(spacing);
  field->SetRegions(size);
  field->SetDirection(direction);
  field->Allocate();
  field->FillBuffer(ones);

  using ComposerType = itk::ComposeDisplacementFieldsImageFilter<DisplacementFieldType>;
  auto composer = ComposerType::New();
  composer->SetDisplacementField(field);
  composer->SetWarpingField(field);
  composer->Update();

  std::cout << "displacement field: " << composer->GetDisplacementField() << '\n';
  std::cout << "warping field: " << composer->GetWarpingField() << '\n';
  std::cout << "interpolator: " << composer->GetInterpolator() << '\n';

  try
  {
    composer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << '\n';
    std::cerr << excp << '\n';
  }

  DisplacementFieldType::IndexType index;
  index[0] = 30;
  index[1] = 30;

  VectorType v = composer->GetOutput()->GetPixel(index);

  if (itk::Math::NotAlmostEquals(v[0], 2) || itk::Math::NotAlmostEquals(v[1], 2))
  {
    std::cerr << "Failed to compose properly." << '\n';
  }

  composer->Print(std::cout, 3);

  return EXIT_SUCCESS;
}
