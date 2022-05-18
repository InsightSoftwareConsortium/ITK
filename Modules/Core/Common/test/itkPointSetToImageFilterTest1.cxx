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

#include "itkImageFileWriter.h"
#include "itkPointSetToImageFilter.h"
#include "itkPointSet.h"
#include "itkTestingMacros.h"


int
itkPointSetToImageFilterTest1(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputImageFile" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int PointSetDimension = 2;
  constexpr unsigned int ImageDimension = 2;

  using PointSetPointType = float;
  using PixelType = unsigned char;

  using PointSetType = itk::PointSet<PointSetPointType, PointSetDimension>;
  using BinaryImageType = itk::Image<PixelType, ImageDimension>;

  using PointSetType = itk::PointSet<PointSetPointType, PointSetDimension>;

  using PointType = PointSetType::PointType;

  auto pointSet = PointSetType::New();

  // Create a point set describing a circle
  float        radius = 100.0;
  unsigned int count = 0;
  for (float theta = 0; theta < 2.0 * itk::Math::pi; theta += 0.1)
  {
    PointType point;
    point[0] = radius * std::cos(theta);
    point[1] = radius * std::sin(theta);

    pointSet->SetPoint(count, point);
    count++;
  }

  using FilterType = itk::PointSetToImageFilter<PointSetType, BinaryImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, PointSetToImageFilter, ImageSource);


  BinaryImageType::SpacingType::ValueType spacingValue = 1.0;
  BinaryImageType::SpacingType            spacing;
  spacing.Fill(spacingValue);
  filter->SetSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, filter->GetSpacing());

  BinaryImageType::PointType origin{ { { -125, -125 } } };
  filter->SetOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, filter->GetOrigin());

  typename BinaryImageType::DirectionType direction;
  direction.SetIdentity();
  filter->SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, filter->GetDirection());

  typename BinaryImageType::ValueType insideValue = itk::NumericTraits<typename BinaryImageType::ValueType>::OneValue();
  filter->SetInsideValue(insideValue);
  ITK_TEST_SET_GET_VALUE(insideValue, filter->GetInsideValue());

  typename BinaryImageType::ValueType outsideValue =
    itk::NumericTraits<typename BinaryImageType::ValueType>::ZeroValue();
  filter->SetOutsideValue(outsideValue);
  ITK_TEST_SET_GET_VALUE(outsideValue, filter->GetOutsideValue());

  typename BinaryImageType::SizeType size = { { 250, 250 } };
  filter->SetSize(size);
  ITK_TEST_SET_GET_VALUE(size, filter->GetSize());

  filter->SetInput(pointSet);
  ITK_TEST_SET_GET_VALUE(pointSet, filter->GetInput());

  unsigned int idx = 0;
  ITK_TEST_SET_GET_VALUE(pointSet, filter->GetInput(idx));

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  BinaryImageType::Pointer binaryImage = filter->GetOutput();

  itk::WriteImage(binaryImage, argv[1]);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
