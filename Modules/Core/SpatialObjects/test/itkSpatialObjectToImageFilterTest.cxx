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

#include "itkEllipseSpatialObject.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkCommand.h"
#include "itkTestingMacros.h"

class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  itk::ProcessObject::Pointer m_Process;
};

int
itkSpatialObjectToImageFilterTest(int, char *[])
{
  using EllipseType = itk::EllipseSpatialObject<2>;

  auto ellipse = EllipseType::New();
  ellipse->SetRadiusInObjectSpace(10);
  ellipse->Update();

  // Center the circle in the image
  EllipseType::TransformType::OffsetType offset;
  offset.Fill(25);
  ellipse->GetModifiableObjectToParentTransform()->SetOffset(offset);
  ellipse->Update();

  using ImageType = itk::Image<double, 2>;

  using SpatialObjectToImageFilterType = itk::SpatialObjectToImageFilter<EllipseType, ImageType>;
  auto imageFilter = SpatialObjectToImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(imageFilter, SpatialObjectToImageFilter, ImageSource);


  imageFilter->SetInput(ellipse);

  SpatialObjectToImageFilterType::ValueType insideValue = 2;
  imageFilter->SetInsideValue(insideValue);
  ITK_TEST_SET_GET_VALUE(insideValue, imageFilter->GetInsideValue());

  SpatialObjectToImageFilterType::ValueType outsideValue = 0;
  imageFilter->SetOutsideValue(0);
  ITK_TEST_SET_GET_VALUE(outsideValue, imageFilter->GetOutsideValue());

  unsigned int childrenDepth = 1;
  imageFilter->SetChildrenDepth(childrenDepth);
  ITK_TEST_SET_GET_VALUE(childrenDepth, imageFilter->GetChildrenDepth());

  ImageType::IndexType indx;
  indx[0] = 10;
  indx[1] = 10;
  imageFilter->SetIndex(indx);
  ITK_TEST_SET_GET_VALUE(indx, imageFilter->GetIndex());

  ImageType::SizeType size;
  size[0] = 50;
  size[1] = 50;
  imageFilter->SetSize(size);
  ITK_TEST_SET_GET_VALUE(size, imageFilter->GetSize());

  // Testing spacing
  std::cout << "Testing Spacing: ";

  float  spacingFloat[2];
  double spacingDouble[2];

  for (unsigned int i = 0; i < 2; ++i)
  {
    spacingFloat[i] = 1.0;
    spacingDouble[i] = 1.0;
  }
  imageFilter->SetSpacing(spacingFloat);
  imageFilter->SetSpacing(spacingDouble);
  const double * spacing_result = imageFilter->GetSpacing();

  for (unsigned int i = 0; i < 2; ++i)
  {
    if (spacing_result[i] != 1.0)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing Origin
  std::cout << "Testing Origin: ";

  float  originFloat[2];
  double originDouble[2];

  for (unsigned int i = 0; i < 2; ++i)
  {
    originFloat[i] = 0.0;
    originDouble[i] = 0.0;
  }
  imageFilter->SetOrigin(originFloat);
  imageFilter->SetOrigin(originDouble);
  const double * origin_result = imageFilter->GetOrigin();

  for (unsigned int i = 0; i < 2; ++i)
  {
    if (origin_result[i] != 0.0)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing PrintSelf
  std::cout << imageFilter << std::endl;

  // Test Progress Reporter
  ShowProgressObject progressWatch(imageFilter);
  using CommandType = itk::SimpleMemberCommand<ShowProgressObject>;
  auto command = CommandType::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  imageFilter->AddObserver(itk::ProgressEvent(), command);

  // Update the filter
  imageFilter->Update();

  ImageType::Pointer image = imageFilter->GetOutput();

  std::cout << "Testing Output Image: ";

  ImageType::IndexType index;
  // Test only centered pixels
  for (int i = -5; i < 5; ++i)
  {
    for (int j = -5; j < 5; ++j)
    {
      index[0] = 25 + i;
      index[1] = 25 + j;

      if (image->GetPixel(index) != 2.0)
      {
        std::cout << "[FAILURE]" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Test the UseObjectValue
  bool useObjectValue = true;
  imageFilter->SetUseObjectValue(useObjectValue);
  ITK_TEST_SET_GET_BOOLEAN(imageFilter, UseObjectValue, useObjectValue);

  imageFilter->Update();

  std::cout << "Testing SetUseObjectValue: ";

  // Test only centered pixels
  for (int i = -5; i < 5; ++i)
  {
    for (int j = -5; j < 5; ++j)
    {
      index[0] = 25 + i;
      index[1] = 25 + j;

      if (image->GetPixel(index) != 1.0)
      {
        std::cout << "[FAILURE]" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
