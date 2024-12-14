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

#include <iostream>
#include "itkChangeInformationImageFilter.h"
#include "itkTestingMacros.h"

constexpr unsigned int ImageDimension = 3;
using ImageType = itk::Image<float, ImageDimension>;
using ImagePointer = ImageType::Pointer;

void
PrintInformation(ImagePointer image1, ImagePointer image2)
{
  std::cout << "Input  "
            << "      Output" << '\n';
  std::cout << "Origin"
            << "      Origin" << '\n';
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    std::cout << "  " << image1->GetOrigin()[i] << "       " << image2->GetOrigin()[i] << '\n';
  }
  std::cout << "Spacing"
            << "      Spacing" << '\n';
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    std::cout << "    " << image1->GetSpacing()[i] << "        " << image2->GetSpacing()[i] << '\n';
  }
  std::cout << "Direction"
            << "  Direction" << '\n';
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    std::cout << "  ";
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      std::cout << image1->GetDirection()[i][j] << ' ';
    }
    std::cout << "     ";
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      std::cout << image2->GetDirection()[i][j] << ' ';
    }
    std::cout << '\n';
  }
}

void
PrintInformation3(ImagePointer image1, ImagePointer image2, ImagePointer image3)
{
  std::cout << "Input  "
            << "      Output"
            << "      Reference" << '\n';
  std::cout << "Origin"
            << "      Origin"
            << "      Origin" << '\n';
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    std::cout << "  " << image1->GetOrigin()[i] << "       " << image2->GetOrigin()[i] << "       "
              << image3->GetOrigin()[i] << '\n';
  }
  std::cout << "Spacing"
            << "      Spacing"
            << "      Spacing" << '\n';
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    std::cout << "    " << image1->GetSpacing()[i] << "        " << image2->GetSpacing()[i] << "        "
              << image3->GetSpacing()[i] << '\n';
  }
  std::cout << "Direction"
            << "  Direction"
            << "  Direction" << '\n';
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    std::cout << "  ";
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      std::cout << image1->GetDirection()[i][j] << ' ';
    }
    std::cout << "     ";
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      std::cout << image2->GetDirection()[i][j] << ' ';
    }
    std::cout << "     ";
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      std::cout << image3->GetDirection()[i][j] << ' ';
    }
    std::cout << '\n';
  }
}

int
itkChangeInformationImageFilterTest(int, char *[])
{
  using FilterType = itk::ChangeInformationImageFilter<ImageType>;
  using ArrayType = itk::FixedArray<double, ImageDimension>;

  auto inputImage = ImageType::New();
  auto referenceImage = ImageType::New();
  auto filter = FilterType::New();

  itk::SpacePrecisionType  spacing[ImageDimension] = { 1, 2, 3 };
  itk::SpacePrecisionType  origin[ImageDimension] = { -100, -200, -300 };
  ImageType::DirectionType direction;
  direction[0][0] = 1.0;
  direction[1][0] = 0.0;
  direction[2][0] = 0.0;
  direction[0][1] = 0.0;
  direction[1][1] = -1.0;
  direction[2][1] = 0.0;
  direction[0][2] = 0.0;
  direction[1][2] = 0.0;
  direction[2][2] = 1.0;

  using SizeType = itk::Size<ImageDimension>;

  auto size = SizeType::Filled(20);

  inputImage->SetRegions(size);
  inputImage->Allocate();

  ImageType::DirectionType referenceDirection;
  referenceDirection[0][0] = 1.0;
  referenceDirection[1][0] = 0.0;
  referenceDirection[2][0] = 0.0;
  referenceDirection[0][1] = 0.0;
  referenceDirection[1][1] = -1.0;
  referenceDirection[2][1] = 0.0;
  referenceDirection[0][2] = 0.0;
  referenceDirection[1][2] = 0.0;
  referenceDirection[2][2] = 1.0;

  itk::SpacePrecisionType referenceOrigin[ImageDimension] = { -1000, -2000, -3000 };
  itk::SpacePrecisionType referenceSpacing[ImageDimension] = { 1000, 2000, 3000 };

  referenceImage->SetOrigin(referenceOrigin);
  referenceImage->SetSpacing(referenceSpacing);
  referenceImage->SetDirection(referenceDirection);

  referenceImage->SetRegions(size);
  referenceImage->Allocate();

  inputImage->SetSpacing(spacing);
  inputImage->SetOrigin(origin);

  const itk::SpacePrecisionType newOrigin[ImageDimension] = { 1000.0, 2000.0, 3000.0 };
  itk::SpacePrecisionType       newSpacing[ImageDimension] = { 10.0, 20.0, 30.0 };

  ImageType::OffsetValueType newOffset[ImageDimension] = { 10, 20, 30 };

  ImageType::DirectionType newDirection;
  newDirection[0][0] = 0.0;
  newDirection[1][0] = 1.0;
  newDirection[2][0] = 0.0;
  newDirection[0][1] = -1.0;
  newDirection[1][1] = 0.0;
  newDirection[2][1] = 0.0;
  newDirection[0][2] = 0.0;
  newDirection[1][2] = 0.0;
  newDirection[2][2] = -1.0;

  filter->SetInput(inputImage);
  filter->SetOutputSpacing(newSpacing);
  filter->SetOutputOrigin(newOrigin);
  filter->SetOutputOffset(newOffset);
  filter->SetOutputDirection(newDirection);
  filter->SetReferenceImage(referenceImage);


  // Test GetObjectMacro
  const ImageType * referenceImage2 = filter->GetReferenceImage();
  std::cout << "filter->GetReferenceImage(): " << referenceImage2 << '\n';

  // Test GetMacros
  const bool useReferenceImage = filter->GetUseReferenceImage();
  std::cout << "filter->GetUseReferenceImage(): " << useReferenceImage << '\n';
  const ArrayType outputSpacing = filter->GetOutputSpacing();
  std::cout << "filter->GetOutputSpacing(): " << outputSpacing << '\n';

  const ArrayType outputOrigin = filter->GetOutputOrigin();
  std::cout << "filter->GetOutputOrigin(): " << outputOrigin << '\n';

  const ImageType::DirectionType outputDirection = filter->GetOutputDirection();
  std::cout << "filter->GetOutputDirection(): " << '\n' << outputDirection << '\n';

  const bool changeSpacing = filter->GetChangeSpacing();
  std::cout << "filter->GetChangeSpacing(): " << changeSpacing << '\n';

  const bool changeOrigin = filter->GetChangeOrigin();
  std::cout << "filter->GetChangeOrigin(): " << changeOrigin << '\n';

  const bool changeDirection = filter->GetChangeDirection();
  std::cout << "filter->GetChangeDirection(): " << changeDirection << '\n';

  const bool changeRegion = filter->GetChangeRegion();
  std::cout << "filter->GetChangeRegion(): " << changeRegion << '\n';

  const bool centerImage = filter->GetCenterImage();
  std::cout << "filter->GetCenterImage(): " << centerImage << '\n';

  // Test GetVectorMacro
  const itk::OffsetValueType * outputOffset = filter->GetOutputOffset().m_InternalArray;
  std::cout << "filter->GetOutputOffset(): " << outputOffset << '\n';


  std::cout << "-----------filter: " << filter << '\n';
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------Default behavior: " << '\n';
  PrintInformation(inputImage, filter->GetOutput());

  filter->ChangeAll();
  filter->ChangeRegionOff();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------ChangeAll(), ChangeRegionOff(): " << '\n';
  PrintInformation(inputImage, filter->GetOutput());

  filter->CenterImageOn();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------CenterImageOn(): " << '\n';
  PrintInformation(inputImage, filter->GetOutput());

  filter->CenterImageOn();
  filter->ChangeSpacingOff();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------CenterImageOn(), ChangeSpacingOff(): " << '\n';
  PrintInformation(inputImage, filter->GetOutput());

  filter->CenterImageOn();
  filter->ChangeSpacingOn();
  filter->ChangeOriginOff();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------CenterImageOn(), ChangeOriginOff(): " << '\n';
  PrintInformation(inputImage, filter->GetOutput());

  filter->CenterImageOff();
  filter->ChangeNone();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------ChangeNone(): " << '\n';
  PrintInformation(inputImage, filter->GetOutput());

  filter->CenterImageOff();
  filter->UseReferenceImageOn();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------ChangeNone(), UseReferenceOn(): " << '\n';
  PrintInformation3(inputImage, filter->GetOutput(), referenceImage);

  filter->ChangeOriginOn();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------ChangeOriginOn(), UseReferenceOn(): " << '\n';
  PrintInformation3(inputImage, filter->GetOutput(), referenceImage);

  filter->ChangeOriginOff();
  filter->ChangeSpacingOn();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------ChangeSpacingOn(), UseReferenceOn(): " << '\n';
  PrintInformation3(inputImage, filter->GetOutput(), referenceImage);

  filter->ChangeOriginOff();
  filter->ChangeSpacingOff();
  filter->ChangeDirectionOn();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  std::cout << "-----------ChangeDirectionOn(), UseReferenceOn(): " << '\n';
  PrintInformation3(inputImage, filter->GetOutput(), referenceImage);

  filter->ChangeAll();
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->UpdateLargestPossibleRegion());
  std::cout << "-----------ChangeAll(), UseReferenceOn(): " << '\n';
  PrintInformation3(inputImage, filter->GetOutput(), referenceImage);


  return EXIT_SUCCESS;
}
