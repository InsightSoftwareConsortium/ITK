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

#include "itkRandomImageSource.h"
#include "itkTestingMacros.h"


template <typename TImage>
int
itkRandomImageSourceAttributesTestHelper(const typename TImage::SizeType      size,
                                         const typename TImage::SpacingType   spacing,
                                         const typename TImage::PointType     origin,
                                         const typename TImage::DirectionType direction,
                                         const typename TImage::ValueType     min,
                                         const typename TImage::ValueType     max)
{

  using ImageSourceType = itk::RandomImageSource<TImage>;
  auto randomImageSource = ImageSourceType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(randomImageSource, RandomImageSource, ImageSource);


  randomImageSource->SetSize(size);
  ITK_TEST_SET_GET_VALUE(*size.GetSize(), *randomImageSource->GetSize());

  randomImageSource->SetSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, randomImageSource->GetSpacing());

  randomImageSource->SetOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, randomImageSource->GetOrigin());

  randomImageSource->SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, randomImageSource->GetDirection());

  randomImageSource->SetMin(min);
  ITK_TEST_SET_GET_VALUE(min, randomImageSource->GetMin());

  randomImageSource->SetMax(max);
  ITK_TEST_SET_GET_VALUE(max, randomImageSource->GetMax());


  return EXIT_SUCCESS;
}


int
itkRandomImageSourceAttributesTest(int, char *[])
{

  constexpr unsigned int Dimension2D = 2;
  constexpr unsigned int Dimension3D = 3;

  using PixelType = float;

  int testStatus = EXIT_SUCCESS;

  {
    using ImageType2D = itk::Image<PixelType, Dimension2D>;

    const ImageType2D::SizeType      size{ { 25, 25 } };
    const ImageType2D::SpacingType   spacing{ { { 0.7, 2.1 } } };
    const ImageType2D::PointType     origin{ { { -1.7, 5.2 } } };
    const double                     d[4] = { 0, 1.0, 1.0, 0 };
    const ImageType2D::DirectionType direction = ImageType2D::DirectionType::InternalMatrixType(d);
    const ImageType2D::ValueType     min{ 0.0 };
    const ImageType2D::ValueType     max{ 1000.0 };

    testStatus = itkRandomImageSourceAttributesTestHelper<ImageType2D>(size, spacing, origin, direction, min, max);
  }

  {
    using ImageType3D = itk::Image<PixelType, Dimension3D>;

    const ImageType3D::SizeType      size{ { 14, 17, 36 } };
    const ImageType3D::SpacingType   spacing{ { { 0.7, 0.4, 1.2 } } };
    const ImageType3D::PointType     origin{ { { -1.7, 5.2, 3.4 } } };
    const double                     d[9] = { 0, 1.0, 0, 1.0, 0, 0, 0, 1.0, 0 };
    const ImageType3D::DirectionType direction = ImageType3D::DirectionType::InternalMatrixType(d);
    const ImageType3D::ValueType     min{ 0.0 };
    const ImageType3D::ValueType     max{ 10.0 };

    testStatus = itkRandomImageSourceAttributesTestHelper<ImageType3D>(size, spacing, origin, direction, min, max);
  }


  std::cout << "Test finished." << std::endl;
  return testStatus;
}
