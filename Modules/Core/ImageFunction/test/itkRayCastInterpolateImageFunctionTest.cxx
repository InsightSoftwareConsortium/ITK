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

#include "itkImage.h"
#include "itkRayCastInterpolateImageFunction.h"
#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkTestingMacros.h"


int
itkRayCastInterpolateImageFunctionTest(int itkNotUsed(argc), char * itkNotUsed(argv)[])
{
  std::cout << "Testing RayCastInterpolateImageFunction:\n";

  using PixelType = unsigned char;
  constexpr unsigned int ImageDimension = 3;

  using ImageType = itk::Image<PixelType, ImageDimension>;

  using IndexType = ImageType::IndexType;
  using PointType = ImageType::PointType;
  using SpacingType = ImageType::SpacingType;
  using SizeType = ImageType::SizeType;
  using RegionType = ImageType::RegionType;

  /* Allocate a simple test image */
  auto      image = ImageType::New();
  IndexType start;
  start.Fill(0);
  SizeType size;
  size[0] = 30;
  size[1] = 30;
  size[2] = 30;

  RegionType region;
  region.SetIndex(start);
  region.SetSize(size);
  image->SetRegions(region);
  image->Allocate();

  PointType origin;
  origin.Fill(0.0);

  SpacingType spacing;
  spacing.Fill(1.0);

  /* Set origin and spacing of physical coordinates */
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  /* Initialize the image contents */
  IndexType index;
  for (unsigned int slice = 0; slice < size[2]; ++slice)
  {
    index[2] = slice;
    for (unsigned int row = 0; row < size[1]; ++row)
    {
      index[1] = row;
      for (unsigned int col = 0; col < size[0]; ++col)
      {
        index[0] = col;
        auto value = (PixelType)(slice + row + col);
        image->SetPixel(index, value);
      }
    }
  }

  using RayCastInterpolatorType = itk::RayCastInterpolateImageFunction<ImageType, double>;

  /* Create and initialize the interpolator */
  auto interp = RayCastInterpolatorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(interp, RayCastInterpolateImageFunction, InterpolateImageFunction);


  // Test exceptions
  ITK_TRY_EXPECT_EXCEPTION(interp->GetRadius());

  interp->SetInputImage(image);

  PointType focus;
  focus[0] = 15.0;
  focus[1] = 15.0;
  focus[2] = 100.0;

  interp->SetFocalPoint(focus);
  ITK_TEST_SET_GET_VALUE(focus, interp->GetFocalPoint());


  /* Create the transform */
  using TransformType = itk::TranslationTransform<double, ImageDimension>;

  auto transform = TransformType::New();

  interp->SetTransform(transform);
  ITK_TEST_SET_GET_VALUE(transform, interp->GetTransform());

  /* Create the auxiliary interpolator */
  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType, double>;

  auto auxInterpolator = InterpolatorType::New();

  interp->SetInterpolator(auxInterpolator);
  ITK_TEST_SET_GET_VALUE(auxInterpolator, interp->GetInterpolator());

  /* Exercise the SetThreshold() method */
  double threshold = 1.0;
  interp->SetThreshold(threshold);
  ITK_TEST_SET_GET_VALUE(threshold, interp->GetThreshold());

  /* Evaluate the function */
  double integral;


  // Evaluate the ray casting function at the same point as the focal point:
  //   - Allows to increase coverage.
  //   - Makes the ray be invalid.
  //   - Sets the traversal direction to TraversalDirectionEnum::UNDEFINED_DIRECTION
  //   - The integral should equal to 0.
  integral = interp->Evaluate(focus);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(integral, 0.0));

  PointType query;
  query[0] = 15.;
  query[1] = 15.;
  query[2] = -2.;

  integral = interp->Evaluate(query);

  std::cout << "Integral = " << integral << std::endl;

  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    ITK_TEST_SET_GET_VALUE(size[d], interp->GetRadius()[d]);
  }

  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(integral, 1276.));


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
