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

#include "itkMIRegistrationFunction.h"
#include "itkGaussianImageSource.h"
#include "itkTestingMacros.h"

#include <iostream>

/**
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test exercises the MIRegistrationFunction
 *
 */

int
itkMIRegistrationFunctionTest(int, char *[])
{

  constexpr unsigned int ImageDimension = 2;

  using PixelType = double;
  using DeformationPixelType = itk::Vector<PixelType, ImageDimension>;

  using MovingImageType = itk::Image<PixelType, ImageDimension>;
  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using DisplacementFieldType = itk::Image<DeformationPixelType, ImageDimension>;

  // Declare Gaussian image sources
  using MovingImageSourceType = itk::GaussianImageSource<MovingImageType>;
  using FixedImageSourceType = itk::GaussianImageSource<FixedImageType>;

  // Create the two images
  FixedImageType::SizeValueType  fixedImageSize[] = { 100, 100 };
  MovingImageType::SizeValueType movingImageSize[] = { 100, 100 };

  FixedImageType::SpacingValueType  fixedImageSpacing[] = { 1.0f, 1.0f };
  MovingImageType::SpacingValueType movingImageSpacing[] = { 1.0f, 1.0f };

  FixedImageType::PointValueType  fixedImageOrigin[] = { 0.0f, 0.0f };
  MovingImageType::PointValueType movingImageOrigin[] = { 0.0f, 0.0f };

  auto movingImageSource = MovingImageSourceType::New();
  auto fixedImageSource = FixedImageSourceType::New();

  fixedImageSource->SetSize(fixedImageSize);
  fixedImageSource->SetOrigin(fixedImageOrigin);
  fixedImageSource->SetSpacing(fixedImageSpacing);
  fixedImageSource->SetNormalized(false);
  fixedImageSource->SetScale(250.0f);

  movingImageSource->SetSize(movingImageSize);
  movingImageSource->SetOrigin(movingImageOrigin);
  movingImageSource->SetSpacing(movingImageSpacing);
  movingImageSource->SetNormalized(false);
  movingImageSource->SetScale(250.0f);

  movingImageSource->Update();
  fixedImageSource->Update();

  MovingImageType::Pointer movingImage = movingImageSource->GetOutput();
  FixedImageType::Pointer  fixedImage = fixedImageSource->GetOutput();

  // Set up the metric
  using MetricFunctionType = itk::MIRegistrationFunction<FixedImageType, MovingImageType, DisplacementFieldType>;

  auto metricFunction = MetricFunctionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(metricFunction, MIRegistrationFunction, PDEDeformableRegistrationFunction);


  // Plug the images into the metric function
  metricFunction->SetFixedImage(fixedImage);
  metricFunction->SetMovingImage(movingImage);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
