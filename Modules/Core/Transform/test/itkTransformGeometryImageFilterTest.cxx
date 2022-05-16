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

#include "itkTransformGeometryImageFilter.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVersorRigid3DTransform.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingMacros.h"

#include <iostream>

// return true if it fails the validation
inline bool
Validate(double input, double desired, double tolerance)
{
  return std::abs<double>(input - desired) > tolerance * std::abs<double>(desired);
}

// Returns true if images are different
template <typename ImageType>
bool
imagesDifferent(ImageType * baselineImage, ImageType * outputImage)
{
  double tol = 1.e-3; // tolerance

  typename ImageType::PointType     origin = outputImage->GetOrigin();
  typename ImageType::DirectionType direction = outputImage->GetDirection();
  typename ImageType::SpacingType   spacing = outputImage->GetSpacing();

  typename ImageType::PointType     origin_d = baselineImage->GetOrigin();
  typename ImageType::DirectionType direction_d = baselineImage->GetDirection();
  typename ImageType::SpacingType   spacing_d = baselineImage->GetSpacing();

  // Image info validation
  bool result = false; // no difference by default
  for (unsigned int i = 0; i < ImageType::ImageDimension; ++i)
  {
    result = (result || Validate(origin[i], origin_d[i], tol));
    result = (result || Validate(spacing[i], spacing_d[i], tol));
    for (unsigned int j = 0; j < ImageType::ImageDimension; ++j)
    {
      result = (result || Validate(direction(i, j), direction_d(i, j), tol));
    }
  }

  // Voxel contents validation
  using ImageConstIterator = itk::ImageRegionConstIterator<ImageType>;
  ImageConstIterator it1(outputImage, outputImage->GetRequestedRegion());
  ImageConstIterator it2(baselineImage, baselineImage->GetRequestedRegion());
  it1.GoToBegin();
  it2.GoToBegin();
  while (!it1.IsAtEnd())
  {
    result = (result || Validate(it1.Get(), it2.Get(), tol));
    ++it1;
    ++it2;
  }

  return result;
}

int
itkTransformGeometryImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Wrong arguments!" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage baselineImage outputImage"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using PixelType = short;

  using ImageType = itk::Image<PixelType, Dimension>;
  using ImagePointer = ImageType::Pointer;
  using TransformType = itk::VersorRigid3DTransform<double>;
  using FilterType = itk::TransformGeometryImageFilter<ImageType, ImageType>;

  // Read in input test image
  ImagePointer inputImage;
  ITK_TRY_EXPECT_NO_EXCEPTION(inputImage = itk::ReadImage<ImageType>(argv[1]));

  // Set up transforms
  ImageType::PointType center;
  center.Fill(0.0);
  center[0] = 2.0; // In mm along X (RL-axis)
  center[1] = 5.0; // In mm along Y (AP-axis)
  center[2] = 7.0; // In mm along Z (IS-axis)

  itk::Vector<double, Dimension> translation;
  translation.Fill(0.);
  translation[0] = 10.0; // In mm along X (RL-axis)
  translation[1] = 15.0; // In mm along Y (AP-axis)
  translation[2] = 20.0; // In mm along Z (IS-axis)

  itk::Vector<double, Dimension> rotationAxis;
  rotationAxis[0] = 0.1;
  rotationAxis[1] = 0.2;
  rotationAxis[2] = 0.7;

  double rotationAngle = .5; // Radians

  auto transform = TransformType::New(); // Identity by default
  transform->SetCenter(center);
  transform->Translate(translation);
  transform->SetRotation(rotationAxis, rotationAngle);

  // Set up the transform filter
  auto filter = FilterType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, TransformGeometryImageFilter, InPlaceImageFilter);

  // Test the exceptions
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  filter->SetInputImage(inputImage);
  ITK_TEST_SET_GET_VALUE(inputImage, filter->GetInputImage());
  filter->SetTransform(transform);
  ITK_TEST_SET_GET_VALUE(transform, filter->GetTransform());
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());
  ImagePointer outputImage = filter->GetOutput();
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(outputImage, argv[3]));

  // Read in baseline image
  ImagePointer baselineImage = nullptr;
  ITK_TRY_EXPECT_NO_EXCEPTION(baselineImage = itk::ReadImage<ImageType>(argv[2]));

  // Now do comparisons
  bool result = imagesDifferent<ImageType>(baselineImage, outputImage);
  transform->ApplyToImageMetadata(inputImage);
  result = (result || imagesDifferent<ImageType>(baselineImage, inputImage));

  // Make sure we can invoke ApplyToImageMetadata via const/raw pointer
  TransformType * rawPointerTransform = transform.GetPointer();
  rawPointerTransform->ApplyToImageMetadata(inputImage);
  rawPointerTransform->ApplyToImageMetadata(inputImage.GetPointer());
  const TransformType * constRawPointerTransform = transform.GetPointer();
  constRawPointerTransform->ApplyToImageMetadata(inputImage);
  TransformType::ConstPointer constPointerTransform = transform.GetPointer();
  constPointerTransform->ApplyToImageMetadata(inputImage);
  constPointerTransform->ApplyToImageMetadata(inputImage.GetPointer());

  return result;
}
