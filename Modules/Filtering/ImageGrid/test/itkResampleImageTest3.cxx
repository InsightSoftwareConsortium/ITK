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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkTestingMacros.h"

/* Further testing of itkResampleImageFilter
 * Test that ResampleImageFilter can handle a ReferenceImage with
 * meta information that differs from the input imake.
 * Output is compared with baseline image using the cmake itk_add_test
 * '--compare' option.
 */

int
itkResampleImageTest3(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage resampledImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int VDimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, VDimension>;
  using CoordRepType = double;

  using TransformType = itk::IdentityTransform<CoordRepType, VDimension>;
  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType, CoordRepType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;
  using ChangeInfoType = itk::ChangeInformationImageFilter<ImageType>;
  using ResampleFilterType = itk::ResampleImageFilter<ImageType, ImageType>;

  auto reader1 = ReaderType::New();
  reader1->SetFileName(argv[1]);
  reader1->Update();

  // Create an identity transformation
  auto identityTransform = TransformType::New();

  // Create a linear interpolation image function
  auto interpolator = InterpolatorType::New();

  // Create an image with flipped directions
  ImageType::DirectionType direction;
  direction[0][0] = -1.0;
  direction[0][1] = 0.0;
  direction[1][0] = 0.0;
  direction[1][1] = -1.0;
  ImageType::RegionType inputRegion = reader1->GetOutput()->GetLargestPossibleRegion();
  ImageType::PointType  origin;
  origin[0] = inputRegion.GetSize()[0];
  origin[1] = inputRegion.GetSize()[1];

  auto changeInfo = ChangeInfoType::New();
  changeInfo->ChangeDirectionOn();
  changeInfo->SetOutputDirection(direction);
  changeInfo->ChangeOriginOn();
  changeInfo->SetOutputOrigin(origin);
  changeInfo->SetInput(reader1->GetOutput());

  // Create and configure a resampling filter
  auto resample = ResampleFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(resample, ResampleImageFilter, ImageToImageFilter);

  resample->SetInput(reader1->GetOutput());
  ITK_TEST_SET_GET_VALUE(reader1->GetOutput(), resample->GetInput());

  resample->SetReferenceImage(changeInfo->GetOutput());
  ITK_TEST_SET_GET_VALUE(changeInfo->GetOutput(), resample->GetReferenceImage());

  resample->UseReferenceImageOn();
  ITK_TEST_EXPECT_TRUE(resample->GetUseReferenceImage());

  resample->SetTransform(identityTransform);
  ITK_TEST_SET_GET_VALUE(identityTransform, resample->GetTransform());

  resample->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, resample->GetInterpolator());

  auto writer1 = WriterType::New();
  writer1->SetFileName(argv[2]);
  writer1->SetInput(resample->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer1->Update());


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
