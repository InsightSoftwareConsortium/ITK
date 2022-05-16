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

#include "itkAffineTransform.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkNearestNeighborExtrapolateImageFunction.h"
#include "itkTestingMacros.h"

/* Further testing of itkResampleImageFilter
 * Output is compared with baseline image using the cmake itk_add_test
 * '--compare' option.
 */

namespace
{

template <typename TCoordRepType, unsigned int VDimension>
class NonlinearAffineTransform : public itk::AffineTransform<TCoordRepType, VDimension>
{
public:
  /** Standard class type aliases.   */
  using Self = NonlinearAffineTransform;
  using Superclass = itk::AffineTransform<TCoordRepType, VDimension>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** New macro for creation of through a smart pointer. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NonlinearAffineTransform, AffineTransform);

  /** Override this. See test below. */
  bool
  IsLinear() const override
  {
    return false;
  }
};
} // namespace

int
itkResampleImageTest2(int argc, char * argv[])
{

  if (argc < 8)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "inputImage "
              << " referenceImage"
              << " resampledImageLinear"
              << " resampledImageNonLinear"
              << " resampledImageLinearNearestExtrapolate"
              << " resampledImageNonLinearNearestExtrapolate"
              << " useReferenceImage"
              << " [outputSpacing]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int VDimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, VDimension>;
  using CoordRepType = double;

  using AffineTransformType = itk::AffineTransform<CoordRepType, VDimension>;
  using NonlinearAffineTransformType = NonlinearAffineTransform<CoordRepType, VDimension>;
  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType, CoordRepType>;
  using ExtrapolatorType = itk::NearestNeighborExtrapolateImageFunction<ImageType, CoordRepType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto reader1 = ReaderType::New();
  auto reader2 = ReaderType::New();

  auto writer1 = WriterType::New();
  auto writer2 = WriterType::New();
  auto writer3 = WriterType::New();
  auto writer4 = WriterType::New();

  reader1->SetFileName(argv[1]);

  writer1->SetFileName(argv[3]);
  writer2->SetFileName(argv[4]);
  writer3->SetFileName(argv[5]);
  writer4->SetFileName(argv[6]);

  // Create an affine transformation
  auto affineTransform = AffineTransformType::New();
  affineTransform->Scale(2.0);

  // Create a linear interpolation image function
  auto interpolator = InterpolatorType::New();

  // Create a nearest neighbor extrapolate image function
  auto extrapolator = ExtrapolatorType::New();

  // Create and configure a resampling filter
  using ResampleFilterType = itk::ResampleImageFilter<ImageType, ImageType>;

  auto resample = ResampleFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(resample, ResampleImageFilter, ImageToImageFilter);


  ITK_TRY_EXPECT_NO_EXCEPTION(reader1->Update());

  resample->SetInput(reader1->GetOutput());
  ITK_TEST_SET_GET_VALUE(reader1->GetOutput(), resample->GetInput());

  resample->SetTransform(affineTransform);
  ITK_TEST_SET_GET_VALUE(affineTransform, resample->GetTransform());

  resample->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, resample->GetInterpolator());

  bool useReferenceImage = std::stoi(argv[7]);
  ITK_TEST_SET_GET_BOOLEAN(resample, UseReferenceImage, useReferenceImage);


  // If the reference image is to be used, read it and set it to the filter;
  // else, create an image region for the output image.
  if (useReferenceImage)
  {
    reader2->SetFileName(argv[2]);

    ITK_TRY_EXPECT_NO_EXCEPTION(reader2->Update());

    resample->SetReferenceImage(reader2->GetOutput());
    ITK_TEST_SET_GET_VALUE(reader2->GetOutput(), resample->GetReferenceImage());
  }
  else
  {
    // Set a fixed, isotropic output spacing
    typename ImageType::SpacingType::ValueType outputSpacingValue = 1.5;
    if (argc > 7)
    {
      outputSpacingValue = std::stod(argv[8]);
    }

    typename ImageType::SpacingType outputSpacing;
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      outputSpacing[i] = outputSpacingValue;
    }

    const typename ImageType::SizeType &    inputSize = resample->GetInput()->GetLargestPossibleRegion().GetSize();
    const typename ImageType::SpacingType & inputSpacing = resample->GetInput()->GetSpacing();

    typename ImageType::SizeType outputSize;

    using SizeValueType = typename ImageType::SizeType::SizeValueType;
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      outputSize[i] =
        itk::Math::Ceil<SizeValueType>(static_cast<double>(inputSize[i]) * inputSpacing[i] / outputSpacing[i]);
    }

    typename ImageType::DirectionType outputDirection = resample->GetInput()->GetDirection();

    typename ImageType::PointType outputOrigin = resample->GetInput()->GetOrigin();

    resample->SetOutputSpacing(outputSpacing);
    ITK_TEST_SET_GET_VALUE(outputSpacing, resample->GetOutputSpacing());

    resample->SetSize(outputSize);
    ITK_TEST_SET_GET_VALUE(outputSize, resample->GetSize());

    resample->SetOutputOrigin(outputOrigin);
    ITK_TEST_SET_GET_VALUE(outputOrigin, resample->GetOutputOrigin());

    resample->SetOutputDirection(outputDirection);
    ITK_TEST_SET_GET_VALUE(outputDirection, resample->GetOutputDirection());
  }

  // Run the resampling filter with the normal, linear, affine transform.
  // This will use ResampleImageFilter::LinearThreadedGenerateData().
  std::cout << "Test with normal AffineTransform." << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(resample->Update());

  writer1->SetInput(resample->GetOutput());

  // Check GetReferenceImage
  if (useReferenceImage)
  {
    if (resample->GetReferenceImage() != reader2->GetOutput())
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "GetReferenceImage() failed ! " << std::endl;
      return EXIT_FAILURE;
    }
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(writer1->Update());


  // Assign an affine transform that returns
  // false for IsLinear() instead of true, to force
  // the filter to use the NonlinearThreadedGenerateData method
  // instead of LinearThreadedGenerateData. This will test that
  // we get the same results for both methods.
  std::cout << "Test with NonlinearAffineTransform." << std::endl;
  auto nonlinearAffineTransform = NonlinearAffineTransformType::New();

  nonlinearAffineTransform->Scale(2.0);
  resample->SetTransform(nonlinearAffineTransform);

  ITK_TRY_EXPECT_NO_EXCEPTION(resample->Update());

  writer2->SetInput(resample->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer2->Update());


  // Instead of using the default pixel when sampling outside the input image,
  // we use a nearest neighbor extrapolator.
  std::cout << "Test with nearest neighbor extrapolator, affine transform." << std::endl;
  resample->SetTransform(affineTransform);
  resample->SetExtrapolator(extrapolator);
  ITK_TEST_SET_GET_VALUE(extrapolator, resample->GetExtrapolator());

  ITK_TRY_EXPECT_NO_EXCEPTION(resample->Update());

  writer3->SetInput(resample->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer3->Update());


  // Instead of using the default pixel when sampling outside the input image,
  // we use a nearest neighbor extrapolator.
  std::cout << "Test with nearest neighbor extrapolator, nonlinear transform." << std::endl;
  resample->SetTransform(nonlinearAffineTransform);

  ITK_TRY_EXPECT_NO_EXCEPTION(resample->Update());

  writer4->SetInput(resample->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer4->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
