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

#include "itkGaussianImageSource.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkKullbackLeiblerCompareHistogramImageToImageMetric.h"
#include "itkTranslationTransform.h"
#include "itkTestingMacros.h"

/** This test uses two 2D-Gaussians (standard deviation RegionSize/2).
    This test computes the mutual information between the two images.
*/
int
itkCompareHistogramImageToImageMetricTest(int, char *[])
{

  // Create two simple images.
  constexpr unsigned int ImageDimension = 2;
  using PixelType = double;
  using CoordinateRepresentationType = double;

  // Allocate Images
  using MovingImageType = itk::Image<PixelType, ImageDimension>;
  using FixedImageType = itk::Image<PixelType, ImageDimension>;

  // Declare Gaussian Sources
  using MovingImageSourceType = itk::GaussianImageSource<MovingImageType>;
  using FixedImageSourceType = itk::GaussianImageSource<FixedImageType>;

  // Note: the following declarations are classical arrays
  FixedImageType::SizeValueType  fixedImageSize[] = { 100, 100 };
  MovingImageType::SizeValueType movingImageSize[] = { 100, 100 };

  FixedImageType::SpacingValueType  fixedImageSpacing[] = { 1.0f, 1.0f };
  MovingImageType::SpacingValueType movingImageSpacing[] = { 1.0f, 1.0f };

  FixedImageType::PointValueType  fixedImageOrigin[] = { 0.0f, 0.0f };
  MovingImageType::PointValueType movingImageOrigin[] = { 0.0f, 0.0f };

  auto movingImageSource = MovingImageSourceType::New();
  auto fixedImageSource = FixedImageSourceType::New();

  movingImageSource->SetSize(movingImageSize);
  movingImageSource->SetOrigin(movingImageOrigin);
  movingImageSource->SetSpacing(movingImageSpacing);
  movingImageSource->SetNormalized(false);
  movingImageSource->SetScale(250.0f);

  fixedImageSource->SetSize(fixedImageSize);
  fixedImageSource->SetOrigin(fixedImageOrigin);
  fixedImageSource->SetSpacing(fixedImageSpacing);
  fixedImageSource->SetNormalized(false);
  fixedImageSource->SetScale(250.0f);

  ITK_TRY_EXPECT_NO_EXCEPTION(movingImageSource->Update()); // Force the filter to run
  ITK_TRY_EXPECT_NO_EXCEPTION(fixedImageSource->Update());  // Force the filter to run

  MovingImageType::Pointer movingImage = movingImageSource->GetOutput();
  FixedImageType::Pointer  fixedImage = fixedImageSource->GetOutput();

  // Set up the metric.
  using MetricType = itk::KullbackLeiblerCompareHistogramImageToImageMetric<FixedImageType, MovingImageType>;
  using TransformBaseType = MetricType::TransformType;
  using ScalesType = MetricType::ScalesType;
  using ParametersType = TransformBaseType::ParametersType;

  auto metric = MetricType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    metric, KullbackLeiblerCompareHistogramImageToImageMetric, CompareHistogramImageToImageMetric);


  auto epsilon = 1e-12;
  metric->SetEpsilon(epsilon);
  ITK_TEST_SET_GET_VALUE(epsilon, metric->GetEpsilon());

  unsigned int                        nBins = 256;
  MetricType::HistogramType::SizeType histSize;

  histSize.SetSize(2);

  histSize[0] = nBins;
  histSize[1] = nBins;
  metric->SetHistogramSize(histSize);

  // Plug the images into the metric.
  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);

  // Set up a transform.
  using TransformType = itk::TranslationTransform<CoordinateRepresentationType, ImageDimension>;

  auto transform = TransformType::New();
  metric->SetTransform(transform);

  // Set up an interpolator.
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  auto interpolator = InterpolatorType::New();
  interpolator->SetInputImage(movingImage);
  metric->SetInterpolator(interpolator);

  // Define the region over which the metric will be computed.
  metric->SetFixedImageRegion(fixedImage->GetBufferedRegion());

  // Set up transform parameters.
  ParametersType parameters(transform->GetNumberOfParameters());

  for (unsigned int k = 0; k < ImageDimension; ++k)
  {
    parameters[k] = 0.0f;
  }

  // Set scales for derivative calculation.
  ScalesType scales(transform->GetNumberOfParameters());

  for (unsigned int k = 0; k < transform->GetNumberOfParameters(); ++k)
  {
    scales[k] = 1;
  }

  metric->SetDerivativeStepLengthScales(scales);

  // Now set up the Training Stuff
  metric->SetTrainingTransform(transform);
  ITK_TEST_SET_GET_VALUE(transform, metric->GetTrainingTransform());

  metric->SetTrainingFixedImage(fixedImage);
  ITK_TEST_SET_GET_VALUE(fixedImage, metric->GetTrainingFixedImage());

  metric->SetTrainingFixedImageRegion(fixedImage->GetBufferedRegion());
  ITK_TEST_SET_GET_VALUE(fixedImage->GetBufferedRegion(), metric->GetTrainingFixedImageRegion());

  metric->SetTrainingMovingImage(movingImage);
  ITK_TEST_SET_GET_VALUE(movingImage, metric->GetTrainingMovingImage());

  metric->SetTrainingInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, metric->GetTrainingInterpolator());

  ITK_TEST_EXPECT_EQUAL(transform->GetNumberOfParameters(), metric->GetNumberOfParameters());

  // Initialize the metric.
  metric->Initialize();

  // Print out metric value and derivative.
  MetricType::MeasureType    measure = metric->GetValue(parameters);
  MetricType::DerivativeType derivative;
  metric->GetDerivative(parameters, derivative);

  std::cout << "Metric value = " << measure << std::endl << "Derivative = " << derivative << std::endl;


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
