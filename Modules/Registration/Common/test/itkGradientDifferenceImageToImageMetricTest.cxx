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

#include "itkGradientDifferenceImageToImageMetric.h"
#include "itkGaussianImageSource.h"
#include "itkTranslationTransform.h"
#include "itkTestingMacros.h"


int
itkGradientDifferenceImageToImageMetricTest(int, char *[])
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
  movingImageSource->SetScale(200.0f);

  fixedImageSource->SetSize(fixedImageSize);
  fixedImageSource->SetOrigin(fixedImageOrigin);
  fixedImageSource->SetSpacing(fixedImageSpacing);
  fixedImageSource->SetNormalized(false);
  fixedImageSource->SetScale(200.0f);

  movingImageSource->Update(); // Force the filter to run
  fixedImageSource->Update();  // Force the filter to run

  MovingImageType::Pointer movingImage = movingImageSource->GetOutput();
  FixedImageType::Pointer  fixedImage = fixedImageSource->GetOutput();

  // Set up the metric.
  using MetricType = itk::GradientDifferenceImageToImageMetric<FixedImageType, MovingImageType>;

  using TransformBaseType = MetricType::TransformType;
  using DerivativeType = MetricType::DerivativeType;
  using ParametersType = TransformBaseType::ParametersType;

  auto metric = MetricType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(metric, GradientDifferenceImageToImageMetric, ImageToImageMetric);


  double derivativeDelta = 0.001;
  metric->SetDerivativeDelta(derivativeDelta);
  ITK_TEST_SET_GET_VALUE(derivativeDelta, metric->GetDerivativeDelta());

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
  const unsigned int numberOfParameters = transform->GetNumberOfParameters();

  ParametersType parameters(numberOfParameters);
  for (unsigned int k = 0; k < numberOfParameters; ++k)
  {
    parameters[k] = 0.0;
  }


  try
  {
    // Initialize the metric.
    metric->Initialize();

    // Do some work
    DerivativeType          derivatives(numberOfParameters);
    MetricType::MeasureType value;
    for (double y = -10.0; y <= 10.0; y += 5.0)
    {
      parameters[1] = y;
      for (double x = -10.0; x <= 10.0; x += 5.0)
      {
        parameters[0] = x;
        metric->GetValueAndDerivative(parameters, value, derivatives);
        std::cout << "Parameters: " << parameters << ", Value: " << value << ", Derivatives: " << derivatives
                  << std::endl;
      }
    }

    // Exercise Print() method.
    metric->Print(std::cout);

    std::cout << "Test passed." << std::endl;
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
