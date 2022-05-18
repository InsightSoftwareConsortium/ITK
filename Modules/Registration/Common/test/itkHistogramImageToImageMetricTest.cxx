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
#include "itkMeanSquaresHistogramImageToImageMetric.h"
#include "itkTranslationTransform.h"
#include "itkMath.h"

int
itkHistogramImageToImageMetricTest(int, char *[])
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

  movingImageSource->Update(); // Force the filter to run
  fixedImageSource->Update();  // Force the filter to run

  MovingImageType::Pointer movingImage = movingImageSource->GetOutput();
  FixedImageType::Pointer  fixedImage = fixedImageSource->GetOutput();

  // Set up the metric.
  using MetricType = itk::MeanSquaresHistogramImageToImageMetric<FixedImageType, MovingImageType>;
  using TransformBaseType = MetricType::TransformType;
  using ScalesType = MetricType::ScalesType;
  using DerivativeType = MetricType::DerivativeType;
  using ParametersType = TransformBaseType::ParametersType;

  auto metric = MetricType::New();

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
  const unsigned int numberOfParameters = transform->GetNumberOfParameters();

  ParametersType parameters(numberOfParameters);
  for (unsigned int k = 0; k < numberOfParameters; ++k)
  {
    parameters[k] = 0.0;
  }

  // Set scales for derivative calculation.
  ScalesType scales(numberOfParameters);
  for (unsigned int k = 0; k < numberOfParameters; ++k)
  {
    scales[k] = 1;
  }

  constexpr double STEP_LENGTH = 0.001;
  metric->SetDerivativeStepLength(STEP_LENGTH);
  metric->SetDerivativeStepLengthScales(scales);

  try
  {
    // Initialize the metric.
    metric->Initialize();

    // Test SetPaddingValue() and GetPaddingValue().
    metric->SetPaddingValue(-1);
    metric->SetUsePaddingValue(true);

    if (itk::Math::NotExactlyEquals(metric->GetPaddingValue(), -1))
    {
      std::cerr << "Incorrect padding value." << std::endl;
      return EXIT_FAILURE;
    }

    // Check to make sure the returned histogram size is the same as histSize.
    if (histSize != metric->GetHistogramSize())
    {
      std::cout << "Incorrect histogram size." << std::endl;
      return EXIT_FAILURE;
    }

    // Check GetDerivativeStepLength().
    if (itk::Math::NotExactlyEquals(metric->GetDerivativeStepLength(), STEP_LENGTH))
    {
      std::cout << "Incorrect derivative step length." << std::endl;
      return EXIT_FAILURE;
    }

    // Check GetDerivativeStepLengthScales().
    if (metric->GetDerivativeStepLengthScales() != scales)
    {
      std::cout << "Incorrect scales." << std::endl;
      return EXIT_FAILURE;
    }

    // Do some work
    DerivativeType          derivatives(numberOfParameters);
    MetricType::MeasureType value;
    for (double y = -50.0; y <= 50.0; y += 25.0)
    {
      parameters[1] = y;
      for (double x = -50.0; x <= 50.0; x += 25.0)
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


  std::cout << "Exercise the SetLowerBound() and SetUpperBound() methods " << std::endl;

  MetricType::MeasurementVectorType lowerBound;
  lowerBound.Fill(0.0);
  MetricType::MeasurementVectorType upperBound;
  upperBound.Fill(0.0);

  metric->SetLowerBound(lowerBound);
  metric->SetUpperBound(upperBound);

  try
  {
    // Initialize the metric.
    metric->Initialize();

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

  // Force an exception
  try
  {
    ParametersType parameters2(2);
    DerivativeType derivatives2(2);
    ScalesType     badScales(1);
    metric->SetDerivativeStepLengthScales(badScales);
    metric->Initialize();
    metric->GetDerivative(parameters2, derivatives2);
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cerr << "Expected exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_SUCCESS;
  }
  return EXIT_FAILURE;
}
