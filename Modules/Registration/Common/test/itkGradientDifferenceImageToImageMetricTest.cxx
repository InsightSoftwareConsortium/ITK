/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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


int itkGradientDifferenceImageToImageMetricTest(int , char*[] )
{
  // Create two simple images.
  const unsigned int ImageDimension = 2;
  typedef double PixelType;
  typedef double CoordinateRepresentationType;

  //Allocate Images
  typedef itk::Image<PixelType,ImageDimension> MovingImageType;
  typedef itk::Image<PixelType,ImageDimension> FixedImageType;

  // Declare Gaussian Sources
  typedef itk::GaussianImageSource<MovingImageType> MovingImageSourceType;
  typedef itk::GaussianImageSource<FixedImageType>  FixedImageSourceType;

  // Note: the following declarations are classical arrays
  FixedImageType::SizeValueType fixedImageSize[] = {100,  100};
  MovingImageType::SizeValueType movingImageSize[] = {100,  100};

  FixedImageType::SpacingValueType fixedImageSpacing[]  = {1.0f, 1.0f};
  MovingImageType::SpacingValueType movingImageSpacing[] = {1.0f, 1.0f};

  FixedImageType::PointValueType fixedImageOrigin[] = {0.0f, 0.0f};
  MovingImageType::PointValueType movingImageOrigin[] = {0.0f, 0.0f};

  MovingImageSourceType::Pointer movingImageSource =
    MovingImageSourceType::New();
  FixedImageSourceType::Pointer  fixedImageSource  =
    FixedImageSourceType::New();

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
  FixedImageType::Pointer  fixedImage  = fixedImageSource->GetOutput();

  // Set up the metric.
  typedef itk::GradientDifferenceImageToImageMetric<
                                            FixedImageType,
                                            MovingImageType> MetricType;

  typedef MetricType::TransformType         TransformBaseType;
  typedef MetricType::DerivativeType        DerivativeType;
  typedef TransformBaseType::ParametersType ParametersType;

  MetricType::Pointer metric = MetricType::New();

  // Plug the images into the metric.
  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);

  // Set up a transform.
  typedef itk::TranslationTransform<CoordinateRepresentationType,
    ImageDimension> TransformType;

  TransformType::Pointer transform = TransformType::New();
  metric->SetTransform(transform.GetPointer());

  // Set up an interpolator.
  typedef itk::LinearInterpolateImageFunction<MovingImageType,
    double> InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  interpolator->SetInputImage(movingImage.GetPointer());
  metric->SetInterpolator(interpolator.GetPointer());

  // Define the region over which the metric will be computed.
  metric->SetFixedImageRegion(fixedImage->GetBufferedRegion());

  // Set up transform parameters.
  const unsigned int numberOfParameters = transform->GetNumberOfParameters();

  ParametersType parameters( numberOfParameters );
  for (unsigned int k = 0; k < numberOfParameters; k++)
    {
    parameters[k] = 0.0;
    }


  try
    {
    // Initialize the metric.
    metric->Initialize();

    // Do some work
    DerivativeType derivatives( numberOfParameters );
    MetricType::MeasureType value;
    for (double y = -10.0; y <= 10.0; y += 5.0)
      {
      parameters[1] = y;
      for (double x = -10.0; x <= 10.0; x += 5.0)
        {
        parameters[0] = x;
        metric->GetValueAndDerivative (parameters, value, derivatives);
        std::cout << "Parameters: " << parameters
                  << ", Value: " << value
                  << ", Derivatives: " << derivatives << std::endl;
        }
      }

    // Exercise Print() method.
    metric->Print(std::cout);

    std::cout << "Test passed." << std::endl;
    }
  catch (itk::ExceptionObject& ex)
    {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
