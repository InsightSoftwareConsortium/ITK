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

#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkPointSet.h"
#include "itkPointSetToImageRegistrationMethod.h"
#include "itkImageRegistrationMethodImageSource.h"
#include "itkTestingMacros.h"

#include <iostream>

/**
 *
 *  This program tests the registration of a PointSet against an image.
 *
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 */

int
itkPointSetToImageRegistrationTest(int, char *[])
{
  constexpr unsigned int ImageDimension = 2;

  using PixelType = double;

  using CoordinateRepresentationType = double;

  using MovingImageType = itk::Image<PixelType, ImageDimension>;
  using FixedImageType = itk::Image<PixelType, ImageDimension>;

  using ImageSourceType = itk::testhelper::ImageRegistrationMethodImageSource<PixelType, PixelType, ImageDimension>;

  ImageSourceType::Pointer imageSource = ImageSourceType::New();

  itk::Size<ImageDimension> size;
  size[0] = 100;
  size[1] = 100;

  imageSource->GenerateImages(size);

  // Create the two images
  MovingImageType::ConstPointer movingImage = imageSource->GetMovingImage();
  FixedImageType::ConstPointer  fixedImage = imageSource->GetFixedImage();

  // Create the point set and load it with data by sampling
  // the fixed image.
  //
  using FixedPointSetType = itk::PointSet<float, ImageDimension>;
  FixedPointSetType::Pointer fixedPointSet = FixedPointSetType::New();

  constexpr unsigned int numberOfPoints = 10000;

  fixedPointSet->SetPointData(FixedPointSetType::PointDataContainer::New());

  fixedPointSet->GetPoints()->Reserve(numberOfPoints);
  fixedPointSet->GetPointData()->Reserve(numberOfPoints);

  using ImageIteratorType = itk::ImageRegionConstIterator<FixedImageType>;

  ImageIteratorType it(fixedImage, fixedImage->GetBufferedRegion());

  const unsigned int skip = fixedImage->GetBufferedRegion().GetNumberOfPixels() / numberOfPoints;

  unsigned int counter = 0;

  FixedPointSetType::PointIdentifier pointId = 0;
  FixedPointSetType::PointType       point;

  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    if (counter == 0)
    {
      fixedImage->TransformIndexToPhysicalPoint(it.GetIndex(), point);
      fixedPointSet->SetPoint(pointId, point);
      fixedPointSet->SetPointData(pointId, it.Get());
      ++pointId;
      if (pointId == numberOfPoints)
      {
        break;
      }
      counter = skip;
    }
    --counter;
    ++it;
  }

  // Set up the Metric
  using MetricType = itk::NormalizedCorrelationPointSetToImageMetric<FixedPointSetType, MovingImageType>;

  using TransformBaseType = MetricType::TransformType;
  using ParametersType = TransformBaseType::ParametersType;

  MetricType::Pointer metric = MetricType::New();

  // Set up the Transform
  using TransformType = itk::TranslationTransform<CoordinateRepresentationType, ImageDimension>;
  TransformType::Pointer transform = TransformType::New();

  // Set up the Interpolator
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  // Set up the Optimizer
  using OptimizerType = itk::RegularStepGradientDescentOptimizer;
  OptimizerType::Pointer optimizer = OptimizerType::New();

  // Set up the Registration method
  using RegistrationType = itk::PointSetToImageRegistrationMethod<FixedPointSetType, MovingImageType>;

  RegistrationType::Pointer registration = RegistrationType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(registration, PointSetToImageRegistrationMethod, ProcessObject);

  using CommandIterationType = itk::CommandIterationUpdate<OptimizerType>;

  // Instantiate an Observer to report the progress of the Optimization
  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer(optimizer);

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales(transform->GetNumberOfParameters());
  scales.Fill(1.0);

  unsigned long numberOfIterations = 50;
  double        maximumStepLenght = 1.0; // no step will be larger than this
  double        minimumStepLenght = 0.01;
  double        gradientTolerance = 1e-6; // convergence criterion

  optimizer->SetScales(scales);
  optimizer->SetNumberOfIterations(numberOfIterations);
  optimizer->SetMinimumStepLength(minimumStepLenght);
  optimizer->SetMaximumStepLength(maximumStepLenght);
  optimizer->SetGradientMagnitudeTolerance(gradientTolerance);
  optimizer->MinimizeOn();

  // Connect all the components required for the registration
  registration->SetMetric(metric);
  ITK_TEST_SET_GET_VALUE(metric, registration->GetMetric());

  registration->SetOptimizer(optimizer);
  ITK_TEST_SET_GET_VALUE(optimizer, registration->GetOptimizer());

  registration->SetTransform(transform);
  ITK_TEST_SET_GET_VALUE(transform, registration->GetTransform());

  registration->SetFixedPointSet(fixedPointSet);
  ITK_TEST_SET_GET_VALUE(fixedPointSet, registration->GetFixedPointSet());

  registration->SetMovingImage(movingImage);
  ITK_TEST_SET_GET_VALUE(movingImage, registration->GetMovingImage());

  registration->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, registration->GetInterpolator());

  // Set up transform parameters
  ParametersType parameters(transform->GetNumberOfParameters());

  // Initialize the offset/vector part
  for (unsigned int k = 0; k < parameters.size(); k++)
  {
    parameters[k] = 0.0f;
  }
  transform->SetParameters(parameters);
  registration->SetInitialTransformParameters(transform->GetParameters());
  ITK_TEST_SET_GET_VALUE(transform->GetParameters(), registration->GetInitialTransformParameters());


  ITK_TRY_EXPECT_NO_EXCEPTION(registration->Update());


  // Print the last transform parameters to improve coverage
  //
  ParametersType finalParameters = registration->GetLastTransformParameters();

  const unsigned int numberOfParameters = parameters.Size();

  std::cout << "Last Transform Parameters: " << std::endl;
  for (unsigned int i = 0; i < numberOfParameters; ++i)
  {
    std::cout << finalParameters[i] << std::endl;
  }

  return EXIT_SUCCESS;
}
