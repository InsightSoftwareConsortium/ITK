/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkGradientDescentOptimizer.h"
#include "itkTestingMacros.h"

/**
 *  This program test one instantiation of the itk::ImageRegistrationMethod class
 *
 *  Only types are tested in this file.
 */

int
itkImageRegistrationMethodTest_11(int, char *[])
{

  constexpr unsigned int dimension = 3;

  // Fixed Image Type
  using FixedImageType = itk::Image<float, dimension>;

  // Moving Image Type
  using MovingImageType = itk::Image<char, dimension>;

  // Transform Type
  using TransformType = itk::TranslationTransform<double, dimension>;

  // Optimizer Type
  using OptimizerType = itk::GradientDescentOptimizer;

  // Metric Type
  using MetricType = itk::MeanReciprocalSquareDifferenceImageToImageMetric<FixedImageType, MovingImageType>;

  // Interpolation technique
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  // Registration Method
  using RegistrationType = itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;


  MetricType::Pointer       metric = MetricType::New();
  TransformType::Pointer    transform = TransformType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  FixedImageType::Pointer   fixedImage = FixedImageType::New();
  MovingImageType::Pointer  movingImage = MovingImageType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();


  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetTransform(transform);
  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImage);
  registration->SetInterpolator(interpolator);


  //
  // Now verify that all the sets are consistent with the Gets
  //
  ITK_TEST_SET_GET_VALUE(metric, registration->GetMetric());
  ITK_TEST_SET_GET_VALUE(optimizer, registration->GetOptimizer());
  ITK_TEST_SET_GET_VALUE(transform, registration->GetTransform());
  ITK_TEST_SET_GET_VALUE(fixedImage, registration->GetFixedImage());
  ITK_TEST_SET_GET_VALUE(movingImage, registration->GetMovingImage());
  ITK_TEST_SET_GET_VALUE(interpolator, registration->GetInterpolator());

  //
  // Now verify that they can be changed
  //
  MetricType::Pointer       metric2 = MetricType::New();
  TransformType::Pointer    transform2 = TransformType::New();
  OptimizerType::Pointer    optimizer2 = OptimizerType::New();
  FixedImageType::Pointer   fixedImage2 = FixedImageType::New();
  MovingImageType::Pointer  movingImage2 = MovingImageType::New();
  InterpolatorType::Pointer interpolator2 = InterpolatorType::New();


  registration->SetMetric(metric2);
  registration->SetOptimizer(optimizer2);
  registration->SetTransform(transform2);
  registration->SetFixedImage(fixedImage2);
  registration->SetMovingImage(movingImage2);
  registration->SetInterpolator(interpolator2);


  //
  // Now verify that all the sets are consistent with the Gets
  //
  ITK_TEST_SET_GET_VALUE(metric2, registration->GetMetric());
  ITK_TEST_SET_GET_VALUE(optimizer2, registration->GetOptimizer());
  ITK_TEST_SET_GET_VALUE(transform2, registration->GetTransform());
  ITK_TEST_SET_GET_VALUE(fixedImage2, registration->GetFixedImage());
  ITK_TEST_SET_GET_VALUE(movingImage2, registration->GetMovingImage());
  ITK_TEST_SET_GET_VALUE(interpolator2, registration->GetInterpolator());


  //
  //  Now verify that they can be set to nullptr
  //
  MetricType::Pointer       metric3 = nullptr;
  TransformType::Pointer    transform3 = nullptr;
  OptimizerType::Pointer    optimizer3 = nullptr;
  FixedImageType::Pointer   fixedImage3 = nullptr;
  MovingImageType::Pointer  movingImage3 = nullptr;
  InterpolatorType::Pointer interpolator3 = nullptr;


  registration->SetMetric(metric3);
  registration->SetOptimizer(optimizer3);
  registration->SetTransform(transform3);
  registration->SetFixedImage(fixedImage3);
  registration->SetMovingImage(movingImage3);
  registration->SetInterpolator(interpolator3);


  //
  // Now verify that all the sets are consistent with the Gets
  //
  ITK_TEST_SET_GET_VALUE(metric3, registration->GetMetric());
  ITK_TEST_SET_GET_VALUE(optimizer3, registration->GetOptimizer());
  ITK_TEST_SET_GET_VALUE(transform3, registration->GetTransform());
  ITK_TEST_SET_GET_VALUE(fixedImage3, registration->GetFixedImage());
  ITK_TEST_SET_GET_VALUE(movingImage3, registration->GetMovingImage());
  ITK_TEST_SET_GET_VALUE(interpolator3, registration->GetInterpolator());


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
