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

#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"

#include "itkTextOutput.h"

/**
 *  This program test one instantiation of the itk::ImageRegistrationMethod class
 *
 *  This file tests initialization errors.
 */

int
itkImageRegistrationMethodTest(int, char *[])
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  bool pass;

  constexpr unsigned int dimension = 3;

  // Fixed Image Type
  using FixedImageType = itk::Image<float, dimension>;

  // Moving Image Type
  using MovingImageType = itk::Image<char, dimension>;

  // Transform Type
  using TransformType = itk::TranslationTransform<double, dimension>;

  // Optimizer Type
  using OptimizerType = itk::RegularStepGradientDescentOptimizer;

  // Metric Type
  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;

  // Interpolation technique
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  // Registration Method
  using RegistrationType = itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;


  auto metric = MetricType::New();
  auto transform = TransformType::New();
  auto optimizer = OptimizerType::New();
  auto fixedImage = FixedImageType::New();
  auto movingImage = MovingImageType::New();
  auto interpolator = InterpolatorType::New();
  auto registration = RegistrationType::New();

  FixedImageType::SizeType size;
  size.Fill(4); // the size of image have to be at least 4 in each dimension to
                // compute gradient image inside the metric.
  FixedImageType::RegionType region(size);
  fixedImage->SetRegions(region);
  fixedImage->Allocate();
  fixedImage->FillBuffer(3.0);

  movingImage->SetRegions(region);
  movingImage->Allocate();
  movingImage->FillBuffer(4);

  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetTransform(transform);
  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImage);
  registration->SetInterpolator(interpolator);

  // Exercise Get methods
  std::cout << "metric: " << registration->GetMetric() << std::endl;
  std::cout << "optimizer: " << registration->GetOptimizer() << std::endl;
  std::cout << "transform: " << registration->GetTransform() << std::endl;
  std::cout << "fixed image: " << registration->GetFixedImage() << std::endl;
  std::cout << "moving image: " << registration->GetMovingImage() << std::endl;
  std::cout << "interpolator: " << registration->GetInterpolator() << std::endl;

  std::cout << "initial parameters: ";
  std::cout << registration->GetInitialTransformParameters() << std::endl;

  using ParametersType = RegistrationType::ParametersType;
  ParametersType initialParameters(transform->GetNumberOfParameters());
  initialParameters.Fill(0);

  ParametersType badParameters(2);
  badParameters.Fill(5);

  registration->SetInitialTransformParameters(initialParameters);

  std::cout << registration;
  /****************************************************
   * Test out initialization errors
   ****************************************************/

#define TEST_INITIALIZATION_ERROR(ComponentName, badComponent, goodComponent) \
  registration->Set##ComponentName(badComponent);                             \
  try                                                                         \
  {                                                                           \
    pass = false;                                                             \
    registration->Update();                                                   \
  }                                                                           \
  catch (const itk::ExceptionObject & err)                                    \
  {                                                                           \
    std::cout << "Caught expected ExceptionObject" << std::endl;              \
    std::cout << err << std::endl;                                            \
    pass = true;                                                              \
  }                                                                           \
  registration->Set##ComponentName(goodComponent);                            \
                                                                              \
  if (!pass)                                                                  \
  {                                                                           \
    std::cout << "Test failed." << std::endl;                                 \
    return EXIT_FAILURE;                                                      \
  }                                                                           \
  ITK_MACROEND_NOOP_STATEMENT

  TEST_INITIALIZATION_ERROR(InitialTransformParameters, badParameters, initialParameters);
  TEST_INITIALIZATION_ERROR(Metric, nullptr, metric);
  TEST_INITIALIZATION_ERROR(Optimizer, nullptr, optimizer);
  TEST_INITIALIZATION_ERROR(Transform, nullptr, transform);
  TEST_INITIALIZATION_ERROR(FixedImage, nullptr, fixedImage);
  TEST_INITIALIZATION_ERROR(MovingImage, nullptr, movingImage);
  TEST_INITIALIZATION_ERROR(Interpolator, nullptr, interpolator);

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
