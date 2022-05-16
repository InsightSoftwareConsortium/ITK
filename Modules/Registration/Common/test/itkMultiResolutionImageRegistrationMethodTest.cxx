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

#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"

#include "itkTextOutput.h"
#include "itkTestingMacros.h"


/**
 *  This program test one instantiation of the
 *  itk::MultiResolutionImageRegistrationMethod class
 *
 *  This file tests initialization errors.
 */

int
itkMultiResolutionImageRegistrationMethodTest(int, char *[])
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  bool pass;

  constexpr unsigned int dimension = 3;

  // Fixed Image Type
  using FixedImageType = itk::Image<float, dimension>;

  // Moving Image Type
  //  using MovingImageType = itk::Image<char,dimension>;
  using MovingImageType = itk::Image<float, dimension>;

  // Transform Type
  using TransformType = itk::TranslationTransform<double, dimension>;

  // Optimizer Type
  using OptimizerType = itk::RegularStepGradientDescentOptimizer;

  // Metric Type
  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;

  // Interpolation technique
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  // Fixed Image Pyramid Type
  using FixedImagePyramidType = itk::RecursiveMultiResolutionPyramidImageFilter<FixedImageType, FixedImageType>;

  // Moving Image Pyramid Type
  using MovingImagePyramidType = itk::RecursiveMultiResolutionPyramidImageFilter<MovingImageType, MovingImageType>;


  // Registration Method
  using RegistrationType = itk::MultiResolutionImageRegistrationMethod<FixedImageType, MovingImageType>;


  auto metric = MetricType::New();
  auto transform = TransformType::New();
  auto optimizer = OptimizerType::New();
  auto fixedImage = FixedImageType::New();
  auto movingImage = MovingImageType::New();
  auto interpolator = InterpolatorType::New();
  auto fixedImagePyramid = FixedImagePyramidType::New();
  auto movingImagePyramid = MovingImagePyramidType::New();

  auto registration = RegistrationType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(registration, MultiResolutionImageRegistrationMethod, ProcessObject);


  FixedImageType::SizeType size;
  size.Fill(8);

  FixedImageType::RegionType region(size);
  fixedImage->SetRegions(region);
  fixedImage->Allocate();
  fixedImage->FillBuffer(3.0);

  movingImage->SetRegions(region);
  movingImage->Allocate();
  movingImage->FillBuffer(4.0);

  registration->SetMetric(metric);
  ITK_TEST_SET_GET_VALUE(metric, registration->GetMetric());

  registration->SetOptimizer(optimizer);
  ITK_TEST_SET_GET_VALUE(optimizer, registration->GetOptimizer());

  registration->SetTransform(transform);
  ITK_TEST_SET_GET_VALUE(transform, registration->GetTransform());

  registration->SetFixedImage(fixedImage);
  ITK_TEST_SET_GET_VALUE(fixedImage, registration->GetFixedImage());

  registration->SetMovingImage(movingImage);
  ITK_TEST_SET_GET_VALUE(movingImage, registration->GetMovingImage());

  registration->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, registration->GetInterpolator());

  registration->SetFixedImagePyramid(fixedImagePyramid);
  ITK_TEST_SET_GET_VALUE(fixedImagePyramid, registration->GetFixedImagePyramid());

  registration->SetMovingImagePyramid(movingImagePyramid);
  ITK_TEST_SET_GET_VALUE(movingImagePyramid, registration->GetMovingImagePyramid());

  registration->SetFixedImageRegion(fixedImage->GetBufferedRegion());
  ITK_TEST_SET_GET_VALUE(fixedImage->GetBufferedRegion(), registration->GetFixedImageRegion());

  itk::SizeValueType numberOfLevels = 2;
  registration->SetNumberOfLevels(numberOfLevels);
  ITK_TEST_SET_GET_VALUE(numberOfLevels, registration->GetNumberOfLevels());

  using ParametersType = RegistrationType::ParametersType;
  ParametersType initialParameters(transform->GetNumberOfParameters());
  initialParameters.Fill(0);
  registration->SetInitialTransformParameters(initialParameters);
  ITK_TEST_SET_GET_VALUE(initialParameters, registration->GetInitialTransformParameters());

  typename ParametersType::ValueType initialTransformParametersOfNextLevelVal(0.0);
  ParametersType                     initialTransformParametersOfNextLevel(1, initialTransformParametersOfNextLevelVal);
  registration->SetInitialTransformParametersOfNextLevel(initialTransformParametersOfNextLevel);
  ITK_TEST_SET_GET_VALUE(initialTransformParametersOfNextLevel,
                         registration->GetInitialTransformParametersOfNextLevel());

  // Exercise Get methods
  std::cout << "CurrentLevel: " << registration->GetCurrentLevel() << std::endl;


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

  TEST_INITIALIZATION_ERROR(Metric, nullptr, metric);
  TEST_INITIALIZATION_ERROR(Optimizer, nullptr, optimizer);
  TEST_INITIALIZATION_ERROR(Transform, nullptr, transform);
  TEST_INITIALIZATION_ERROR(FixedImage, nullptr, fixedImage);
  TEST_INITIALIZATION_ERROR(MovingImage, nullptr, movingImage);
  TEST_INITIALIZATION_ERROR(Interpolator, nullptr, interpolator);
  TEST_INITIALIZATION_ERROR(FixedImagePyramid, nullptr, fixedImagePyramid);
  TEST_INITIALIZATION_ERROR(MovingImagePyramid, nullptr, movingImagePyramid);
  TEST_INITIALIZATION_ERROR(InitialTransformParameters, ParametersType(1), initialParameters);


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
