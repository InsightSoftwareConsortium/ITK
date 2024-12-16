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
#include "itkLinearInterpolateImageFunction.h"
#include "itkGradientDescentOptimizer.h"
#include "itkMeanSquaresImageToImageMetric.h"

#include "itkImageRegistrationMethodImageSource.h"

/**
 *  This program tests one instantiation of the itk::ImageRegistrationMethod class
 *
 *
 */


template <typename DataType>
bool
DoRegistration()
{

  bool pass = true;

  constexpr unsigned int dimension = 2;

  // Fixed Image Type
  using FixedImageType = itk::Image<DataType, dimension>;

  // Moving Image Type
  using MovingImageType = itk::Image<DataType, dimension>;

  // Size Type
  using SizeType = typename MovingImageType::SizeType;

  // Transform Type
  using TransformType = itk::AffineTransform<double, dimension>;
  using ParametersType = typename TransformType::ParametersType;

  using FixedImagePixelType = typename FixedImageType::PixelType;
  using MovingImagePixelType = typename MovingImageType::PixelType;

  // ImageSource
  using ImageSourceType =
    itk::testhelper::ImageRegistrationMethodImageSource<FixedImagePixelType, MovingImagePixelType, dimension>;
  // Transform Type
  using TransformType = itk::AffineTransform<double, dimension>;
  using ParametersType = typename TransformType::ParametersType;

  // Optimizer Type
  using OptimizerType = itk::GradientDescentOptimizer;

  // Metric Type
  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;

  // Interpolation technique
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  // Registration Method
  using RegistrationType = itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;

  using CommandIterationType = itk::CommandIterationUpdate<OptimizerType>;


  auto metric = MetricType::New();
  auto transform = TransformType::New();
  auto optimizer = OptimizerType::New();
  auto interpolator = InterpolatorType::New();
  auto registration = RegistrationType::New();

  auto imageSource = ImageSourceType::New();

  SizeType size;
  size[0] = 100;
  size[1] = 100;

  imageSource->GenerateImages(size);

  const typename FixedImageType::ConstPointer  fixedImage = imageSource->GetFixedImage();
  const typename MovingImageType::ConstPointer movingImage = imageSource->GetMovingImage();

  //
  // Connect all the components required for Registratio
  //
  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetTransform(transform);
  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImage);
  registration->SetInterpolator(interpolator);


  // Select the Region of Interest over which the Metric will be computed
  // Registration time will be proportional to the number of pixels in this region.
  metric->SetFixedImageRegion(fixedImage->GetBufferedRegion());

  // Instantiate an Observer to report the progress of the Optimization
  auto iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer(optimizer);

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales(transform->GetNumberOfParameters());
  scales.Fill(1.0);


  constexpr unsigned long numberOfIterations = 100;
  constexpr double        translationScale = 1e-6;
  constexpr double        learningRate = 1e-8;

  for (unsigned int i = 0; i < dimension; ++i)
  {
    scales[i + dimension * dimension] = translationScale;
  }

  optimizer->SetScales(scales);
  optimizer->SetLearningRate(learningRate);
  optimizer->SetNumberOfIterations(numberOfIterations);
  optimizer->MinimizeOn();

  // Start from an Identity transform (in a normal case, the user
  // can probably provide a better guess than the identity...
  transform->SetIdentity();
  registration->SetInitialTransformParameters(transform->GetParameters());

  // Initialize the internal connections of the registration method.
  // This can potentially throw an exception
  try
  {
    registration->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << e << std::endl;
    pass = false;
  }

  ParametersType actualParameters = imageSource->GetActualParameters();
  ParametersType finalParameters = registration->GetLastTransformParameters();

  const unsigned int numbeOfParameters = actualParameters.Size();

  // We know that for the Affine transform the Translation parameters are at
  // the end of the list of parameters.
  const unsigned int offsetOrder = finalParameters.Size() - actualParameters.Size();
  constexpr double   tolerance = 1.0; // equivalent to 1 pixel.

  for (unsigned int i = 0; i < numbeOfParameters; ++i)
  {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i + offsetOrder] << " == " << -actualParameters[i] << std::endl;
    if (itk::Math::abs(finalParameters[i + offsetOrder] - (-actualParameters[i])) > tolerance)
    {
      std::cout << "Tolerance exceeded at component " << i << std::endl;
      pass = false;
    }
  }

  return pass;
}
int
itkImageRegistrationMethodTest_16(int itkNotUsed(argc), char *[] itkNotUsed(argv))
{
  const bool result_uc = DoRegistration<unsigned char>();
  const bool result_c = DoRegistration<char>();
  const bool result_us = DoRegistration<unsigned short>();
  const bool result_s = DoRegistration<short>();
  const bool result_ui = DoRegistration<unsigned int>();
  const bool result_i = DoRegistration<int>();
  const bool result_ul = DoRegistration<unsigned long>();
  const bool result_l = DoRegistration<long>();
  const bool result_f = DoRegistration<float>();
  const bool result_d = DoRegistration<double>();

  std::cout << "<unsigned char>:  " << result_uc << std::endl;
  std::cout << "<char>:           " << result_c << std::endl;
  std::cout << "<unsigned short>: " << result_us << std::endl;
  std::cout << "<short>:          " << result_s << std::endl;
  std::cout << "<unsigned int>:   " << result_ui << std::endl;
  std::cout << "<int>:            " << result_i << std::endl;
  std::cout << "<unsigned long>:  " << result_ul << std::endl;
  std::cout << "<long>:           " << result_l << std::endl;
  std::cout << "<float>:          " << result_f << std::endl;
  std::cout << "<double>:         " << result_d << std::endl;

  return EXIT_SUCCESS;
}
