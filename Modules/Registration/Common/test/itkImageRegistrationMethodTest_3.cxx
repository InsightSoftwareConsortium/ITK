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

#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkGradientDescentOptimizer.h"

#include "itkImageRegistrationMethodImageSource.h"

/**
 *  This program tests one instantiation of the itk::ImageRegistrationMethod class
 *
 *
 */

int
itkImageRegistrationMethodTest_3(int argc, char * argv[])
{

  bool pass = true;

  constexpr unsigned int dimension = 2;

  // Fixed Image Type
  using FixedImageType = itk::Image<float, dimension>;

  // Moving Image Type
  using MovingImageType = itk::Image<float, dimension>;

  // Size Type
  using SizeType = MovingImageType::SizeType;


  // ImageSource
  using ImageSourceType = itk::testhelper::
    ImageRegistrationMethodImageSource<FixedImageType::PixelType, MovingImageType::PixelType, dimension>;
  // Transform Type
  using TransformType = itk::TranslationTransform<double, dimension>;
  using ParametersType = TransformType::ParametersType;

  // Optimizer Type
  using OptimizerType = itk::GradientDescentOptimizer;

  // Metric Type
  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;

  // Interpolation technique
  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  // Registration Method
  using RegistrationType = itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;

  using CommandIterationType = itk::CommandIterationUpdate<OptimizerType>;


  MetricType::Pointer       metric = MetricType::New();
  TransformType::Pointer    transform = TransformType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();

  ImageSourceType::Pointer imageSource = ImageSourceType::New();

  SizeType size;
  size[0] = 100;
  size[1] = 100;

  imageSource->GenerateImages(size);

  FixedImageType::ConstPointer  fixedImage = imageSource->GetFixedImage();
  MovingImageType::ConstPointer movingImage = imageSource->GetMovingImage();

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
  CommandIterationType::Pointer iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer(optimizer);

  // Scale the translation components of the Transform in the Optimizer
  OptimizerType::ScalesType scales(transform->GetNumberOfParameters());
  scales.Fill(1.0);

  std::cout << "Scales = " << scales << std::endl;


  unsigned long numberOfIterations = 50;
  double        translationScale = 1.0;
  double        learningRate = 1e-2;

  if (argc > 1)
  {
    numberOfIterations = atol(argv[1]);
    std::cout << "numberOfIterations = " << numberOfIterations << std::endl;
  }
  if (argc > 2)
  {
    translationScale = std::stod(argv[2]);
    std::cout << "translationScale = " << translationScale << std::endl;
  }
  if (argc > 3)
  {
    learningRate = std::stod(argv[3]);
    std::cout << "learningRate = " << learningRate << std::endl;
  }

  for (unsigned int i = 0; i < dimension; i++)
  {
    scales[i] = translationScale;
  }
  std::cout << "Before scales " << scales << std::endl;
  optimizer->SetScales(scales);
  optimizer->SetLearningRate(learningRate);
  optimizer->SetNumberOfIterations(numberOfIterations);
  optimizer->MinimizeOn();

  // Start from an Identity transform (in a normal case, the user
  // can probably provide a better guess than the identity...
  transform->SetIdentity();
  std::cout << "SetInitial Transf Parameters " << transform->GetParameters() << std::endl;
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


  constexpr double tolerance = 1.0; // equivalent to 1 pixel.

  for (unsigned int i = 0; i < numbeOfParameters; i++)
  {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i] << " == " << -actualParameters[i] << std::endl;
    if (itk::Math::abs(finalParameters[i] - (-actualParameters[i])) > tolerance)
    {
      std::cout << "Tolerance exceeded at component " << i << std::endl;
      pass = false;
    }
  }


  if (!pass)
  {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}
