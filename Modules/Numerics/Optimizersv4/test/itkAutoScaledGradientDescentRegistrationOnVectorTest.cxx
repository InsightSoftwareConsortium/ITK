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
#include "itkGradientDescentOptimizerv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkRegistrationParameterScalesFromJacobian.h"

#include "itkSize.h"
#include "itkImageRegistrationMethodImageSource.h"
#include "itkVectorImageToImageMetricTraitsv4.h"
#include "itkTestingMacros.h"

/**
 *  This is a test using GradientDescentOptimizerv4 and parameter scales
 *  estimator. The scales are estimated before the first iteration by
 *  RegistrationParameterScalesFromPhysicalShift. The learning rates are estimated
 *  at each iteration according to the shift of each step.
 */

template <typename TMovingTransform>
int
itkAutoScaledGradientDescentRegistrationOnVectorTestTemplated(int         numberOfIterations,
                                                              double      shiftOfStep,
                                                              std::string scalesOption)
{
  const unsigned int Dimension = TMovingTransform::SpaceDimension;

  using PixelType = itk::Vector<float, 2>;

  // Fixed Image Type
  using FixedImageType = itk::Image<PixelType, Dimension>;

  // Moving Image Type
  using MovingImageType = itk::Image<PixelType, Dimension>;

  // Size Type
  using SizeType = typename MovingImageType::SizeType;

  // ImageSource
  using ImageSourceType =
    typename itk::testhelper::ImageRegistrationMethodImageSource<typename FixedImageType::PixelType,
                                                                 typename MovingImageType::PixelType,
                                                                 Dimension>;

  typename FixedImageType::ConstPointer  fixedImage;
  typename MovingImageType::ConstPointer movingImage;
  typename ImageSourceType::Pointer      imageSource;

  imageSource = ImageSourceType::New();

  SizeType size;
  size[0] = 100;
  size[1] = 100;

  imageSource->GenerateImages(size);

  fixedImage = imageSource->GetFixedImage();
  movingImage = imageSource->GetMovingImage();

  // Transform for the moving image
  using MovingTransformType = TMovingTransform;
  auto movingTransform = MovingTransformType::New();
  movingTransform->SetIdentity();

  // Transform for the fixed image
  using FixedTransformType = itk::IdentityTransform<double, Dimension>;
  auto fixedTransform = FixedTransformType::New();
  fixedTransform->SetIdentity();

  // ParametersType for the moving transform
  using ParametersType = typename MovingTransformType::ParametersType;

  // Metric
  using MetricTraitsType = itk::
    VectorImageToImageMetricTraitsv4<FixedImageType, MovingImageType, FixedImageType, PixelType::Dimension, double>;
  using MetricType =
    itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType, FixedImageType, double, MetricTraitsType>;
  auto metric = MetricType::New();

  // Assign images and transforms to the metric.
  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);

  metric->SetFixedTransform(fixedTransform);
  metric->SetMovingTransform(movingTransform);

  // Initialize the metric to prepare for use
  metric->Initialize();

  // Optimizer
  using OptimizerType = itk::GradientDescentOptimizerv4;
  auto optimizer = OptimizerType::New();

  optimizer->SetMetric(metric);
  optimizer->SetNumberOfIterations(numberOfIterations);

  // Instantiate an Observer to report the progress of the Optimization
  using CommandIterationType = itk::CommandIterationUpdate<OptimizerType>;
  auto iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer(optimizer);

  // Optimizer parameter scales estimator
  typename itk::OptimizerParameterScalesEstimator::Pointer scalesEstimator;

  using ShiftScalesEstimatorType = itk::RegistrationParameterScalesFromPhysicalShift<MetricType>;
  using JacobianScalesEstimatorType = itk::RegistrationParameterScalesFromJacobian<MetricType>;

  if (scalesOption.compare("shift") == 0)
  {
    std::cout << "Testing RegistrationParameterScalesFromPhysicalShift" << std::endl;
    auto shiftScalesEstimator = ShiftScalesEstimatorType::New();
    shiftScalesEstimator->SetMetric(metric);
    shiftScalesEstimator->SetTransformForward(true); // default
    scalesEstimator = shiftScalesEstimator;
  }
  else
  {
    std::cout << "Testing RegistrationParameterScalesFromJacobian" << std::endl;
    auto jacobianScalesEstimator = JacobianScalesEstimatorType::New();
    jacobianScalesEstimator->SetMetric(metric);
    jacobianScalesEstimator->SetTransformForward(true); // default
    scalesEstimator = jacobianScalesEstimator;
  }

  optimizer->SetScalesEstimator(scalesEstimator);
  // If SetTrustedStepScale is not called, it will use voxel spacing.
  optimizer->SetMaximumStepSizeInPhysicalUnits(shiftOfStep);

  std::cout << "Start optimization..." << std::endl << "Number of iterations: " << numberOfIterations << std::endl;

  try
  {
    optimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during Optimization:" << std::endl;
    std::cout << e.GetLocation() << std::endl;
    std::cout << e.GetDescription() << std::endl;
    std::cout << e.what() << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "...finished. " << std::endl
            << "StopCondition: " << optimizer->GetStopConditionDescription() << std::endl
            << "Metric: NumberOfValidPoints: " << metric->GetNumberOfValidPoints() << std::endl;

  //
  // results
  //
  ParametersType finalParameters = movingTransform->GetParameters();
  ParametersType fixedParameters = movingTransform->GetFixedParameters();
  std::cout << "Estimated scales = " << optimizer->GetScales() << std::endl;
  std::cout << "finalParameters = " << finalParameters << std::endl;
  std::cout << "fixedParameters = " << fixedParameters << std::endl;
  bool pass = true;

  ParametersType actualParameters = imageSource->GetActualParameters();
  std::cout << "actualParameters = " << actualParameters << std::endl;
  const unsigned int numbeOfParameters = actualParameters.Size();

  // We know that for the Affine transform the Translation parameters are at
  // the end of the list of parameters.
  const unsigned int offsetOrder = finalParameters.Size() - actualParameters.Size();

  constexpr double tolerance = 1.0; // equivalent to 1 pixel.

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

  if (!pass)
  {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "Test PASSED." << std::endl;
    return EXIT_SUCCESS;
  }
}

int
itkAutoScaledGradientDescentRegistrationOnVectorTest(int argc, char ** const argv)
{
  if (argc > 3)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " [numberOfIterations=30 shiftOfStep=1.0] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  unsigned int numberOfIterations = 30;
  double       shiftOfStep = 1.0;

  if (argc >= 2)
  {
    numberOfIterations = std::stoi(argv[1]);
  }
  if (argc >= 3)
  {
    shiftOfStep = std::stod(argv[2]);
  }

  constexpr unsigned int Dimension = 2;

  std::cout << std::endl << "Optimizing translation transform with shift scales" << std::endl;
  using TranslationTransformType = itk::TranslationTransform<double, Dimension>;
  int ret1 = itkAutoScaledGradientDescentRegistrationOnVectorTestTemplated<TranslationTransformType>(
    numberOfIterations, shiftOfStep, "shift");

  std::cout << std::endl << "Optimizing translation transform with Jacobian scales" << std::endl;
  using TranslationTransformType = itk::TranslationTransform<double, Dimension>;
  int ret2 = itkAutoScaledGradientDescentRegistrationOnVectorTestTemplated<TranslationTransformType>(
    numberOfIterations, 0.0, "jacobian");

  if (ret1 == EXIT_SUCCESS && ret2 == EXIT_SUCCESS)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
