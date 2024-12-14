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
#include "itkQuasiNewtonOptimizerv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"
#include "itkRegistrationParameterScalesFromIndexShift.h"
#include "itkRegistrationParameterScalesFromJacobian.h"

#include "itkImageRegistrationMethodImageSource.h"

#include "itkIdentityTransform.h"
#include "itkAffineTransform.h"
#include "itkTranslationTransform.h"
#include "itkTestingMacros.h"

/**
 *  This is a test using GradientDescentOptimizerv4 and parameter scales
 *  estimator. The scales are estimated before the first iteration by
 *  RegistrationParameterScalesFromShift. The learning rates are estimated
 *  at each iteration according to the shift of each step.
 */

template <typename TMovingTransform>
int
itkQuasiNewtonOptimizerv4TestTemplated(int         numberOfIterations,
                                       double      shiftOfStep,
                                       std::string scalesOption,
                                       bool        usePhysicalSpaceForShift = true)
{
  const unsigned int Dimension = TMovingTransform::SpaceDimension;
  using PixelType = double;

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
  using MetricType = itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType, FixedImageType>;
  auto metric = MetricType::New();

  // Assign images and transforms to the metric.
  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);
  metric->SetVirtualDomainFromImage(const_cast<FixedImageType *>(fixedImage.GetPointer()));

  metric->SetFixedTransform(fixedTransform);
  metric->SetMovingTransform(movingTransform);

  // Initialize the metric to prepare for use
  metric->Initialize();

  // Optimizer
  using OptimizerType = itk::QuasiNewtonOptimizerv4;
  auto optimizer = OptimizerType::New();

  optimizer->SetMetric(metric);
  optimizer->SetNumberOfIterations(numberOfIterations);

  // Instantiate an Observer to report the progress of the Optimization
  using CommandIterationType = itk::CommandIterationUpdate<OptimizerType>;
  auto iterationCommand = CommandIterationType::New();
  iterationCommand->SetOptimizer(optimizer);

  // Optimizer parameter scales estimator
  typename itk::OptimizerParameterScalesEstimator::Pointer scalesEstimator;

  using PhysicalShiftScalesEstimatorType = itk::RegistrationParameterScalesFromPhysicalShift<MetricType>;
  using IndexShiftScalesEstimatorType = itk::RegistrationParameterScalesFromIndexShift<MetricType>;
  using JacobianScalesEstimatorType = itk::RegistrationParameterScalesFromJacobian<MetricType>;

  if (scalesOption.compare("shift") == 0)
  {
    if (usePhysicalSpaceForShift)
    {
      std::cout << "Testing RegistrationParameterScalesFrom*Physical*Shift" << '\n';
      auto shiftScalesEstimator = PhysicalShiftScalesEstimatorType::New();
      shiftScalesEstimator->SetMetric(metric);
      shiftScalesEstimator->SetTransformForward(true); // default
      scalesEstimator = shiftScalesEstimator;
    }
    else
    {
      std::cout << "Testing RegistrationParameterScalesFrom*Index*Shift" << '\n';
      auto shiftScalesEstimator = IndexShiftScalesEstimatorType::New();
      shiftScalesEstimator->SetMetric(metric);
      shiftScalesEstimator->SetTransformForward(true); // default
      scalesEstimator = shiftScalesEstimator;
    }
  }
  else
  {
    std::cout << "Testing RegistrationParameterScalesFromJacobian" << '\n';
    auto jacobianScalesEstimator = JacobianScalesEstimatorType::New();
    jacobianScalesEstimator->SetMetric(metric);
    scalesEstimator = jacobianScalesEstimator;
  }

  optimizer->SetScalesEstimator(scalesEstimator);
  // If SetTrustedStepScale is not called, it will use voxel spacing.
  optimizer->SetMaximumStepSizeInPhysicalUnits(shiftOfStep);
  optimizer->SetMaximumNewtonStepSizeInPhysicalUnits(shiftOfStep * 3.0);

  std::cout << "Start optimization..." << '\n' << "Number of iterations: " << numberOfIterations << '\n';

  try
  {
    optimizer->StartOptimization();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception thrown ! " << '\n';
    std::cout << "An error occurred during Optimization:" << '\n';
    std::cout << e.GetLocation() << '\n';
    std::cout << e.GetDescription() << '\n';
    std::cout << e.what() << '\n';
    return EXIT_FAILURE;
  }

  std::cout << "...finished. " << '\n'
            << "StopCondition: " << optimizer->GetStopConditionDescription() << '\n'
            << "Metric: NumberOfValidPoints: " << metric->GetNumberOfValidPoints() << '\n';

  //
  // results
  //
  ParametersType       finalParameters = movingTransform->GetParameters();
  const ParametersType fixedParameters = movingTransform->GetFixedParameters();
  std::cout << "Estimated scales = " << optimizer->GetScales() << '\n';
  std::cout << "finalParameters = " << finalParameters << '\n';
  std::cout << "fixedParameters = " << fixedParameters << '\n';
  bool pass = true;

  ParametersType actualParameters = imageSource->GetActualParameters();
  std::cout << "actualParameters = " << actualParameters << '\n';
  const unsigned int numbeOfParameters = actualParameters.Size();

  // We know that for the Affine transform the Translation parameters are at
  // the end of the list of parameters.
  const unsigned int offsetOrder = finalParameters.Size() - actualParameters.Size();

  constexpr double tolerance = 1.0; // equivalent to 1 pixel.

  for (unsigned int i = 0; i < numbeOfParameters; ++i)
  {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i + offsetOrder] << " == " << -actualParameters[i] << '\n';
    if (itk::Math::abs(finalParameters[i + offsetOrder] - (-actualParameters[i])) > tolerance)
    {
      std::cout << "Tolerance exceeded at component " << i << '\n';
      pass = false;
    }
  }

  if (!pass)
  {
    std::cout << "Test FAILED." << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "Test PASSED." << '\n';
    return EXIT_SUCCESS;
  }
}

int
itkQuasiNewtonOptimizerv4Test(int argc, char ** const argv)
{
  if (argc > 3)
  {
    std::cerr << "Missing Parameters " << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " [numberOfIterations=50 shiftOfStep=1] ";
    std::cerr << '\n';
    return EXIT_FAILURE;
  }
  unsigned int numberOfIterations = 50;
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

  std::cout << '\n' << "Optimizing translation transform with shift scales" << '\n';
  using TranslationTransformType = itk::TranslationTransform<double, Dimension>;
  const int ret1 =
    itkQuasiNewtonOptimizerv4TestTemplated<TranslationTransformType>(numberOfIterations, shiftOfStep, "shift");

  std::cout << '\n' << "Optimizing translation transform with Jacobian scales" << '\n';
  using TranslationTransformType = itk::TranslationTransform<double, Dimension>;
  const int ret2 =
    itkQuasiNewtonOptimizerv4TestTemplated<TranslationTransformType>(numberOfIterations, shiftOfStep, "jacobian");

  if (ret1 == EXIT_SUCCESS && ret2 == EXIT_SUCCESS)
  {
    std::cout << '\n' << "Tests PASSED." << '\n';
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << '\n' << "Tests FAILED." << '\n';
    return EXIT_FAILURE;
  }
}
