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

/**
 * Test program for MeanSquaresImageToImageMetricv4 and
 * LBFGSOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 * A regression test is performed using ctest.
 */

#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkLBFGSOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

#include "itkCastImageFilter.h"

#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

#include <iomanip>

int
itkMeanSquaresImageToImageMetricv4RegistrationTest2(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [gradientTolerance=1e-4] [max function iterations=100] [lineSearchTol=0.9] [stepLength=1.0] "
                 "[trace-debug=false]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  double gTolerance = 1e-4;   // Gradient magnitude tolerance
  int    maxIterations = 100; // Maximum number of iterations
  double lineSearchTol = 0.9; // Line search tolerance
  double stepLength = 1.0;    // Default step length
  bool   trace = false;       // Tracing

  if (argc > 4)
  {
    gTolerance = std::stod(argv[4]);
  }
  if (argc > 5)
  {
    maxIterations = std::stoi(argv[5]);
  }
  if (argc > 6)
  {
    lineSearchTol = std::stod(argv[6]);
  }
  if (argc > 7)
  {
    stepLength = std::stod(argv[7]);
  }
  if (argc > 8)
  {
    trace = static_cast<bool>(std::stoi(argv[8]));
  }

  std::cout << argc << std::endl;
  std::cout << "gTolerance: " << gTolerance << " maxIterations: " << maxIterations
            << " lineSearchTol: " << lineSearchTol << " stepLength: " << stepLength << " trace: " << trace << std::endl;

  /** load the images **/

  constexpr unsigned int Dimension = 2;
  using PixelType = double;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  fixedImageReader->Update();
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  movingImageReader->Update();
  MovingImageType::Pointer movingImage = movingImageReader->GetOutput();

  /** define a resample filter that will ultimately be used to deform the image */
  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  auto resample = ResampleFilterType::New();

  /** create a composite transform holder for other transforms  */
  using CompositeType = itk::CompositeTransform<double, Dimension>;
  auto compositeTransform = CompositeType::New();

  // create an affine transform
  using AffineTransformType = itk::AffineTransform<double, Dimension>;
  auto affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout << " affineTransform params prior to optimization " << affineTransform->GetParameters() << std::endl;

  // identity transform for fixed image
  using IdentityTransformType = itk::IdentityTransform<double, Dimension>;
  auto identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  // the metric
  using MetricType = itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType>;
  using PointSetType = MetricType::FixedSampledPointSetType;
  auto metric = MetricType::New();

  using PointType = PointSetType::PointType;
  PointSetType::Pointer                             pset(PointSetType::New());
  unsigned long                                     ind = 0, ct = 0;
  itk::ImageRegionIteratorWithIndex<FixedImageType> it(fixedImage, fixedImage->GetLargestPossibleRegion());

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    // take every N^th point
    if (true /*ct % 4 == 0*/)
    {
      PointType pt;
      fixedImage->TransformIndexToPhysicalPoint(it.GetIndex(), pt);
      pset->SetPoint(ind, pt);
      ind++;
    }
    ct++;
  }
  std::cout << "Setting point set with " << ind << " points of "
            << fixedImage->GetLargestPossibleRegion().GetNumberOfPixels() << " total " << std::endl;
  metric->SetFixedSampledPointSet(pset);
  metric->SetUseSampledPointSet(true);
  std::cout << "Testing metric with point set..." << std::endl;


  // Assign images and transforms.
  // By not setting a virtual domain image or virtual domain settings,
  // the metric will use the fixed image for the virtual domain.
  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);
  metric->SetFixedTransform(identityTransform);
  metric->SetMovingTransform(affineTransform);
  const bool gaussian = false;
  metric->SetUseMovingImageGradientFilter(gaussian);
  metric->SetUseFixedImageGradientFilter(gaussian);
  metric->Initialize();

  using RegistrationParameterScalesFromShiftType = itk::RegistrationParameterScalesFromPhysicalShift<MetricType>;
  RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator =
    RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  std::cout << "Do an affine registration: " << std::endl;

  // optimizer
  using OptimizerType = itk::LBFGSOptimizerv4;
  auto optimizer = OptimizerType::New();
  optimizer->SetMetric(metric);
  optimizer->SetScalesEstimator(shiftScaleEstimator);

  optimizer->SetTrace(trace);
  optimizer->SetMaximumNumberOfFunctionEvaluations(maxIterations);
  optimizer->SetGradientConvergenceTolerance(gTolerance);
  optimizer->SetLineSearchAccuracy(lineSearchTol);
  optimizer->SetDefaultStepLength(stepLength);
  std::cout << "Initial stop description   = " << optimizer->GetStopConditionDescription() << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(optimizer->StartOptimization());


  std::cout << "Number of work units: metric: " << metric->GetNumberOfWorkUnitsUsed()
            << " optimizer: " << optimizer->GetNumberOfWorkUnits() << std::endl;
  std::cout << "Scales: " << optimizer->GetScales() << " DoEstimateScales: " << optimizer->GetDoEstimateScales()
            << std::endl;
  std::cout << "GetNumberOfSkippedFixedSampledPoints: " << metric->GetNumberOfSkippedFixedSampledPoints() << std::endl;

  // warp the image with the transform
  resample->SetTransform(affineTransform);
  resample->SetInput(movingImageReader->GetOutput());
  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(0);
  resample->Update();

  // write the warped image into a file
  using OutputPixelType = double;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using CastFilterType = itk::CastImageFilter<MovingImageType, OutputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();
  auto caster = CastFilterType::New();
  writer->SetFileName(argv[3]);
  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "After optimization affine params are: " << affineTransform->GetParameters() << std::endl;
  return EXIT_SUCCESS;
}
