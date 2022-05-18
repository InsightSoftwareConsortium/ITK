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
 * Test program for itkMattesMutualInformationImageToImageMetricv4RegistrationTest and
 * GradientDescentOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 */
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkCastImageFilter.h"

#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iomanip>
#include "itkTestingMacros.h"

int
itkMattesMutualInformationImageToImageMetricv4RegistrationTest(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [numberOfIterations = 10] [numberOfDisplacementIterations = 10] ";
    std::cerr << " [doSampling = false] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << argc << std::endl;
  unsigned int numberOfIterations = 10;
  unsigned int numberOfDisplacementIterations = 10;
  bool         doSampling = false;

  if (argc >= 5)
  {
    numberOfIterations = std::stoi(argv[4]);
  }
  if (argc >= 6)
  {
    numberOfDisplacementIterations = std::stoi(argv[5]);
  }
  if (argc >= 7)
  {
    doSampling = std::stoi(argv[6]);
  }

  std::cout << " iterations " << numberOfIterations << " displacementIterations " << numberOfDisplacementIterations
            << std::endl;

  constexpr unsigned int Dimension = 2;
  using PixelType = double; // I assume png is unsigned short

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  // get the images
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

  using DisplacementTransformType = itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<double, Dimension>;
  auto displacementTransform = DisplacementTransformType::New();

  using DisplacementFieldType = DisplacementTransformType::DisplacementFieldType;
  auto field = DisplacementFieldType::New();

  // set the field to be the same as the fixed image region, which will
  // act by default as the virtual domain in this example.
  field->SetRegions(fixedImage->GetLargestPossibleRegion());
  // make sure the field has the same spatial information as the image
  field->CopyInformation(fixedImage);
  std::cout << "fixedImage->GetLargestPossibleRegion(): " << fixedImage->GetLargestPossibleRegion() << std::endl;
  field->Allocate();
  // Fill it with 0's
  DisplacementTransformType::OutputVectorType zeroVector;
  zeroVector.Fill(0);
  field->FillBuffer(zeroVector);
  // Assign to transform
  displacementTransform->SetDisplacementField(field);
  displacementTransform->SetGaussianSmoothingVarianceForTheUpdateField(5);
  displacementTransform->SetGaussianSmoothingVarianceForTheTotalField(6);

  // identity transform for fixed image
  using IdentityTransformType = itk::IdentityTransform<double, Dimension>;
  auto identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  // The metric
  using MetricType = itk::MattesMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType>;
  using PointSetType = MetricType::FixedSampledPointSetType;
  auto metric = MetricType::New();
  metric->SetNumberOfHistogramBins(20);

  if (!doSampling)
  {
    std::cout << "Dense sampling." << std::endl;
    metric->SetUseSampledPointSet(false);
  }
  else
  {
    using PointType = PointSetType::PointType;
    PointSetType::Pointer                             pset(PointSetType::New());
    unsigned long                                     ind = 0, ct = 0;
    itk::ImageRegionIteratorWithIndex<FixedImageType> It(fixedImage, fixedImage->GetLargestPossibleRegion());
    for (It.GoToBegin(); !It.IsAtEnd(); ++It)
    {
      // take every N^th point
      if (ct % 20 == 0)
      {
        PointType pt;
        fixedImage->TransformIndexToPhysicalPoint(It.GetIndex(), pt);
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
  }

  // Assign images and transforms.
  // By not setting a virtual domain image or virtual domain settings,
  // the metric will use the fixed image for the virtual domain.
  //  metric->SetVirtualDomainFromImage( fixedImage );
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

  std::cout << "First do an affine registration " << std::endl;
  using OptimizerType = itk::GradientDescentOptimizerv4;
  auto optimizer = OptimizerType::New();
  optimizer->SetMetric(metric);
  optimizer->SetNumberOfIterations(numberOfIterations);
  optimizer->SetScalesEstimator(shiftScaleEstimator);
  optimizer->StartOptimization();

  std::cout << "Affine registration complete. GetNumberOfSkippedFixedSampledPoints: "
            << metric->GetNumberOfSkippedFixedSampledPoints() << std::endl;
  std::cout << "GetNumberOfWorkUnitsUsed: " << metric->GetNumberOfWorkUnitsUsed() << std::endl;

  // now add the displacement field to the composite transform
  compositeTransform->AddTransform(affineTransform);
  compositeTransform->AddTransform(displacementTransform);
  compositeTransform->SetAllTransformsToOptimizeOn();           // Set back to optimize all.
  compositeTransform->SetOnlyMostRecentTransformToOptimizeOn(); // set to optimize the displacement field

  if (numberOfDisplacementIterations == 0)
  {
    std::cout << "Skipping deformable registration." << std::endl;
  }
  else
  {
    std::cout << "Follow affine with deformable registration " << std::endl;
    metric->SetMovingTransform(compositeTransform);
    metric->SetUseSampledPointSet(doSampling);
    metric->Initialize();

    // Optimizer
    RegistrationParameterScalesFromShiftType::ScalesType displacementScales(
      displacementTransform->GetNumberOfLocalParameters());
    displacementScales.Fill(1);
    optimizer->SetMetric(metric);
    optimizer->SetNumberOfIterations(numberOfDisplacementIterations);
    optimizer->SetScalesEstimator(shiftScaleEstimator);
    try
    {
      optimizer->StartOptimization();
    }
    catch (const itk::ExceptionObject & e)
    {
      std::cout << "Exception thrown ! " << std::endl;
      std::cout << "An error occurred during deformation Optimization:" << std::endl;
      std::cout << e.GetLocation() << std::endl;
      std::cout << e.GetDescription() << std::endl;
      std::cout << e.what() << std::endl;
      std::cout << "Test FAILED." << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << "...finished. " << std::endl;

    std::cout << "GetNumberOfSkippedFixedSampledPoints: " << metric->GetNumberOfSkippedFixedSampledPoints()
              << std::endl;

    // std::cout << "\n\n*gradient: " << optimizer->GetGradient() << std::endl;
    std::cout << "Scales: " << optimizer->GetScales() << std::endl;
    std::cout << "Final learning rate: " << optimizer->GetLearningRate() << std::endl;
  }

  // warp the image with the displacement field
  resample->SetTransform(compositeTransform);
  resample->SetInput(movingImageReader->GetOutput());
  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(0);
  resample->Update();

  // write out the displacement field
  using DisplacementWriterType = itk::ImageFileWriter<DisplacementFieldType>;
  auto        displacementwriter = DisplacementWriterType::New();
  std::string outfilename(argv[3]);
  std::string ext = itksys::SystemTools::GetFilenameExtension(outfilename);
  std::string name = itksys::SystemTools::GetFilenameWithoutExtension(outfilename);
  std::string path = itksys::SystemTools::GetFilenamePath(outfilename);
  std::string defout = path + std::string("/") + name + std::string("_def") + ext;
  displacementwriter->SetFileName(defout.c_str());
  displacementwriter->SetInput(displacementTransform->GetDisplacementField());
  displacementwriter->Update();

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
  writer->Update();

  std::cout << "After optimization affine params are: " << affineTransform->GetParameters() << std::endl;
  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}
