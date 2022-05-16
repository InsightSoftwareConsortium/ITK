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
 * Test program for image registration with multiple metric types and
 * QuasiNewtonOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 */
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkQuasiNewtonOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

#include "itkIdentityTransform.h"
#include "itkTranslationTransform.h"
#include "itkAffineTransform.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkCompositeTransform.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkRegistrationParameterScalesFromJacobian.h"

#include "itkCastImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itksys/SystemTools.hxx"
#include "itkResampleImageFilter.h"
#include "itkTestingMacros.h"

template <unsigned int Dimension, typename TAffineTransform>
int
itkQuasiNewtonOptimizerv4RegistrationTestMain(int argc, char * argv[])
{
  std::string  metricString = argv[2];
  unsigned int numberOfIterations = 10;
  unsigned int numberOfDisplacementIterations = 10;

  if (argc >= 7)
  {
    numberOfIterations = std::stoi(argv[6]);
  }
  if (argc >= 8)
  {
    numberOfDisplacementIterations = std::stoi(argv[7]);
  }
  std::cout << " iterations " << numberOfIterations << " displacementIterations " << numberOfDisplacementIterations
            << std::endl;

  using PixelType = double; // I assume png is unsigned short

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[3]);
  movingImageReader->SetFileName(argv[4]);

  // get the images
  fixedImageReader->Update();
  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  movingImageReader->Update();
  typename MovingImageType::Pointer movingImage = movingImageReader->GetOutput();

  /** define a resample filter that will ultimately be used to deform the image */
  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  auto resample = ResampleFilterType::New();


  /** create a composite transform holder for other transforms  */
  using CompositeType = itk::CompositeTransform<double, Dimension>;

  auto compositeTransform = CompositeType::New();

  // create an affine transform
  using AffineTransformType = TAffineTransform;

  auto affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout << " affineTransform params " << affineTransform->GetParameters() << std::endl;
  using DisplacementTransformType = itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<double, Dimension>;
  auto displacementTransform = DisplacementTransformType::New();
  using DisplacementFieldType = typename DisplacementTransformType::DisplacementFieldType;
  auto field = DisplacementFieldType::New();

  // set the field to be the same as the fixed image region, which will
  // act by default as the virtual domain in this example.
  field->SetRegions(fixedImage->GetLargestPossibleRegion());
  // make sure the field has the same spatial information as the image
  field->CopyInformation(fixedImage);
  std::cout << "fixedImage->GetLargestPossibleRegion(): " << fixedImage->GetLargestPossibleRegion() << std::endl
            << "fixedImage->GetBufferedRegion(): " << fixedImage->GetBufferedRegion() << std::endl;
  field->Allocate();
  // Fill it with 0's
  typename DisplacementTransformType::OutputVectorType zeroVector;
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
  using MetricBaseType = itk::ImageToImageMetricv4<FixedImageType, MovingImageType>;
  typename MetricBaseType::Pointer metric;

  if (metricString.compare("ms") == 0)
  {
    using MeanSquaresMetricType = itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType>;
    auto meanSquaresMetric = MeanSquaresMetricType::New();
    metric = meanSquaresMetric.GetPointer();
  }
  else if (metricString.compare("mi") == 0)
  {
    using MIMetricType = itk::JointHistogramMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType>;
    using PointSetType = typename MIMetricType::FixedSampledPointSetType;
    auto miMetric = MIMetricType::New();
    metric = miMetric.GetPointer();

    miMetric->SetNumberOfHistogramBins(20);
    using PointType = typename PointSetType::PointType;
    typename PointSetType::Pointer                    pset(PointSetType::New());
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
    miMetric->SetFixedSampledPointSet(pset);
    miMetric->SetUseSampledPointSet(true);
    std::cout << "Testing metric with point set..." << std::endl;
  }
  else if (metricString.compare("anc") == 0)
  {
    // The metric
    using ANCMetricType = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType>;
    auto nbcMetric = ANCMetricType::New();
    metric = nbcMetric.GetPointer();

    itk::Size<Dimension> radSize;
    radSize.Fill(2);
    nbcMetric->SetRadius(radSize);
  }
  else
  {
    std::cerr << "The given metric type is not supported: " << metricString << std::endl;
    std::cerr << "The supported metric types are: " << std::endl;
    std::cerr << "   ms   - MeanSquaresImageToImageMetricv4" << std::endl;
    std::cerr << "   mi   - JointHistogramMutualInformationImageToImageMetricv4" << std::endl;
    std::cerr << "   anc  - ANTSNeighborhoodCorrelationImageToImageMetricv4" << std::endl;
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  // Assign images and transforms.
  // By not setting a virtual domain image or virtual domain settings,
  // the metric will use the fixed image for the virtual domain.
  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);
  metric->SetFixedTransform(identityTransform);
  metric->SetMovingTransform(affineTransform);
  bool gaussian = false;
  metric->SetUseMovingImageGradientFilter(gaussian);
  metric->SetUseFixedImageGradientFilter(gaussian);
  metric->Initialize();

  using RegistrationParameterScalesFromShiftType = itk::RegistrationParameterScalesFromPhysicalShift<MetricBaseType>;
  typename RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator =
    RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  std::cout << "First do an affine registration " << std::endl;

  using OptimizerType = itk::QuasiNewtonOptimizerv4;
  auto optimizer = OptimizerType::New();
  optimizer->SetMetric(metric);
  optimizer->SetNumberOfIterations(numberOfIterations);
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

  std::cout << "Follow affine with deformable registration " << std::endl;
  // now add the displacement field to the composite transform
  compositeTransform->AddTransform(affineTransform);
  compositeTransform->AddTransform(displacementTransform);
  compositeTransform->SetAllTransformsToOptimizeOn();           // Set back to optimize all.
  compositeTransform->SetOnlyMostRecentTransformToOptimizeOn(); // set to optimize the displacement field
  metric->SetMovingTransform(compositeTransform);
  metric->SetUseSampledPointSet(false);
  metric->Initialize();

  // Optimizer
  typename RegistrationParameterScalesFromShiftType::ScalesType displacementScales(
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
  std::string outfilename(argv[5]);
  std::string ext = itksys::SystemTools::GetFilenameExtension(outfilename);
  std::string name = itksys::SystemTools::GetFilenameWithoutExtension(outfilename);
  std::string defout = name + std::string("_def") + ext;
  displacementwriter->SetFileName(defout.c_str());
  displacementwriter->SetInput(displacementTransform->GetDisplacementField());
  displacementwriter->Update();

  // write the warped image into a file
  // using OutputPixelType = double;
  using OutputPixelType = unsigned short;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using CastFilterType = itk::CastImageFilter<MovingImageType, OutputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  auto caster = CastFilterType::New();
  writer->SetFileName(argv[5]);
  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  std::cout << "Test PASSED." << affineTransform->GetParameters() << std::endl;
  return EXIT_SUCCESS;
}

int
itkQuasiNewtonOptimizerv4RegistrationTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " dimension";
    std::cerr << " metric-type{ms|mi|anc}";
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [numberOfIterations numberOfDisplacementIterations] ";
    std::cerr << std::endl;
    std::cerr << " The metric types are: " << std::endl;
    std::cerr << "   ms   - MeanSquaresImageToImageMetricv4" << std::endl;
    std::cerr << "   mi   - JointHistogramMutualInformationImageToImageMetricv4" << std::endl;
    std::cerr << "   anc  - ANTSNeighborhoodCorrelationImageToImageMetricv4" << std::endl;
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int Dimension = std::stoi(argv[1]);

  if (Dimension == 2)
  {
    using AffineTransformType = itk::AffineTransform<double, 2>;
    // using AffineTransformType = itk::Euler2DTransform<double>;
    return itkQuasiNewtonOptimizerv4RegistrationTestMain<2, AffineTransformType>(argc, argv);
  }
  else if (Dimension == 3)
  {
    using AffineTransformType = itk::AffineTransform<double, 3>;
    // using AffineTransformType = itk::Euler3DTransform<double>;
    return itkQuasiNewtonOptimizerv4RegistrationTestMain<3, AffineTransformType>(argc, argv);
  }
  else
  {
    std::cerr << "Dimension not supported: " << Dimension << std::endl;
    std::cerr << "Dimension supported: 2 3" << std::endl;
    return EXIT_FAILURE;
  }
}
