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
 * Test program for JointHistogramMutualInformationImageToImageMetricv4 and
 * GradientDescentOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 */
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkCastImageFilter.h"

#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iomanip>
#include "itkTestingMacros.h"

namespace
{

template <typename TOptimizer, typename TMIMetric>
/** \class JointPDFStatus
 * \brief Save the JointPDF from the metric to an image file and check to make sure
 * they are normalized properly. */
class JointPDFStatus : public itk::Command
{
public:
  using Self = JointPDFStatus;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;

  itkSimpleNewMacro(Self); // Clone, and CreateAnother are not needed here.

  using OptimizerType = TOptimizer;
  using MIMetricType = TMIMetric;

  void
  SetMIMetric(const MIMetricType * metric)
  {
    this->m_MIMetric = metric;
  }

  void
  SetOutputFileNameBase(const char * filename)
  {
    this->m_OutputFileNameBase = filename;
  }

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    const auto * optimizer = dynamic_cast<const OptimizerType *>(object);
    if (optimizer == nullptr)
    {
      return;
    }
    if (!(itk::IterationEvent().CheckEvent(&event)))
    {
      return;
    }
    std::cout << "Current optimizer iteration: " << optimizer->GetCurrentIteration() << "\n";
    std::cout << "Current optimizer value:     " << optimizer->GetCurrentMetricValue() << "\n";

    std::string        ext = itksys::SystemTools::GetFilenameExtension(this->m_OutputFileNameBase);
    std::string        name = itksys::SystemTools::GetFilenameWithoutExtension(this->m_OutputFileNameBase);
    std::string        path = itksys::SystemTools::GetFilenamePath(this->m_OutputFileNameBase);
    std::ostringstream ostrm;
    ostrm << name << "_jointpdf_" << this->m_Count << ext;
    std::cout << "Writing joint pdf to:        " << ostrm.str() << std::endl;
    ostrm.str("");
    ostrm << path << "/" << name << "_jointpdf_" << this->m_Count << ext;
    this->m_Writer->SetFileName(ostrm.str());

    using JointPDFType = typename MIMetricType::JointPDFType;
    const JointPDFType * jointPDF = this->m_MIMetric->GetJointPDF();
    this->m_Writer->SetInput(jointPDF);
    this->m_Writer->Update();

    // Check for correct normalization.
    using IteratorType = itk::ImageRegionConstIterator<JointPDFType>;
    IteratorType it(jointPDF, jointPDF->GetBufferedRegion());
    double       sum = 0.0;
    for (it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
      sum += it.Get();
    }
    std::cout << "The PDF sum is               " << std::setprecision(20) << sum << std::endl;

    ++this->m_Count;
  }

protected:
  JointPDFStatus()
    : m_MIMetric(nullptr)

  {
    this->m_Writer = WriterType::New();
  }

private:
  const MIMetricType * m_MIMetric;

  unsigned int m_Count{ 0 };
  std::string  m_OutputFileNameBase;

  using WriterType = typename itk::ImageFileWriter<typename MIMetricType::JointPDFType>;
  typename WriterType::Pointer m_Writer;
};
} // namespace

int
itkJointHistogramMutualInformationImageToImageRegistrationTest(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [numberOfIterations numberOfDisplacementIterations] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << argc << std::endl;
  unsigned int numberOfIterations = 10;
  unsigned int numberOfDisplacementIterations = 10;
  if (argc >= 5)
  {
    numberOfIterations = std::stoi(argv[4]);
  }
  if (argc >= 6)
  {
    numberOfDisplacementIterations = std::stoi(argv[5]);
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
  using MetricType = itk::JointHistogramMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType>;
  using PointSetType = MetricType::FixedSampledPointSetType;
  auto metric = MetricType::New();
  metric->SetNumberOfHistogramBins(20);

  using PointType = PointSetType::PointType;
  PointSetType::Pointer                             pset(PointSetType::New());
  unsigned long                                     ind = 0, ct = 0;
  itk::ImageRegionIteratorWithIndex<FixedImageType> It(fixedImage, fixedImage->GetLargestPossibleRegion());
  for (It.GoToBegin(); !It.IsAtEnd(); ++It)
  {
    // take every N^th point
    if (ct % 20 == 0) // about a factor of 5 speed-up over dense
    {
      PointType pt;
      fixedImage->TransformIndexToPhysicalPoint(It.GetIndex(), pt);
      pset->SetPoint(ind, pt);
      ind++;
    }
    ct++;
  }
  // brief profiling notes on mutual information affine registration macbook air , mi using every 20th point for sparse
  //  1 thread dense = 10 sec
  //  2 thread dense = 7.5  sec
  //  1 thread sparse = 2.2 sec
  //  2 thread sparse = 1.8 sec
  // this uses only 1500 points so it's probably not a great multi-thread test for the sparse case
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

  std::cout << "First do an affine registration " << std::endl;
  using OptimizerType = itk::GradientDescentOptimizerv4;
  auto optimizer = OptimizerType::New();
  using JointPDFStatusType = JointPDFStatus<OptimizerType, MetricType>;
  auto jointPDFStatus = JointPDFStatusType::New();
  jointPDFStatus->SetOutputFileNameBase(argv[3]);
  jointPDFStatus->SetMIMetric(metric);
  // optimizer->AddObserver( itk::IterationEvent(), jointPDFStatus );
  optimizer->SetMetric(metric);
  optimizer->SetNumberOfIterations(numberOfIterations);
  optimizer->SetScalesEstimator(shiftScaleEstimator);
  optimizer->StartOptimization();

  std::cout << "Number of work units: metric: " << metric->GetNumberOfWorkUnitsUsed()
            << " optimizer: " << optimizer->GetNumberOfWorkUnits() << std::endl;
  std::cout << "GetNumberOfSkippedFixedSampledPoints: " << metric->GetNumberOfSkippedFixedSampledPoints() << std::endl;

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
  RegistrationParameterScalesFromShiftType::ScalesType displacementScales(
    displacementTransform->GetNumberOfLocalParameters());
  displacementScales.Fill(1);
  if (false)
  {
    optimizer->SetScales(displacementScales);
  }
  else
  {
    optimizer->SetScalesEstimator(shiftScaleEstimator);
  }
  optimizer->SetMetric(metric);
  optimizer->SetNumberOfIterations(numberOfDisplacementIterations);
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

  std::cout << "GetNumberOfSkippedFixedSampledPoints: " << metric->GetNumberOfSkippedFixedSampledPoints() << std::endl;


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
