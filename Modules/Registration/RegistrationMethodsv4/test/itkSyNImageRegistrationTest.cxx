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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkImageRegistrationMethodv4.h"
#include "itkSyNImageRegistrationMethod.h"

#include "itkAffineTransform.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkCompositeTransform.h"
#include "itkDisplacementFieldTransformParametersAdaptor.h"
#include "itkVector.h"
#include "itkTestingMacros.h"

template <typename TFilter>
class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

  using FixedImageType = typename TFilter::FixedImageType;
  static constexpr unsigned int ImageDimension = FixedImageType::ImageDimension; /** ImageDimension constants */

  using ShrinkFilterType = itk::ShrinkImageFilter<FixedImageType, FixedImageType>;
  using RealType = typename TFilter::OutputTransformType::ScalarType;
  using DisplacementFieldTransformType = itk::DisplacementFieldTransform<RealType, ImageDimension>;
  using DisplacementFieldType = typename DisplacementFieldTransformType::DisplacementFieldType;

protected:
  CommandIterationUpdate() = default;

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    const auto * filter = dynamic_cast<const TFilter *>(object);
    if (typeid(event) != typeid(itk::IterationEvent))
    {
      return;
    }

    unsigned int                                             currentLevel = filter->GetCurrentLevel();
    typename TFilter::ShrinkFactorsPerDimensionContainerType shrinkFactors =
      filter->GetShrinkFactorsPerDimension(currentLevel);
    typename TFilter::SmoothingSigmasArrayType                 smoothingSigmas = filter->GetSmoothingSigmasPerLevel();
    typename TFilter::TransformParametersAdaptorsContainerType adaptors =
      filter->GetTransformParametersAdaptorsPerLevel();

    std::cout << "  Current level = " << currentLevel << std::endl;
    std::cout << "    shrink factor = " << shrinkFactors << std::endl;
    std::cout << "    smoothing sigma = " << smoothingSigmas[currentLevel] << std::endl;
    std::cout << "    required fixed parameters = " << adaptors[currentLevel]->GetRequiredFixedParameters()
              << std::endl;

    /*
    testing "itkGetConstObjectMacro" at each iteration
    */
    auto shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors(shrinkFactors);
    shrinkFilter->SetInput(filter->GetFixedImage());
    shrinkFilter->Update();

    const typename FixedImageType::SizeType ImageSize = shrinkFilter->GetOutput()->GetBufferedRegion().GetSize();

    const typename DisplacementFieldType::SizeType FixedDisplacementFieldSize =
      filter->GetFixedToMiddleTransform()->GetDisplacementField()->GetBufferedRegion().GetSize();

    const typename DisplacementFieldType::SizeType MovingDisplacementFieldSize =
      filter->GetMovingToMiddleTransform()->GetDisplacementField()->GetBufferedRegion().GetSize();

    if ((FixedDisplacementFieldSize == ImageSize) && (MovingDisplacementFieldSize == ImageSize))
    {
      std::cout << " *Filter returns its internal transforms properly*" << std::endl;
    }
    else
    {
      itkExceptionMacro("Internal transforms should be consistent with input image size at each iteration.  "
                        << "Image size = " << ImageSize << ".  Fixed field size = " << FixedDisplacementFieldSize
                        << ".  Moving field size = " << MovingDisplacementFieldSize << ".");
    }
  }
};

template <unsigned int TDimension>
int
PerformDisplacementFieldImageRegistration(int itkNotUsed(argc), char * argv[])
{
  const unsigned int ImageDimension = TDimension;

  using PixelType = double;
  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;

  using ImageReaderType = itk::ImageFileReader<FixedImageType>;

  auto fixedImageReader = ImageReaderType::New();
  fixedImageReader->SetFileName(argv[2]);
  fixedImageReader->Update();
  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  fixedImage->Update();
  fixedImage->DisconnectPipeline();

  auto movingImageReader = ImageReaderType::New();
  movingImageReader->SetFileName(argv[3]);
  movingImageReader->Update();
  typename MovingImageType::Pointer movingImage = movingImageReader->GetOutput();
  movingImage->Update();
  movingImage->DisconnectPipeline();

  using AffineTransformType = itk::AffineTransform<double, ImageDimension>;
  using AffineRegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, AffineTransformType>;
  auto affineSimple = AffineRegistrationType::New();
  affineSimple->SetFixedImage(fixedImage);
  affineSimple->SetMovingImage(movingImage);

  // Shrink the virtual domain by specified factors for each level.  See documentation
  // for the itkShrinkImageFilter for more detailed behavior.
  typename AffineRegistrationType::ShrinkFactorsArrayType affineShrinkFactorsPerLevel;
  affineShrinkFactorsPerLevel.SetSize(3);
  affineShrinkFactorsPerLevel[0] = 4;
  affineShrinkFactorsPerLevel[1] = 4;
  affineShrinkFactorsPerLevel[2] = 4;
  affineSimple->SetShrinkFactorsPerLevel(affineShrinkFactorsPerLevel);

  // Set the number of iterations
  using GradientDescentOptimizerv4Type = itk::GradientDescentOptimizerv4;
  auto * optimizer = dynamic_cast<GradientDescentOptimizerv4Type *>(affineSimple->GetModifiableOptimizer());
  ITK_TEST_EXPECT_TRUE(optimizer != nullptr);
#ifdef NDEBUG
  optimizer->SetNumberOfIterations(100);
#else
  optimizer->SetNumberOfIterations(1);
#endif

  std::cout << "Affine transform" << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(affineSimple->Update());


  //
  // Now do the displacement field transform with gaussian smoothing using
  // the composite transform.
  //

  using RealType = typename AffineRegistrationType::RealType;

  using CompositeTransformType = itk::CompositeTransform<RealType, ImageDimension>;
  auto compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform(affineSimple->GetModifiableTransform());

  using AffineResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  auto affineResampler = AffineResampleFilterType::New();
  affineResampler->SetTransform(compositeTransform);
  affineResampler->SetInput(movingImage);
  affineResampler->SetSize(fixedImage->GetBufferedRegion().GetSize());
  affineResampler->SetOutputOrigin(fixedImage->GetOrigin());
  affineResampler->SetOutputSpacing(fixedImage->GetSpacing());
  affineResampler->SetOutputDirection(fixedImage->GetDirection());
  affineResampler->SetDefaultPixelValue(0);
  affineResampler->Update();

  std::string affineMovingImageFileName = std::string(argv[4]) + std::string("MovingImageAfterAffineTransform.nii.gz");

  using AffineWriterType = itk::ImageFileWriter<FixedImageType>;
  auto affineWriter = AffineWriterType::New();
  affineWriter->SetFileName(affineMovingImageFileName.c_str());
  affineWriter->SetInput(affineResampler->GetOutput());
  affineWriter->Update();

  using VectorType = itk::Vector<RealType, ImageDimension>;
  constexpr VectorType zeroVector{};

  // Create the SyN deformable registration method

  using DisplacementFieldType = itk::Image<VectorType, ImageDimension>;
  auto displacementField = DisplacementFieldType::New();
  displacementField->CopyInformation(fixedImage);
  displacementField->SetRegions(fixedImage->GetBufferedRegion());
  displacementField->Allocate();
  displacementField->FillBuffer(zeroVector);

  auto inverseDisplacementField = DisplacementFieldType::New();
  inverseDisplacementField->CopyInformation(fixedImage);
  inverseDisplacementField->SetRegions(fixedImage->GetBufferedRegion());
  inverseDisplacementField->Allocate();
  inverseDisplacementField->FillBuffer(zeroVector);

  using DisplacementFieldRegistrationType = itk::SyNImageRegistrationMethod<FixedImageType, MovingImageType>;
  typename DisplacementFieldRegistrationType::Pointer displacementFieldRegistration =
    DisplacementFieldRegistrationType::New();

  using OutputTransformType = typename DisplacementFieldRegistrationType::OutputTransformType;
  auto outputTransform = OutputTransformType::New();
  outputTransform->SetDisplacementField(displacementField);
  outputTransform->SetInverseDisplacementField(inverseDisplacementField);

  displacementFieldRegistration->SetInitialTransform(outputTransform);
  displacementFieldRegistration->InPlaceOn();

  // Test member functions
  displacementFieldRegistration->SetDownsampleImagesForMetricDerivatives(false);
  if (displacementFieldRegistration->GetDownsampleImagesForMetricDerivatives() != false)
  {
    return EXIT_FAILURE;
  }
  displacementFieldRegistration->SetDownsampleImagesForMetricDerivatives(true);
  if (displacementFieldRegistration->GetDownsampleImagesForMetricDerivatives() != true)
  {
    return EXIT_FAILURE;
  }
  displacementFieldRegistration->SetAverageMidPointGradients(false);
  if (displacementFieldRegistration->GetAverageMidPointGradients() != false)
  {
    return EXIT_FAILURE;
  }
  displacementFieldRegistration->SetAverageMidPointGradients(true);
  if (displacementFieldRegistration->GetAverageMidPointGradients() != true)
  {
    return EXIT_FAILURE;
  }
  // Create the transform adaptors

  using DisplacementFieldTransformAdaptorType = itk::DisplacementFieldTransformParametersAdaptor<OutputTransformType>;
  typename DisplacementFieldRegistrationType::TransformParametersAdaptorsContainerType adaptors;

  // Create the transform adaptors
  // For the gaussian displacement field, the specified variances are in image spacing terms
  // and, in normal practice, we typically don't change these values at each level.  However,
  // if the user wishes to add that option, they can use the class
  // GaussianSmoothingOnUpdateDisplacementFieldTransformAdaptor

  unsigned int numberOfLevels = 3;

  typename DisplacementFieldRegistrationType::NumberOfIterationsArrayType numberOfIterationsPerLevel;
  numberOfIterationsPerLevel.SetSize(3);
#ifdef NDEBUG
  numberOfIterationsPerLevel[0] = std::stoi(argv[5]);
  numberOfIterationsPerLevel[1] = 2;
  numberOfIterationsPerLevel[2] = 1;
#else
  numberOfIterationsPerLevel[0] = 1;
  numberOfIterationsPerLevel[1] = 1;
  numberOfIterationsPerLevel[2] = 1;
#endif
  RealType varianceForUpdateField = 1.75;
  RealType varianceForTotalField = 0.5;

  typename DisplacementFieldRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize(3);
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;

  typename DisplacementFieldRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize(3);
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 0;

  for (unsigned int level = 0; level < numberOfLevels; ++level)
  {
    // We use the shrink image filter to calculate the fixed parameters of the virtual
    // domain at each level.  To speed up calculation and avoid unnecessary memory
    // usage, we could calculate these fixed parameters directly.

    using ShrinkFilterType = itk::ShrinkImageFilter<DisplacementFieldType, DisplacementFieldType>;
    auto shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors(shrinkFactorsPerLevel[level]);
    shrinkFilter->SetInput(displacementField);
    shrinkFilter->Update();

    typename DisplacementFieldTransformAdaptorType::Pointer fieldTransformAdaptor =
      DisplacementFieldTransformAdaptorType::New();
    fieldTransformAdaptor->SetRequiredSpacing(shrinkFilter->GetOutput()->GetSpacing());
    fieldTransformAdaptor->SetRequiredSize(shrinkFilter->GetOutput()->GetBufferedRegion().GetSize());
    fieldTransformAdaptor->SetRequiredDirection(shrinkFilter->GetOutput()->GetDirection());
    fieldTransformAdaptor->SetRequiredOrigin(shrinkFilter->GetOutput()->GetOrigin());
    fieldTransformAdaptor->SetTransform(outputTransform);

    adaptors.push_back(fieldTransformAdaptor);
  }

  using CorrelationMetricType = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType>;
  auto                                       correlationMetric = CorrelationMetricType::New();
  typename CorrelationMetricType::RadiusType radius;
  radius.Fill(4);
  correlationMetric->SetRadius(radius);
  correlationMetric->SetUseMovingImageGradientFilter(false);
  correlationMetric->SetUseFixedImageGradientFilter(false);

  displacementFieldRegistration->SetFixedImage(fixedImage);
  displacementFieldRegistration->SetMovingImage(movingImage);
  displacementFieldRegistration->SetNumberOfLevels(3);
  displacementFieldRegistration->SetMovingInitialTransform(compositeTransform);
  displacementFieldRegistration->SetShrinkFactorsPerLevel(shrinkFactorsPerLevel);
  displacementFieldRegistration->SetSmoothingSigmasPerLevel(smoothingSigmasPerLevel);
  displacementFieldRegistration->SetMetric(correlationMetric);

  typename OutputTransformType::Pointer fixedToMiddleTransform;
  displacementFieldRegistration->SetFixedToMiddleTransform(fixedToMiddleTransform);

  typename OutputTransformType::Pointer movingToMiddleTransform;
  displacementFieldRegistration->SetMovingToMiddleTransform(movingToMiddleTransform);

  const typename DisplacementFieldRegistrationType::RealType epsilon =
    itk::NumericTraits<typename DisplacementFieldRegistrationType::RealType>::epsilon();
  const typename DisplacementFieldRegistrationType::RealType learningRate = std::stod(argv[6]);
  displacementFieldRegistration->SetLearningRate(learningRate);
  typename DisplacementFieldRegistrationType::RealType obtainedLearningRate =
    displacementFieldRegistration->GetLearningRate();
  if (!itk::Math::FloatAlmostEqual(obtainedLearningRate, learningRate, 10, epsilon))
  {
    std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetLearningRate" << std::endl;
    std::cerr << "Expected value " << learningRate << std::endl;
    std::cerr << " differs from " << obtainedLearningRate;
    std::cerr << " by more than " << epsilon << std::endl;
    return EXIT_FAILURE;
  }

  displacementFieldRegistration->SetNumberOfIterationsPerLevel(numberOfIterationsPerLevel);
  ITK_TEST_SET_GET_VALUE(numberOfIterationsPerLevel, displacementFieldRegistration->GetNumberOfIterationsPerLevel());

  displacementFieldRegistration->SetTransformParametersAdaptorsPerLevel(adaptors);

  displacementFieldRegistration->SetGaussianSmoothingVarianceForTheUpdateField(varianceForUpdateField);
  RealType obtainedVarianceForUpdateField =
    displacementFieldRegistration->GetGaussianSmoothingVarianceForTheUpdateField();
  if (!itk::Math::FloatAlmostEqual(obtainedVarianceForUpdateField, varianceForUpdateField, 10, epsilon))
  {
    std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetGaussianSmoothingVarianceForTheUpdateField" << std::endl;
    std::cerr << "Expected value " << obtainedVarianceForUpdateField << std::endl;
    std::cerr << " differs from " << varianceForUpdateField;
    std::cerr << " by more than " << epsilon << std::endl;
    return EXIT_FAILURE;
  }

  displacementFieldRegistration->SetGaussianSmoothingVarianceForTheTotalField(varianceForTotalField);
  RealType obtainedVarianceForTotalField =
    displacementFieldRegistration->GetGaussianSmoothingVarianceForTheTotalField();
  if (!itk::Math::FloatAlmostEqual(obtainedVarianceForTotalField, varianceForTotalField, 10, epsilon))
  {
    std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetGaussianSmoothingVarianceForTheTotalField" << std::endl;
    std::cerr << "Expected value " << obtainedVarianceForTotalField << std::endl;
    std::cerr << " differs from " << varianceForTotalField;
    std::cerr << " by more than " << epsilon << std::endl;
    return EXIT_FAILURE;
  }

  const typename DisplacementFieldRegistrationType::RealType convergenceThreshold = 1.0e-6;
  displacementFieldRegistration->SetConvergenceThreshold(convergenceThreshold);
  typename DisplacementFieldRegistrationType::RealType obtainedConvergenceThreshold =
    displacementFieldRegistration->GetConvergenceThreshold();
  if (!itk::Math::FloatAlmostEqual(obtainedConvergenceThreshold, convergenceThreshold, 10, epsilon))
  {
    std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetConvergenceThreshold" << std::endl;
    std::cerr << "Expected value " << obtainedConvergenceThreshold << std::endl;
    std::cerr << " differs from " << convergenceThreshold;
    std::cerr << " by more than " << epsilon << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int convergenceWindowSize = 10;
  displacementFieldRegistration->SetConvergenceWindowSize(convergenceWindowSize);
  ITK_TEST_SET_GET_VALUE(convergenceWindowSize, displacementFieldRegistration->GetConvergenceWindowSize());

  using DisplacementFieldCommandType = CommandIterationUpdate<DisplacementFieldRegistrationType>;
  auto DisplacementFieldObserver = DisplacementFieldCommandType::New();
  displacementFieldRegistration->AddObserver(itk::IterationEvent(), DisplacementFieldObserver);

  ITK_TRY_EXPECT_NO_EXCEPTION(displacementFieldRegistration->Update());


  compositeTransform->AddTransform(outputTransform);

  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  auto resampler = ResampleFilterType::New();
  resampler->SetTransform(compositeTransform);
  resampler->SetInput(movingImage);
  resampler->SetSize(fixedImage->GetBufferedRegion().GetSize());
  resampler->SetOutputOrigin(fixedImage->GetOrigin());
  resampler->SetOutputSpacing(fixedImage->GetSpacing());
  resampler->SetOutputDirection(fixedImage->GetDirection());
  resampler->SetDefaultPixelValue(0);
  resampler->Update();

  std::string warpedMovingImageFileName = std::string(argv[4]) + std::string("MovingImageAfterSyN.nii.gz");

  using WriterType = itk::ImageFileWriter<FixedImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(warpedMovingImageFileName.c_str());
  writer->SetInput(resampler->GetOutput());
  writer->Update();

  using InverseResampleFilterType = itk::ResampleImageFilter<FixedImageType, MovingImageType>;
  typename InverseResampleFilterType::Pointer inverseResampler = ResampleFilterType::New();
  inverseResampler->SetTransform(compositeTransform->GetInverseTransform());
  inverseResampler->SetInput(fixedImage);
  inverseResampler->SetSize(movingImage->GetBufferedRegion().GetSize());
  inverseResampler->SetOutputOrigin(movingImage->GetOrigin());
  inverseResampler->SetOutputSpacing(movingImage->GetSpacing());
  inverseResampler->SetOutputDirection(movingImage->GetDirection());
  inverseResampler->SetDefaultPixelValue(0);
  inverseResampler->Update();

  std::string inverseWarpedFixedImageFileName = std::string(argv[4]) + std::string("InverseWarpedFixedImage.nii.gz");

  using InverseWriterType = itk::ImageFileWriter<MovingImageType>;
  auto inverseWriter = InverseWriterType::New();
  inverseWriter->SetFileName(inverseWarpedFixedImageFileName.c_str());
  inverseWriter->SetInput(inverseResampler->GetOutput());
  inverseWriter->Update();

  std::string displacementFieldFileName = std::string(argv[4]) + std::string("DisplacementField.nii.gz");

  using DisplacementFieldWriterType = itk::ImageFileWriter<DisplacementFieldType>;
  auto displacementFieldWriter = DisplacementFieldWriterType::New();
  displacementFieldWriter->SetFileName(displacementFieldFileName.c_str());
  displacementFieldWriter->SetInput(outputTransform->GetDisplacementField());
  displacementFieldWriter->Update();

  return EXIT_SUCCESS;
}

int
itkSyNImageRegistrationTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cout << itkNameOfTestExecutableMacro(argv)
              << " imageDimension fixedImage movingImage outputPrefix numberOfDeformableIterations learningRate"
              << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise the objet's basic methods outside the templated test helper to
  // avoid the Superclass name not being found.
  constexpr unsigned int ImageDimension = 2;

  using PixelType = double;
  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;

  using DisplacementFieldRegistrationType = itk::SyNImageRegistrationMethod<FixedImageType, MovingImageType>;
  typename DisplacementFieldRegistrationType::Pointer displacementFieldRegistration =
    DisplacementFieldRegistrationType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    displacementFieldRegistration, SyNImageRegistrationMethod, ImageRegistrationMethodv4);


  switch (std::stoi(argv[1]))
  {
    case 2:
      PerformDisplacementFieldImageRegistration<2>(argc, argv);
      break;
    case 3:
      PerformDisplacementFieldImageRegistration<3>(argc, argv);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return EXIT_FAILURE;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
