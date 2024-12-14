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

#include "itkAffineTransform.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkBSplineExponentialDiffeomorphicTransform.h"
#include "itkBSplineExponentialDiffeomorphicTransformParametersAdaptor.h"
#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkVectorMagnitudeImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkTestingMacros.h"

template <typename TFilter>
class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

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
    const auto * filter = static_cast<const TFilter *>(object);
    if (typeid(event) != typeid(itk::IterationEvent))
    {
      return;
    }

    const unsigned int                                             currentLevel = filter->GetCurrentLevel();
    const typename TFilter::ShrinkFactorsPerDimensionContainerType shrinkFactors =
      filter->GetShrinkFactorsPerDimension(currentLevel);
    typename TFilter::SmoothingSigmasArrayType                 smoothingSigmas = filter->GetSmoothingSigmasPerLevel();
    typename TFilter::TransformParametersAdaptorsContainerType adaptors =
      filter->GetTransformParametersAdaptorsPerLevel();

    const itk::ObjectToObjectOptimizerBase * optimizerBase = filter->GetOptimizer();
    using GradientDescentOptimizerv4Type = itk::GradientDescentOptimizerv4;
    const typename GradientDescentOptimizerv4Type::ConstPointer optimizer =
      dynamic_cast<const GradientDescentOptimizerv4Type *>(optimizerBase);
    if (!optimizer)
    {
      itkGenericExceptionMacro("Error dynamic_cast failed");
    }
    typename GradientDescentOptimizerv4Type::DerivativeType gradient = optimizer->GetGradient();

    /* orig
    std::cout << "  Current level = " << currentLevel << '\n';
    std::cout << "    shrink factor = " << shrinkFactors[currentLevel] << '\n';
    std::cout << "    smoothing sigma = " << smoothingSigmas[currentLevel] << '\n';
    std::cout << "    required fixed parameters = " << adaptors[currentLevel]->GetRequiredFixedParameters() <<
    '\n';
    */

    // debug:
    std::cout << "  CL Current level:           " << currentLevel << '\n';
    std::cout << "   SF Shrink factor:          " << shrinkFactors << '\n';
    std::cout << "   SS Smoothing sigma:        " << smoothingSigmas[currentLevel] << '\n';
    std::cout << "   RFP Required fixed params: " << adaptors[currentLevel]->GetRequiredFixedParameters() << '\n';
    std::cout << "   LR Final learning rate:    " << optimizer->GetLearningRate() << '\n';
    std::cout << "   FM Final metric value:     " << optimizer->GetCurrentMetricValue() << '\n';
    std::cout << "   SC Optimizer scales:       " << optimizer->GetScales() << '\n';
    std::cout << "   FG Final metric gradient (sample of values): ";
    if (gradient.GetSize() < 10)
    {
      std::cout << gradient;
    }
    else
    {
      for (itk::SizeValueType i = 0; i < gradient.GetSize(); i += (gradient.GetSize() / 16))
      {
        std::cout << gradient[i] << ' ';
      }
    }
    std::cout << '\n';
  }
};

template <unsigned int VImageDimension>
int
PerformBSplineExpImageRegistration(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr
      << " imageDimension fixedImage movingImage outputImage numberOfAffineIterations numberOfDeformableIterations"
      << '\n';
    return EXIT_FAILURE;
  }

  using PixelType = double;
  using FixedImageType = itk::Image<PixelType, VImageDimension>;
  using MovingImageType = itk::Image<PixelType, VImageDimension>;

  using ImageReaderType = itk::ImageFileReader<FixedImageType>;

  auto fixedImageReader = ImageReaderType::New();
  fixedImageReader->SetFileName(argv[2]);
  fixedImageReader->Update();
  const typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  fixedImage->Update();
  fixedImage->DisconnectPipeline();

  auto movingImageReader = ImageReaderType::New();
  movingImageReader->SetFileName(argv[3]);
  movingImageReader->Update();
  const typename MovingImageType::Pointer movingImage = movingImageReader->GetOutput();
  movingImage->Update();
  movingImage->DisconnectPipeline();

  using AffineTransformType = itk::AffineTransform<double, VImageDimension>;
  using AffineRegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, AffineTransformType>;
  using GradientDescentOptimizerv4Type = itk::GradientDescentOptimizerv4;
  auto affineSimple = AffineRegistrationType::New();
  affineSimple->SetFixedImage(fixedImage);
  affineSimple->SetMovingImage(movingImage);

  // Smooth by specified gaussian sigmas for each level.  These values are specified in
  // physical units. Sigmas of zero cause inconsistency between some platforms.
  {
    typename AffineRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
    smoothingSigmasPerLevel.SetSize(3);
    smoothingSigmasPerLevel[0] = 2;
    smoothingSigmasPerLevel[1] = 1;
    smoothingSigmasPerLevel[2] = 1; // 0;
    affineSimple->SetSmoothingSigmasPerLevel(smoothingSigmasPerLevel);
  }

  using GradientDescentOptimizerv4Type = itk::GradientDescentOptimizerv4;
  const typename GradientDescentOptimizerv4Type::Pointer affineOptimizer =
    dynamic_cast<GradientDescentOptimizerv4Type *>(affineSimple->GetModifiableOptimizer());
  if (!affineOptimizer)
  {
    itkGenericExceptionMacro("Error dynamic_cast failed");
  }
#ifdef NDEBUG
  affineOptimizer->SetNumberOfIterations(std::stoi(argv[5]));
#else
  affineOptimizer->SetNumberOfIterations(1);
#endif

  affineOptimizer->SetDoEstimateLearningRateOnce(false); // true by default
  affineOptimizer->SetDoEstimateLearningRateAtEachIteration(true);

  using AffineCommandType = CommandIterationUpdate<AffineRegistrationType>;
  auto affineObserver = AffineCommandType::New();
  affineSimple->AddObserver(itk::IterationEvent(), affineObserver);

  {
    using ImageMetricType = itk::ImageToImageMetricv4<FixedImageType, MovingImageType>;
    const typename ImageMetricType::Pointer imageMetric =
      dynamic_cast<ImageMetricType *>(affineSimple->GetModifiableMetric());
    if (imageMetric.IsNull())
    {
      std::cout << "Test failed - too many pixels different." << '\n';
      return EXIT_FAILURE;
    }
    imageMetric->SetFloatingPointCorrectionResolution(1e4);
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(affineSimple->Update());


  {
    using ImageMetricType = itk::ImageToImageMetricv4<FixedImageType, MovingImageType>;
    const typename ImageMetricType::Pointer imageMetric =
      dynamic_cast<ImageMetricType *>(affineOptimizer->GetModifiableMetric());
    std::cout << "Affine parameters after registration: " << '\n'
              << affineOptimizer->GetCurrentPosition() << '\n'
              << "Last LearningRate: " << affineOptimizer->GetLearningRate() << '\n'
              << "Use FltPtCorrex: " << imageMetric->GetUseFloatingPointCorrection() << '\n'
              << "FltPtCorrexRes: " << imageMetric->GetFloatingPointCorrectionResolution() << '\n'
              << "Number of work units used: metric: " << imageMetric->GetNumberOfWorkUnitsUsed() << '\n'
              << " optimizer: " << affineOptimizer->GetNumberOfWorkUnits() << '\n';
  }
  //
  // Now do the displacement field transform with gaussian smoothing using
  // the composite transform.
  //

  using RealType = typename AffineRegistrationType::RealType;

  using CompositeTransformType = itk::CompositeTransform<RealType, VImageDimension>;
  auto compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform(affineSimple->GetModifiableTransform());

  using VectorType = itk::Vector<RealType, VImageDimension>;
  constexpr VectorType zeroVector{};
  using DisplacementFieldType = itk::Image<VectorType, VImageDimension>;
  auto displacementField = DisplacementFieldType::New();
  displacementField->CopyInformation(fixedImage);
  displacementField->SetRegions(fixedImage->GetBufferedRegion());
  displacementField->Allocate();
  displacementField->FillBuffer(zeroVector);

  using DisplacementFieldTransformType = itk::BSplineExponentialDiffeomorphicTransform<RealType, VImageDimension>;

  using DisplacementFieldRegistrationType =
    itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, DisplacementFieldTransformType>;
  const typename DisplacementFieldRegistrationType::Pointer displacementFieldSimple =
    DisplacementFieldRegistrationType::New();

  auto fieldTransform = DisplacementFieldTransformType::New();

  auto updateControlPoints = itk::MakeFilled<typename DisplacementFieldTransformType::ArrayType>(10);

  auto velocityControlPoints = itk::MakeFilled<typename DisplacementFieldTransformType::ArrayType>(10);

  fieldTransform->SetNumberOfControlPointsForTheUpdateField(updateControlPoints);
  fieldTransform->SetNumberOfControlPointsForTheConstantVelocityField(velocityControlPoints);
  fieldTransform->SetConstantVelocityField(displacementField);
  fieldTransform->SetCalculateNumberOfIntegrationStepsAutomatically(true);

  displacementFieldSimple->SetInitialTransform(fieldTransform);
  displacementFieldSimple->InPlaceOn();

  using CorrelationMetricType = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType>;
  auto correlationMetric = CorrelationMetricType::New();
  auto radius = itk::MakeFilled<typename CorrelationMetricType::RadiusType>(4);
  correlationMetric->SetRadius(radius);
  correlationMetric->SetUseMovingImageGradientFilter(false);
  correlationMetric->SetUseFixedImageGradientFilter(false);

  // correlationMetric->SetUseFloatingPointCorrection(true);
  // correlationMetric->SetFloatingPointCorrectionResolution(1e4);

  using ScalesEstimatorType = itk::RegistrationParameterScalesFromPhysicalShift<CorrelationMetricType>;
  auto scalesEstimator = ScalesEstimatorType::New();
  scalesEstimator->SetMetric(correlationMetric);
  scalesEstimator->SetTransformForward(true);
  scalesEstimator->SetSmallParameterVariation(1.0);

  auto optimizer = GradientDescentOptimizerv4Type::New();
  optimizer->SetLearningRate(1.0);
#ifdef NDEBUG
  optimizer->SetNumberOfIterations(std::stoi(argv[6]));
#else
  optimizer->SetNumberOfIterations(1);
#endif
  optimizer->SetScalesEstimator(nullptr);
  optimizer->SetDoEstimateLearningRateOnce(false); // true by default
  optimizer->SetDoEstimateLearningRateAtEachIteration(true);

  displacementFieldSimple->SetFixedImage(fixedImage);
  displacementFieldSimple->SetMovingImage(movingImage);
  displacementFieldSimple->SetNumberOfLevels(3);
  displacementFieldSimple->SetMovingInitialTransform(compositeTransform);
  displacementFieldSimple->SetMetric(correlationMetric);
  displacementFieldSimple->SetOptimizer(optimizer);

  // Shrink the virtual domain by specified factors for each level.  See documentation
  // for the itkShrinkImageFilter for more detailed behavior.
  typename DisplacementFieldRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize(3);
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;
  displacementFieldSimple->SetShrinkFactorsPerLevel(shrinkFactorsPerLevel);

  // Smooth by specified gaussian sigmas for each level.  These values are specified in
  // physical units.
  typename DisplacementFieldRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize(3);
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 1;
  displacementFieldSimple->SetSmoothingSigmasPerLevel(smoothingSigmasPerLevel);

  using DisplacementFieldTransformAdaptorType =
    itk::BSplineExponentialDiffeomorphicTransformParametersAdaptor<DisplacementFieldTransformType>;

  typename DisplacementFieldRegistrationType::TransformParametersAdaptorsContainerType adaptors;

  for (unsigned int level = 0; level < shrinkFactorsPerLevel.Size(); ++level)
  {
    // We use the shrink image filter to calculate the fixed parameters of the virtual
    // domain at each level.  To speed up calculation and avoid unnecessary memory
    // usage, we could calculate these fixed parameters directly.

    using ShrinkFilterType = itk::ShrinkImageFilter<DisplacementFieldType, DisplacementFieldType>;
    auto shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors(shrinkFactorsPerLevel[level]);
    shrinkFilter->SetInput(displacementField);
    shrinkFilter->Update();

    const typename DisplacementFieldTransformAdaptorType::Pointer fieldTransformAdaptor =
      DisplacementFieldTransformAdaptorType::New();
    fieldTransformAdaptor->SetRequiredSpacing(shrinkFilter->GetOutput()->GetSpacing());
    fieldTransformAdaptor->SetRequiredSize(shrinkFilter->GetOutput()->GetBufferedRegion().GetSize());
    fieldTransformAdaptor->SetRequiredDirection(shrinkFilter->GetOutput()->GetDirection());
    fieldTransformAdaptor->SetRequiredOrigin(shrinkFilter->GetOutput()->GetOrigin());

    adaptors.push_back(fieldTransformAdaptor);
  }
  displacementFieldSimple->SetTransformParametersAdaptorsPerLevel(adaptors);

  using DisplacementFieldRegistrationCommandType = CommandIterationUpdate<DisplacementFieldRegistrationType>;
  const typename DisplacementFieldRegistrationCommandType::Pointer displacementFieldObserver =
    DisplacementFieldRegistrationCommandType::New();
  displacementFieldSimple->AddObserver(itk::IterationEvent(), displacementFieldObserver);

  ITK_TRY_EXPECT_NO_EXCEPTION(displacementFieldSimple->Update());


  compositeTransform->AddTransform(displacementFieldSimple->GetModifiableTransform());

  std::cout << "After displacement registration: " << '\n'
            << "Last LearningRate: " << optimizer->GetLearningRate() << '\n'
            << "Use FltPtCorrex: " << correlationMetric->GetUseFloatingPointCorrection() << '\n'
            << "FltPtCorrexRes: " << correlationMetric->GetFloatingPointCorrectionResolution() << '\n'
            << "Number of work units used: metric: " << correlationMetric->GetNumberOfWorkUnitsUsed()
            << " optimizer: " << displacementFieldSimple->GetOptimizer()->GetNumberOfWorkUnits() << '\n';

  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  auto resampler = ResampleFilterType::New();
  resampler->SetTransform(compositeTransform);
  resampler->SetInput(movingImage);
  resampler->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resampler->SetOutputOrigin(fixedImage->GetOrigin());
  resampler->SetOutputSpacing(fixedImage->GetSpacing());
  resampler->SetOutputDirection(fixedImage->GetDirection());
  resampler->SetDefaultPixelValue(0);
  resampler->Update();

  using WriterType = itk::ImageFileWriter<FixedImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[4]);
  writer->SetInput(resampler->GetOutput());
  writer->Update();

  // Check identity of forward and inverse transforms

  using ComposerType = itk::ComposeDisplacementFieldsImageFilter<DisplacementFieldType, DisplacementFieldType>;
  auto composer = ComposerType::New();
  composer->SetDisplacementField(fieldTransform->GetDisplacementField());
  composer->SetWarpingField(fieldTransform->GetInverseDisplacementField());
  composer->Update();

  using MagnituderType = itk::VectorMagnitudeImageFilter<DisplacementFieldType, MovingImageType>;
  auto magnituder = MagnituderType::New();
  magnituder->SetInput(composer->GetOutput());
  magnituder->Update();

  using StatisticsImageFilterType = itk::StatisticsImageFilter<MovingImageType>;
  auto stats = StatisticsImageFilterType::New();
  stats->SetInput(magnituder->GetOutput());
  stats->Update();

  std::cout << "Identity check:" << '\n';
  std::cout << "  Min:  " << stats->GetMinimum() << '\n';
  std::cout << "  Max:  " << stats->GetMaximum() << '\n';
  std::cout << "  Mean:  " << stats->GetMean() << '\n';
  std::cout << "  Variance:  " << stats->GetVariance() << '\n';

  if (stats->GetMean() > 0.1)
  {
    std::cerr << "Identity test failed." << '\n';
  }

  return EXIT_SUCCESS;
}

int
itkBSplineExponentialImageRegistrationTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr
      << " imageDimension fixedImage movingImage outputImage numberOfAffineIterations numberOfDeformableIterations"
      << '\n';
    return EXIT_FAILURE;
  }

  switch (std::stoi(argv[1]))
  {
    case 2:
      PerformBSplineExpImageRegistration<2>(argc, argv);
      break;
    case 3:
      PerformBSplineExpImageRegistration<3>(argc, argv);
      break;
    default:
      std::cerr << "Unsupported dimension" << '\n';
      return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
