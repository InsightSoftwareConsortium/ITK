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
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkImageMaskSpatialObject.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
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
    if (object == nullptr)
    {
      itkExceptionMacro(<< "Command update on null object");
    }
    std::cout << "Observing from class " << object->GetNameOfClass();
    if (!object->GetObjectName().empty())
    {
      std::cout << " \"" << object->GetObjectName() << "\"";
    }
    std::cout << std::endl;
    const auto * filter = static_cast<const TFilter *>(object);

    if (typeid(event) != typeid(itk::MultiResolutionIterationEvent) || object == nullptr)
    {
      return;
    }

    unsigned int                                             currentLevel = filter->GetCurrentLevel();
    typename TFilter::ShrinkFactorsPerDimensionContainerType shrinkFactors =
      filter->GetShrinkFactorsPerDimension(currentLevel);
    typename TFilter::SmoothingSigmasArrayType                 smoothingSigmas = filter->GetSmoothingSigmasPerLevel();
    typename TFilter::TransformParametersAdaptorsContainerType adaptors =
      filter->GetTransformParametersAdaptorsPerLevel();

    const itk::ObjectToObjectOptimizerBase * optimizerBase = filter->GetOptimizer();
    using GradientDescentOptimizerv4Type = itk::GradientDescentOptimizerv4;
    typename GradientDescentOptimizerv4Type::ConstPointer optimizer =
      dynamic_cast<const GradientDescentOptimizerv4Type *>(optimizerBase);
    if (!optimizer)
    {
      itkGenericExceptionMacro("Error dynamic_cast failed");
    }
    typename GradientDescentOptimizerv4Type::DerivativeType gradient = optimizer->GetGradient();

    std::cout << "  CL Current level:           " << currentLevel << std::endl;
    std::cout << "   SF Shrink factor:          " << shrinkFactors << std::endl;
    std::cout << "   SS Smoothing sigma:        " << smoothingSigmas[currentLevel] << std::endl;
    if (adaptors[currentLevel])
    {
      std::cout << "   RFP Required fixed params: " << adaptors[currentLevel]->GetRequiredFixedParameters()
                << std::endl;
    }
    std::cout << "   LR Final learning rate:    " << optimizer->GetLearningRate() << std::endl;
    std::cout << "   FM Final metric value:     " << optimizer->GetCurrentMetricValue() << std::endl;
    std::cout << "   SC Optimizer scales:       " << optimizer->GetScales() << std::endl;
    std::cout << "   FG Final metric gradient (sample of values): ";
    if (gradient.GetSize() < 10)
    {
      std::cout << gradient;
    }
    else
    {
      for (itk::SizeValueType i = 0; i < gradient.GetSize(); i += (gradient.GetSize() / 16))
      {
        std::cout << gradient[i] << " ";
      }
    }
    std::cout << std::endl;
  }
};

template <unsigned int VImageDimension, typename TPixel>
int
PerformSimpleImageRegistrationWithMaskAndSampling(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " pixelType imageDimension fixedImage movingImage outputImage numberOfAffineIterations "
                 "numberOfDeformableIterations"
              << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = TPixel;
  using FixedImageType = itk::Image<PixelType, VImageDimension>;
  using MovingImageType = itk::Image<PixelType, VImageDimension>;

  using ImageReaderType = itk::ImageFileReader<FixedImageType>;

  auto fixedImageReader = ImageReaderType::New();
  fixedImageReader->SetFileName(argv[3]);
  fixedImageReader->Update();
  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  fixedImage->Update();
  fixedImage->DisconnectPipeline();

  // Create a fixed mask.  We're going to mask the mask region coextensive with the
  // fixed image just to illustrate the concept of masking.

  using ImageMaskSpatialObjectType = itk::ImageMaskSpatialObject<VImageDimension>;
  using MaskImageType = typename ImageMaskSpatialObjectType::ImageType;

  auto maskImage = MaskImageType::New();
  maskImage->CopyInformation(fixedImage);
  maskImage->SetRegions(fixedImage->GetRequestedRegion());
  maskImage->Allocate();
  maskImage->FillBuffer(itk::NumericTraits<typename MaskImageType::PixelType>::OneValue());

  auto maskSpatialObject = ImageMaskSpatialObjectType::New();
  maskSpatialObject->SetImage(maskImage);
  maskSpatialObject->Update();

  auto movingImageReader = ImageReaderType::New();
  movingImageReader->SetFileName(argv[4]);
  movingImageReader->Update();
  typename MovingImageType::Pointer movingImage = movingImageReader->GetOutput();
  movingImage->Update();
  movingImage->DisconnectPipeline();

  using AffineTransformType = itk::AffineTransform<double, VImageDimension>;
  auto affineTransform = AffineTransformType::New();

  using AffineRegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType>;
  auto affineSimple = AffineRegistrationType::New();
  affineSimple->SetObjectName("affineSimple");
  affineSimple->SetFixedImage(fixedImage);
  affineSimple->SetMovingImage(movingImage);
  affineSimple->SetMetricSamplingStrategy(AffineRegistrationType::MetricSamplingStrategyEnum::REGULAR);
  affineSimple->SetMetricSamplingPercentage(0.5);
  affineSimple->SetInitialTransform(affineTransform);
  affineSimple->InPlaceOn();

  // Ensuring code coverage for boolean macros
  affineSimple->SmoothingSigmasAreSpecifiedInPhysicalUnitsOff();
  affineSimple->SetSmoothingSigmasAreSpecifiedInPhysicalUnits(false);
  if (affineSimple->GetSmoothingSigmasAreSpecifiedInPhysicalUnits() != false)
  {
    std::cerr << "Returned unexpected value of TRUE." << std::endl;
    return EXIT_FAILURE;
  }

  using MIMetricType = itk::JointHistogramMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType>;
  auto mutualInformationMetric = MIMetricType::New();
  mutualInformationMetric->SetNumberOfHistogramBins(20);
  mutualInformationMetric->SetUseMovingImageGradientFilter(false);
  mutualInformationMetric->SetUseFixedImageGradientFilter(false);
  mutualInformationMetric->SetUseSampledPointSet(false);
  mutualInformationMetric->SetVirtualDomainFromImage(fixedImage);
  mutualInformationMetric->SetFixedImageMask(maskSpatialObject);
  affineSimple->SetMetric(mutualInformationMetric);

  using AffineScalesEstimatorType = itk::RegistrationParameterScalesFromPhysicalShift<MIMetricType>;
  auto scalesEstimator1 = AffineScalesEstimatorType::New();
  scalesEstimator1->SetMetric(mutualInformationMetric);
  scalesEstimator1->SetTransformForward(true);

  affineSimple->SmoothingSigmasAreSpecifiedInPhysicalUnitsOn();
  affineSimple->SetSmoothingSigmasAreSpecifiedInPhysicalUnits(true);
  if (affineSimple->GetSmoothingSigmasAreSpecifiedInPhysicalUnits() != true)
  {
    std::cerr << "Returned unexpected value of FALSE." << std::endl;
    return EXIT_FAILURE;
  }

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
  typename GradientDescentOptimizerv4Type::Pointer affineOptimizer =
    dynamic_cast<GradientDescentOptimizerv4Type *>(affineSimple->GetModifiableOptimizer());
  if (!affineOptimizer)
  {
    itkGenericExceptionMacro("Error dynamic_cast failed");
  }
#ifdef NDEBUG
  affineOptimizer->SetNumberOfIterations(std::stoi(argv[6]));
#else
  affineOptimizer->SetNumberOfIterations(1);
#endif
  affineOptimizer->SetDoEstimateLearningRateOnce(false); // true by default
  affineOptimizer->SetDoEstimateLearningRateAtEachIteration(true);
  affineOptimizer->SetScalesEstimator(scalesEstimator1);

  using AffineCommandType = CommandIterationUpdate<AffineRegistrationType>;
  auto affineObserver = AffineCommandType::New();
  affineSimple->AddObserver(itk::MultiResolutionIterationEvent(), affineObserver);

  {
    using ImageMetricType = itk::ImageToImageMetricv4<FixedImageType, MovingImageType>;
    typename ImageMetricType::Pointer imageMetric =
      dynamic_cast<ImageMetricType *>(affineSimple->GetModifiableMetric());
    // imageMetric->SetUseFloatingPointCorrection(true);
    imageMetric->SetFloatingPointCorrectionResolution(1e4);
  }

  //
  // Now do the displacement field transform with gaussian smoothing using
  // the composite transform.
  //

  using RealType = typename AffineRegistrationType::RealType;

  using VectorType = itk::Vector<RealType, VImageDimension>;
  constexpr VectorType zeroVector{};
  using DisplacementFieldType = itk::Image<VectorType, VImageDimension>;
  auto displacementField = DisplacementFieldType::New();
  displacementField->CopyInformation(fixedImage);
  displacementField->SetRegions(fixedImage->GetBufferedRegion());
  displacementField->Allocate();
  displacementField->FillBuffer(zeroVector);

  using DisplacementFieldTransformType =
    itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<RealType, VImageDimension>;

  auto fieldTransform = DisplacementFieldTransformType::New();
  fieldTransform->SetGaussianSmoothingVarianceForTheUpdateField(0);
  fieldTransform->SetGaussianSmoothingVarianceForTheTotalField(1.5);
  fieldTransform->SetDisplacementField(displacementField);

  using DisplacementFieldRegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType>;
  typename DisplacementFieldRegistrationType::Pointer displacementFieldSimple =
    DisplacementFieldRegistrationType::New();
  displacementFieldSimple->SetObjectName("displacementFieldSimple");

  using CorrelationMetricType = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType>;
  auto                                       correlationMetric = CorrelationMetricType::New();
  typename CorrelationMetricType::RadiusType radius;
  radius.Fill(4);
  correlationMetric->SetRadius(radius);
  correlationMetric->SetUseMovingImageGradientFilter(false);
  correlationMetric->SetUseFixedImageGradientFilter(false);

  // correlationMetric->SetUseFloatingPointCorrection(true);
  // correlationMetric->SetFloatingPointCorrectionResolution(1e4);

  using ScalesEstimatorType = itk::RegistrationParameterScalesFromPhysicalShift<CorrelationMetricType>;
  auto scalesEstimator = ScalesEstimatorType::New();
  scalesEstimator->SetMetric(correlationMetric);
  scalesEstimator->SetTransformForward(true);

  auto optimizer = GradientDescentOptimizerv4Type::New();
  optimizer->SetLearningRate(1.0);
#ifdef NDEBUG
  optimizer->SetNumberOfIterations(std::stoi(argv[7]));
#else
  optimizer->SetNumberOfIterations(1);
#endif
  optimizer->SetScalesEstimator(scalesEstimator);
  optimizer->SetDoEstimateLearningRateOnce(false); // true by default
  optimizer->SetDoEstimateLearningRateAtEachIteration(true);

  displacementFieldSimple->SetFixedImage(fixedImage);
  displacementFieldSimple->SetMovingImage(movingImage);
  displacementFieldSimple->SetNumberOfLevels(3);
  displacementFieldSimple->SetMovingInitialTransformInput(affineSimple->GetTransformOutput());
  displacementFieldSimple->SetMetric(correlationMetric);
  displacementFieldSimple->SetOptimizer(optimizer);

  displacementFieldSimple->SetInitialTransform(fieldTransform);
  displacementFieldSimple->InPlaceOn();

  typename DisplacementFieldRegistrationType::OptimizerWeightsType optimizerWeights;
  optimizerWeights.SetSize(VImageDimension);
  optimizerWeights.Fill(0.995);

  displacementFieldSimple->SetOptimizerWeights(optimizerWeights);

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
    itk::GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<DisplacementFieldTransformType>;

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

    typename DisplacementFieldTransformAdaptorType::Pointer fieldTransformAdaptor =
      DisplacementFieldTransformAdaptorType::New();
    fieldTransformAdaptor->SetRequiredSpacing(shrinkFilter->GetOutput()->GetSpacing());
    fieldTransformAdaptor->SetRequiredSize(shrinkFilter->GetOutput()->GetBufferedRegion().GetSize());
    fieldTransformAdaptor->SetRequiredDirection(shrinkFilter->GetOutput()->GetDirection());
    fieldTransformAdaptor->SetRequiredOrigin(shrinkFilter->GetOutput()->GetOrigin());

    adaptors.push_back(fieldTransformAdaptor);
  }
  displacementFieldSimple->SetTransformParametersAdaptorsPerLevel(adaptors);

  using DisplacementFieldRegistrationCommandType = CommandIterationUpdate<DisplacementFieldRegistrationType>;
  typename DisplacementFieldRegistrationCommandType::Pointer displacementFieldObserver =
    DisplacementFieldRegistrationCommandType::New();
  displacementFieldSimple->AddObserver(itk::IterationEvent(), displacementFieldObserver);

  ITK_TRY_EXPECT_NO_EXCEPTION(displacementFieldSimple->Update());


  using ImageMetricType = itk::ImageToImageMetricv4<FixedImageType, MovingImageType>;
  typename ImageMetricType::ConstPointer imageMetric = dynamic_cast<const ImageMetricType *>(affineSimple->GetMetric());
  std::cout << " Affine parameters after registration: " << std::endl
            << affineOptimizer->GetCurrentPosition() << std::endl
            << " Last LearningRate: " << affineOptimizer->GetLearningRate() << std::endl
            << " Use FltPtCorrex: " << imageMetric->GetUseFloatingPointCorrection() << std::endl
            << " FltPtCorrexRes: " << imageMetric->GetFloatingPointCorrectionResolution() << std::endl
            << " Number of work units used:" << std::endl
            << "  metric: " << imageMetric->GetNumberOfWorkUnitsUsed() << std::endl
            << "  optimizer: " << affineOptimizer->GetNumberOfWorkUnits() << std::endl;


  std::cout << "After displacement registration: " << std::endl
            << "Last LearningRate: " << optimizer->GetLearningRate() << std::endl
            << "Use FltPtCorrex: " << correlationMetric->GetUseFloatingPointCorrection() << std::endl
            << "FltPtCorrexRes: " << correlationMetric->GetFloatingPointCorrectionResolution() << std::endl
            << "Number of work units used:" << std::endl
            << "  metric: " << correlationMetric->GetNumberOfWorkUnitsUsed()
            << "  optimizer: " << displacementFieldSimple->GetOptimizer()->GetNumberOfWorkUnits() << std::endl;

  using CompositeTransformType = itk::CompositeTransform<RealType, VImageDimension>;
  auto compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform(affineSimple->GetModifiableTransform());
  compositeTransform->AddTransform(displacementFieldSimple->GetModifiableTransform());

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
  writer->SetFileName(argv[5]);
  writer->SetInput(resampler->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}

int
itkSimpleImageRegistrationTestWithMaskAndSampling(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " pixelType imageDimension fixedImage movingImage outputImage numberOfAffineIterations "
                 "numberOfDeformableIterations"
              << std::endl;
    return EXIT_FAILURE;
  }

  switch (std::stoi(argv[2]))
  {
    case 2:
      if (strcmp(argv[1], "float") == 0)
      {
        return PerformSimpleImageRegistrationWithMaskAndSampling<2, float>(argc, argv);
      }
      else
      {
        return PerformSimpleImageRegistrationWithMaskAndSampling<2, double>(argc, argv);
      }

    case 3:
      if (strcmp(argv[1], "float") == 0)
      {
        return PerformSimpleImageRegistrationWithMaskAndSampling<3, float>(argc, argv);
      }
      else
      {
        return PerformSimpleImageRegistrationWithMaskAndSampling<3, double>(argc, argv);
      }

    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return EXIT_FAILURE;
  }
}
