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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkImageRegistrationMethodv4.h"

#include "itkAffineTransform.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkCenteredTransformInitializer.h"
#include "itkCompositeTransform.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkVector.h"
#include "itkTestingMacros.h"

template <class TComputeType, unsigned int TImageDimension>
class RigidTransformTraits
{
public:
  using TransformType = itk::AffineTransform<TComputeType, TImageDimension>;
};

template <>
class RigidTransformTraits<double, 2>
{
public:
  using TransformType = itk::Euler2DTransform<double>;
};

template <>
class RigidTransformTraits<float, 2>
{
public:
  using TransformType = itk::Euler2DTransform<float>;
};

template <>
class RigidTransformTraits<double, 3>
{
public:
  using TransformType = itk::Euler3DTransform<double>;
};

template <>
class RigidTransformTraits<float, 3>
{
public:
  using TransformType = itk::Euler3DTransform<float>;
};


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
  }
};

template <unsigned int TDimension>
int
PerformCompositeImageRegistration(int itkNotUsed(argc), char * argv[])
{
  const unsigned int ImageDimension = TDimension;

  using PixelType = double;
  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;

  using ImageReaderType = itk::ImageFileReader<FixedImageType>;

  typename ImageReaderType::Pointer fixedImageReader = ImageReaderType::New();
  fixedImageReader->SetFileName(argv[2]);
  fixedImageReader->Update();
  typename FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  fixedImage->Update();
  fixedImage->DisconnectPipeline();

  typename ImageReaderType::Pointer movingImageReader = ImageReaderType::New();
  movingImageReader->SetFileName(argv[3]);
  movingImageReader->Update();
  typename MovingImageType::Pointer movingImage = movingImageReader->GetOutput();
  movingImage->Update();
  movingImage->DisconnectPipeline();

  // Change origins far from (0,0)

  typename FixedImageType::PointType farOrigin(1000.0);
  fixedImage->SetOrigin(farOrigin);
  movingImage->SetOrigin(farOrigin);

  // Set up the centered transform initializer

  using AffineTransformType = itk::AffineTransform<double, ImageDimension>;
  typename AffineTransformType::Pointer initialTransform = AffineTransformType::New();

  using TransformInitializerType =
    itk::CenteredTransformInitializer<AffineTransformType, FixedImageType, MovingImageType>;
  typename TransformInitializerType::Pointer initializer = TransformInitializerType::New();

  initializer->SetTransform(initialTransform);
  initializer->SetFixedImage(fixedImage);
  initializer->SetMovingImage(movingImage);
  initializer->MomentsOn();
  initializer->InitializeTransform();

  using CompositeTransformType = itk::CompositeTransform<double, ImageDimension>;
  typename CompositeTransformType::Pointer compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform(initialTransform);

  // Set up MI metric

  using MIMetricType = itk::JointHistogramMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType>;
  typename MIMetricType::Pointer mutualInformationMetric = MIMetricType::New();
  mutualInformationMetric->SetNumberOfHistogramBins(20);
  mutualInformationMetric->SetUseMovingImageGradientFilter(false);
  mutualInformationMetric->SetUseFixedImageGradientFilter(false);
  mutualInformationMetric->SetUseSampledPointSet(false);
  mutualInformationMetric->SetVirtualDomainFromImage(fixedImage);

  // Set up the and optimize the rigid registration

  using RigidTransformType = typename RigidTransformTraits<double, ImageDimension>::TransformType;

  using RigidRegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, RigidTransformType>;
  typename RigidRegistrationType::Pointer rigidRegistration = RigidRegistrationType::New();

  rigidRegistration->SetFixedImage(fixedImage);
  rigidRegistration->SetMovingImage(movingImage);
  rigidRegistration->SetMetric(mutualInformationMetric);

  rigidRegistration->SetInitializeCenterOfLinearOutputTransform(false);
  rigidRegistration->SetMovingInitialTransform(compositeTransform);
  rigidRegistration->InPlaceOn();

  typename RigidRegistrationType::ShrinkFactorsArrayType rigidShrinkFactorsPerLevel;
  rigidShrinkFactorsPerLevel.SetSize(3);
  rigidShrinkFactorsPerLevel[0] = 4;
  rigidShrinkFactorsPerLevel[1] = 4;
  rigidShrinkFactorsPerLevel[2] = 4;
  rigidRegistration->SetShrinkFactorsPerLevel(rigidShrinkFactorsPerLevel);

  using RigidScalesEstimatorType = itk::RegistrationParameterScalesFromPhysicalShift<MIMetricType>;
  typename RigidScalesEstimatorType::Pointer rigidScalesEstimator = RigidScalesEstimatorType::New();
  rigidScalesEstimator->SetMetric(mutualInformationMetric);
  rigidScalesEstimator->SetTransformForward(true);

  using GradientDescentOptimizerv4Type = itk::GradientDescentOptimizerv4;
  auto * rigidOptimizer = dynamic_cast<GradientDescentOptimizerv4Type *>(rigidRegistration->GetModifiableOptimizer());
  ITK_TEST_EXPECT_TRUE(rigidOptimizer != nullptr);
  rigidOptimizer->SetLearningRate(0.1);
#ifdef NDEBUG
  rigidOptimizer->SetNumberOfIterations(100);
#else
  rigidOptimizer->SetNumberOfIterations(1);
#endif
  rigidOptimizer->SetDoEstimateLearningRateOnce(false); // true by default
  rigidOptimizer->SetDoEstimateLearningRateAtEachIteration(true);
  rigidOptimizer->SetScalesEstimator(rigidScalesEstimator);

  using RigidCommandType = CommandIterationUpdate<RigidRegistrationType>;
  typename RigidCommandType::Pointer rigidObserver = RigidCommandType::New();
  rigidRegistration->AddObserver(itk::MultiResolutionIterationEvent(), rigidObserver);

  try
  {
    std::cout << "Rigid transform" << std::endl;
    rigidRegistration->Update();
  }
  catch (itk::ExceptionObject & e)
  {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
  }
  compositeTransform->AddTransform(rigidRegistration->GetModifiableTransform());

  // Set up and optimize the affine registration

  using AffineRegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, AffineTransformType>;
  typename AffineRegistrationType::Pointer affineRegistration = AffineRegistrationType::New();

  affineRegistration->SetFixedImage(fixedImage);
  affineRegistration->SetMovingImage(movingImage);
  affineRegistration->SetMetric(mutualInformationMetric);

  affineRegistration->SetInitializeCenterOfLinearOutputTransform(true);
  affineRegistration->SetMovingInitialTransform(compositeTransform);
  affineRegistration->InPlaceOn();

  typename AffineRegistrationType::ShrinkFactorsArrayType affineShrinkFactorsPerLevel;
  affineShrinkFactorsPerLevel.SetSize(3);
  affineShrinkFactorsPerLevel[0] = 4;
  affineShrinkFactorsPerLevel[1] = 4;
  affineShrinkFactorsPerLevel[2] = 4;
  affineRegistration->SetShrinkFactorsPerLevel(affineShrinkFactorsPerLevel);

  using AffineScalesEstimatorType = itk::RegistrationParameterScalesFromPhysicalShift<MIMetricType>;
  typename AffineScalesEstimatorType::Pointer affineScalesEstimator = AffineScalesEstimatorType::New();
  affineScalesEstimator->SetMetric(mutualInformationMetric);
  affineScalesEstimator->SetTransformForward(true);

  using GradientDescentOptimizerv4Type = itk::GradientDescentOptimizerv4;
  auto * affineOptimizer = dynamic_cast<GradientDescentOptimizerv4Type *>(affineRegistration->GetModifiableOptimizer());
  ITK_TEST_EXPECT_TRUE(affineOptimizer != nullptr);
  affineOptimizer->SetLearningRate(0.1);
#ifdef NDEBUG
  affineOptimizer->SetNumberOfIterations(100);
#else
  affineOptimizer->SetNumberOfIterations(1);
#endif
  affineOptimizer->SetDoEstimateLearningRateOnce(false); // true by default
  affineOptimizer->SetDoEstimateLearningRateAtEachIteration(true);
  affineOptimizer->SetScalesEstimator(affineScalesEstimator);

  using AffineCommandType = CommandIterationUpdate<AffineRegistrationType>;
  typename AffineCommandType::Pointer affineObserver = AffineCommandType::New();
  affineRegistration->AddObserver(itk::MultiResolutionIterationEvent(), affineObserver);

  try
  {
    std::cout << "Affine transform" << std::endl;
    affineRegistration->Update();
  }
  catch (itk::ExceptionObject & e)
  {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
  }
  compositeTransform->AddTransform(affineRegistration->GetModifiableTransform());

  // Write out resulting images

  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  typename ResampleFilterType::Pointer resampler = ResampleFilterType::New();
  resampler->SetTransform(compositeTransform);
  resampler->SetInput(movingImage);
  resampler->SetSize(fixedImage->GetBufferedRegion().GetSize());
  resampler->SetOutputOrigin(fixedImage->GetOrigin());
  resampler->SetOutputSpacing(fixedImage->GetSpacing());
  resampler->SetOutputDirection(fixedImage->GetDirection());
  resampler->SetDefaultPixelValue(0);
  resampler->Update();

  std::string warpedMovingImageFileName = std::string(argv[4]) + std::string("WarpedMovingImage.nii.gz");

  using WriterType = itk::ImageFileWriter<FixedImageType>;
  typename WriterType::Pointer writer = WriterType::New();
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
  typename InverseWriterType::Pointer inverseWriter = InverseWriterType::New();
  inverseWriter->SetFileName(inverseWarpedFixedImageFileName.c_str());
  inverseWriter->SetInput(inverseResampler->GetOutput());
  inverseWriter->Update();

  return EXIT_SUCCESS;
}

int
itkSimpleImageRegistrationTest3(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cout << argv[0] << " imageDimension fixedImage movingImage outputPrefix" << std::endl;
    exit(1);
  }

  switch (std::stoi(argv[1]))
  {
    case 2:
      return PerformCompositeImageRegistration<2>(argc, argv);

    case 3:
      return PerformCompositeImageRegistration<3>(argc, argv);

    default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit(EXIT_FAILURE);
  }
}
