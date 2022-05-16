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
#include "itkBSplineSyNImageRegistrationMethod.h"

#include "itkAffineTransform.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkCompositeTransform.h"
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
PerformBSplineSyNImageRegistration(int itkNotUsed(argc), char * argv[])
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

  using AffineCommandType = CommandIterationUpdate<AffineRegistrationType>;
  auto affineObserver = AffineCommandType::New();
  affineSimple->AddObserver(itk::IterationEvent(), affineObserver);

  ITK_TRY_EXPECT_NO_EXCEPTION(affineSimple->Update());


  //
  // Now do the b-spline syn displacement field transform
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

  using DisplacementFieldRegistrationType = itk::BSplineSyNImageRegistrationMethod<FixedImageType, MovingImageType>;
  typename DisplacementFieldRegistrationType::Pointer displacementFieldRegistration =
    DisplacementFieldRegistrationType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    displacementFieldRegistration, BSplineSyNImageRegistrationMethod, SyNImageRegistrationMethod);


  typename DisplacementFieldRegistrationType::OptimizerWeightsType optimizerWeights;
  optimizerWeights.SetSize(TDimension);
  optimizerWeights.Fill(0.995);

  displacementFieldRegistration->SetOptimizerWeights(optimizerWeights);

  using OutputTransformType = typename DisplacementFieldRegistrationType::OutputTransformType;
  auto outputTransform = OutputTransformType::New();
  outputTransform->SetDisplacementField(displacementField);
  outputTransform->SetInverseDisplacementField(inverseDisplacementField);

  // Create the transform adaptors

  using DisplacementFieldTransformAdaptorType =
    itk::BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<OutputTransformType>;
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

  typename OutputTransformType::ArrayType updateMeshSize;
  typename OutputTransformType::ArrayType totalMeshSize;
  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    updateMeshSize[d] = 10;
    totalMeshSize[d] = 0;
  }

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

    // A good heuristic is to double the b-spline mesh resolution at each level
    typename OutputTransformType::ArrayType newUpdateMeshSize = updateMeshSize;
    typename OutputTransformType::ArrayType newTotalMeshSize = totalMeshSize;
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      newUpdateMeshSize[d] = newUpdateMeshSize[d] << (level);
      newTotalMeshSize[d] = newTotalMeshSize[d] << (level);
    }
    fieldTransformAdaptor->SetMeshSizeForTheUpdateField(newUpdateMeshSize);
    fieldTransformAdaptor->SetMeshSizeForTheTotalField(newTotalMeshSize);

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
  displacementFieldRegistration->SetLearningRate(std::stod(argv[6]));
  displacementFieldRegistration->SetNumberOfIterationsPerLevel(numberOfIterationsPerLevel);
  displacementFieldRegistration->SetTransformParametersAdaptorsPerLevel(adaptors);

  std::cout << displacementFieldRegistration->GetOptimizerWeights() << std::endl;

  outputTransform->SetDisplacementField(displacementField);
  outputTransform->SetInverseDisplacementField(inverseDisplacementField);
  displacementFieldRegistration->SetInitialTransform(outputTransform);
  displacementFieldRegistration->InPlaceOn();

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
itkBSplineSyNImageRegistrationTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " imageDimension fixedImage movingImage outputPrefix numberOfDeformableIterations learningRate"
              << std::endl;
    return EXIT_FAILURE;
  }

  switch (std::stoi(argv[1]))
  {
    case 2:
      PerformBSplineSyNImageRegistration<2>(argc, argv);
      break;
    case 3:
      PerformBSplineSyNImageRegistration<3>(argc, argv);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
