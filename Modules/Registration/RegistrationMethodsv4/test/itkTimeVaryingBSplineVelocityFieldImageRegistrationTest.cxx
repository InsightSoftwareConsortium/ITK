/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod.h"

#include "itkAffineTransform.h"
#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkCompositeTransform.h"
#include "itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor.h"
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
PerformTimeVaryingBSplineVelocityFieldImageRegistration(int argc, char * argv[])
{

  int  numberOfAffineIterations = 100;
  int  numberOfDeformableIterationsLevel0 = 10;
  int  numberOfDeformableIterationsLevel1 = 20;
  int  numberOfDeformableIterationsLevel2 = 11;
  auto learningRate = static_cast<double>(0.5);

  if (argc >= 6)
  {
    numberOfAffineIterations = std::stoi(argv[5]);
  }
  if (argc >= 7)
  {
    numberOfDeformableIterationsLevel0 = std::stoi(argv[6]);
  }
  if (argc >= 8)
  {
    numberOfDeformableIterationsLevel1 = std::stoi(argv[7]);
  }
  if (argc >= 9)
  {
    numberOfDeformableIterationsLevel2 = std::stoi(argv[8]);
  }
  if (argc >= 10)
  {
    learningRate = std::stod(argv[9]);
  }

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

  using AffineTransformType = itk::AffineTransform<double, ImageDimension>;
  using AffineRegistrationType = itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType, AffineTransformType>;
  typename AffineRegistrationType::Pointer affineSimple = AffineRegistrationType::New();
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
  using GradientDescentOptimizerType = itk::GradientDescentOptimizerv4;
  auto * optimizer = dynamic_cast<GradientDescentOptimizerType *>(affineSimple->GetModifiableOptimizer());
  ITK_TEST_EXPECT_TRUE(optimizer != nullptr);
  optimizer->SetNumberOfIterations(numberOfAffineIterations);
  std::cout << "number of affine iterations: " << numberOfAffineIterations << std::endl;

  using AffineCommandType = CommandIterationUpdate<AffineRegistrationType>;
  typename AffineCommandType::Pointer affineObserver = AffineCommandType::New();
  affineSimple->AddObserver(itk::IterationEvent(), affineObserver);

  try
  {
    std::cout << "Affine transform" << std::endl;
    affineSimple->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
  }

  //
  // Now do the displacement field transform with gaussian smoothing using
  // the composite transform.
  //

  using RealType = typename AffineRegistrationType::RealType;

  using CompositeTransformType = itk::CompositeTransform<RealType, ImageDimension>;
  typename CompositeTransformType::Pointer compositeTransform = CompositeTransformType::New();
  compositeTransform->AddTransform(affineSimple->GetModifiableTransform());

  using AffineResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  typename AffineResampleFilterType::Pointer affineResampler = AffineResampleFilterType::New();
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
  typename AffineWriterType::Pointer affineWriter = AffineWriterType::New();
  affineWriter->SetFileName(affineMovingImageFileName.c_str());
  affineWriter->SetInput(affineResampler->GetOutput());
  affineWriter->Update();

  using CorrelationMetricType = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<FixedImageType, MovingImageType>;
  typename CorrelationMetricType::Pointer    correlationMetric = CorrelationMetricType::New();
  typename CorrelationMetricType::RadiusType radius;
  radius.Fill(4);
  correlationMetric->SetRadius(radius);
  correlationMetric->SetUseMovingImageGradientFilter(false);
  correlationMetric->SetUseFixedImageGradientFilter(false);

  using VelocityFieldRegistrationType =
    itk::TimeVaryingBSplineVelocityFieldImageRegistrationMethod<FixedImageType, MovingImageType>;
  typename VelocityFieldRegistrationType::Pointer velocityFieldRegistration = VelocityFieldRegistrationType::New();

  using OutputTransformType = typename VelocityFieldRegistrationType::OutputTransformType;
  typename OutputTransformType::Pointer outputTransform = OutputTransformType::New();
  velocityFieldRegistration->SetInitialTransform(outputTransform);
  velocityFieldRegistration->InPlaceOn();

  velocityFieldRegistration->SetFixedImage(fixedImage);
  velocityFieldRegistration->SetMovingImage(movingImage);
  velocityFieldRegistration->SetNumberOfLevels(3);
  velocityFieldRegistration->SetMovingInitialTransform(compositeTransform);
  velocityFieldRegistration->SetMetric(correlationMetric);
  velocityFieldRegistration->SetLearningRate(learningRate);
  std::cout << "learningRate: " << learningRate << std::endl;
  outputTransform->SetSplineOrder(3);
  outputTransform->SetLowerTimeBound(0.0);
  outputTransform->SetUpperTimeBound(1.0);

  typename VelocityFieldRegistrationType::ShrinkFactorsArrayType numberOfIterationsPerLevel;
  numberOfIterationsPerLevel.SetSize(3);
  numberOfIterationsPerLevel[0] = numberOfDeformableIterationsLevel0;
  numberOfIterationsPerLevel[1] = numberOfDeformableIterationsLevel1;
  numberOfIterationsPerLevel[2] = numberOfDeformableIterationsLevel2;
  velocityFieldRegistration->SetNumberOfIterationsPerLevel(numberOfIterationsPerLevel);
  std::cout << "iterations per level: " << numberOfIterationsPerLevel[0] << ", " << numberOfIterationsPerLevel[1]
            << ", " << numberOfIterationsPerLevel[2] << std::endl;

  typename VelocityFieldRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize(3);
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;
  velocityFieldRegistration->SetShrinkFactorsPerLevel(shrinkFactorsPerLevel);

  typename VelocityFieldRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize(3);
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 0;
  velocityFieldRegistration->SetSmoothingSigmasPerLevel(smoothingSigmasPerLevel);

  using VectorType = itk::Vector<RealType, ImageDimension>;
  using TimeVaryingVelocityFieldControlPointLatticeType = itk::Image<VectorType, ImageDimension + 1>;
  typename TimeVaryingVelocityFieldControlPointLatticeType::Pointer velocityFieldLattice =
    TimeVaryingVelocityFieldControlPointLatticeType::New();

  // Determine the parameters (size, spacing, etc) for the time-varying velocity field

  typename FixedImageType::SizeType      fixedImageSize = fixedImage->GetBufferedRegion().GetSize();
  typename FixedImageType::PointType     fixedImageOrigin = fixedImage->GetOrigin();
  typename FixedImageType::SpacingType   fixedImageSpacing = fixedImage->GetSpacing();
  typename FixedImageType::DirectionType fixedImageDirection = fixedImage->GetDirection();

  typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType      transformDomainMeshSize;
  typename TimeVaryingVelocityFieldControlPointLatticeType::PointType     transformDomainOrigin;
  typename TimeVaryingVelocityFieldControlPointLatticeType::SpacingType   transformDomainSpacing;
  typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType      transformDomainSize;
  typename TimeVaryingVelocityFieldControlPointLatticeType::DirectionType transformDomainDirection;

  transformDomainDirection.SetIdentity();
  transformDomainOrigin.Fill(0.0);
  transformDomainMeshSize.Fill(4);
  transformDomainSpacing.Fill(1.0);
  transformDomainSize.Fill(10);

  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    transformDomainOrigin[i] = fixedImageOrigin[i];
    transformDomainMeshSize[i] = 3;
    transformDomainSpacing[i] = fixedImageSpacing[i];
    transformDomainSize[i] = fixedImageSize[i];
    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      transformDomainDirection[i][j] = fixedImageDirection[i][j];
    }
  }

  using TransformType = typename VelocityFieldRegistrationType::OutputTransformType;

  using VelocityFieldTransformAdaptorType =
    itk::TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TransformType>;
  typename VelocityFieldTransformAdaptorType::Pointer initialFieldTransformAdaptor =
    VelocityFieldTransformAdaptorType::New();
  initialFieldTransformAdaptor->SetSplineOrder(outputTransform->GetSplineOrder());
  initialFieldTransformAdaptor->SetRequiredTransformDomainOrigin(transformDomainOrigin);
  initialFieldTransformAdaptor->SetRequiredTransformDomainSpacing(transformDomainSpacing);
  initialFieldTransformAdaptor->SetRequiredTransformDomainSize(transformDomainSize);
  initialFieldTransformAdaptor->SetRequiredTransformDomainMeshSize(transformDomainMeshSize);
  initialFieldTransformAdaptor->SetRequiredTransformDomainDirection(transformDomainDirection);

  VectorType zeroVector(0.0);

  velocityFieldLattice->SetOrigin(initialFieldTransformAdaptor->GetRequiredControlPointLatticeOrigin());
  velocityFieldLattice->SetSpacing(initialFieldTransformAdaptor->GetRequiredControlPointLatticeSpacing());
  velocityFieldLattice->SetDirection(initialFieldTransformAdaptor->GetRequiredControlPointLatticeDirection());
  velocityFieldLattice->SetRegions(initialFieldTransformAdaptor->GetRequiredControlPointLatticeSize());
  velocityFieldLattice->Allocate();
  velocityFieldLattice->FillBuffer(zeroVector);

  typename TransformType::VelocityFieldPointType     velocityFieldOrigin;
  typename TransformType::VelocityFieldSpacingType   velocityFieldSpacing;
  typename TransformType::VelocityFieldSizeType      velocityFieldSize;
  typename TransformType::VelocityFieldDirectionType velocityFieldDirection;

  velocityFieldOrigin.Fill(0.0);
  velocityFieldSpacing.Fill(1.0);
  velocityFieldSize.Fill(velocityFieldRegistration->GetNumberOfTimePointSamples());
  velocityFieldDirection.SetIdentity();

  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    velocityFieldOrigin[i] = fixedImage->GetOrigin()[i];
    velocityFieldSpacing[i] = fixedImage->GetSpacing()[i];
    velocityFieldSize[i] = fixedImage->GetRequestedRegion().GetSize()[i];
    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      velocityFieldDirection[i][j] = fixedImage->GetDirection()[i][j];
    }
  }

  outputTransform->SetTimeVaryingVelocityFieldControlPointLattice(velocityFieldLattice);
  outputTransform->SetVelocityFieldOrigin(velocityFieldOrigin);
  outputTransform->SetVelocityFieldDirection(velocityFieldDirection);
  outputTransform->SetVelocityFieldSpacing(velocityFieldSpacing);
  outputTransform->SetVelocityFieldSize(velocityFieldSize);
  outputTransform->IntegrateVelocityField();

  typename VelocityFieldRegistrationType::TransformParametersAdaptorsContainerType adaptors;

  for (unsigned int level = 0; level < shrinkFactorsPerLevel.Size(); level++)
  {
    using ShrinkFilterType = itk::ShrinkImageFilter<FixedImageType, FixedImageType>;
    typename ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors(shrinkFactorsPerLevel[level]);
    shrinkFilter->SetInput(fixedImage);
    shrinkFilter->Update();

    // Although we shrink the images for the given levels,
    // we keep the size in time the same

    velocityFieldSize.Fill(10);
    velocityFieldOrigin.Fill(0.0);
    velocityFieldSpacing.Fill(1.0);
    velocityFieldDirection.SetIdentity();

    fixedImageSize = shrinkFilter->GetOutput()->GetBufferedRegion().GetSize();
    fixedImageOrigin = shrinkFilter->GetOutput()->GetOrigin();
    fixedImageSpacing = shrinkFilter->GetOutput()->GetSpacing();
    fixedImageDirection = shrinkFilter->GetOutput()->GetDirection();

    for (unsigned int i = 0; i < ImageDimension; i++)
    {
      velocityFieldSize[i] = fixedImageSize[i];
      velocityFieldOrigin[i] = fixedImageOrigin[i];
      velocityFieldSpacing[i] = fixedImageSpacing[i];

      transformDomainMeshSize[i] <<= 1;

      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        velocityFieldDirection[i][j] = fixedImageDirection[i][j];
      }
    }

    typename VelocityFieldTransformAdaptorType::Pointer fieldTransformAdaptor =
      VelocityFieldTransformAdaptorType::New();
    fieldTransformAdaptor->SetSplineOrder(outputTransform->GetSplineOrder());
    fieldTransformAdaptor->SetRequiredTransformDomainSpacing(velocityFieldSpacing);
    fieldTransformAdaptor->SetRequiredTransformDomainSize(velocityFieldSize);
    fieldTransformAdaptor->SetRequiredTransformDomainDirection(velocityFieldDirection);
    fieldTransformAdaptor->SetRequiredTransformDomainOrigin(velocityFieldOrigin);
    fieldTransformAdaptor->SetRequiredTransformDomainMeshSize(transformDomainMeshSize);

    adaptors.push_back(fieldTransformAdaptor);
  }
  velocityFieldRegistration->SetTransformParametersAdaptorsPerLevel(adaptors);

  using VelocityFieldRegistrationCommandType = CommandIterationUpdate<VelocityFieldRegistrationType>;
  typename VelocityFieldRegistrationCommandType::Pointer displacementFieldObserver =
    VelocityFieldRegistrationCommandType::New();
  velocityFieldRegistration->AddObserver(itk::IterationEvent(), displacementFieldObserver);

  try
  {
    std::cout << "Time-varying B-spline velocity field transform" << std::endl;
    velocityFieldRegistration->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
  }

  compositeTransform->AddTransform(outputTransform);

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

  std::string warpedMovingImageFileName =
    std::string(argv[4]) + std::string("MovingImageAfterVelocityFieldTransform.nii.gz");

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

  std::string velocityFieldLatticeFileName =
    std::string(argv[4]) + std::string("VelocityFieldControlPointLattice.nii.gz");

  using VelocityFieldWriterType = itk::ImageFileWriter<TimeVaryingVelocityFieldControlPointLatticeType>;
  typename VelocityFieldWriterType::Pointer velocityFieldLatticeWriter = VelocityFieldWriterType::New();
  velocityFieldLatticeWriter->SetFileName(velocityFieldLatticeFileName.c_str());
  velocityFieldLatticeWriter->SetInput(outputTransform->GetTimeVaryingVelocityFieldControlPointLattice());
  velocityFieldLatticeWriter->Update();

  return EXIT_SUCCESS;
}

int
itkTimeVaryingBSplineVelocityFieldImageRegistrationTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cout << itkNameOfTestExecutableMacro(argv)
              << " imageDimension fixedImage movingImage outputPrefix [numberOfAffineIterations = 100] "
              << "[numberOfDeformableIterationsLevel0 = 10] [numberOfDeformableIterationsLevel1 = 20] "
                 "[numberOfDeformableIterationsLevel2 = 11 ] [learningRate = 0.5]"
              << std::endl;
    exit(1);
  }

  switch (std::stoi(argv[1]))
  {
    case 2:
      PerformTimeVaryingBSplineVelocityFieldImageRegistration<2>(argc, argv);
      break;
    case 3:
      PerformTimeVaryingBSplineVelocityFieldImageRegistration<3>(argc, argv);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit(EXIT_FAILURE);
  }
  return EXIT_SUCCESS;
}
