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

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{BSplineTransform}
// class in a multi-resolution scheme. Here we run 3 levels of resolutions.
// The first level of registration is performed with the spline grid of
// low resolution. Then, a common practice is to increase the resolution
// of the B-spline mesh (or, analogously, the control point grid size)
// at each level.
//
// For this purpose, we introduce the concept of transform adaptors.
// Each level of each stage is defined by a transform adaptor
// which describes how to adapt the transform to the current level by
// increasing the resolution from the previous level.
// Here, we used \doxygen{BSplineTransformParametersAdaptor} class
// to adapt the BSpline transform parameters at each resolution level.
// Note that for many transforms, such as affine, the
// concept of an adaptor may be nonsensical since the number of transform
// parameters does not change between resolution levels.
//
// This examples use the \doxygen{LBFGS2Optimizerv4}, which is the new
// implementation of the quasi-Newtown unbounded limited-memory
// Broyden Fletcher Goldfarb Shannon (LBFGS) optimizer. The unbounded
// version does not require specification of the bounds of the
// parameters space, since the number of parameters change at each
// B-Spline resolution this implementation is preferred.
//
// Since this example is quite similar to the previous example on the use
// of the \code{BSplineTransform} we omit most of the details already
// discussed and will focus on the aspects related to the multi-resolution
// approach.
//
// \index{itk::BSplineTransform}
// \index{itk::BSplineTransform!DeformableRegistration}
// \index{itk::LBFGS2Optimizerv4}
// \index{itk::BSplineTransformParametersAdaptor}
//
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethodv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"

//  Software Guide : BeginLatex
//
//  We include the header files for the transform, optimizer and adaptor.
//
//  \index{itk::BSplineTransform!header}
//  \index{itk::LBFGS2Optimizer!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBSplineTransform.h"
#include "itkLBFGS2Optimizerv4.h"
#include "itkBSplineTransformParametersAdaptor.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"

#include "itkIdentityTransform.h"

#include "itkBSplineTransformInitializer.h"
#include "itkTransformToDisplacementFieldFilter.h"


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
  using OptimizerType = itk::LBFGS2Optimizerv4;

  using OptimizerPointer = const OptimizerType *;

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    auto optimizer = static_cast<OptimizerPointer>(object);

    if (!itk::IterationEvent().CheckEvent(&event))
    {
      return;
    }

    std::cout << optimizer->GetCurrentIteration() << " = ";
    std::cout << optimizer->GetValue() << std::endl;
    std::cout << "\t" << optimizer->GetCurrentGradientNorm() << " "
              << optimizer->GetCurrentParameterNorm() << " "
              << optimizer->GetCurrentStepSize() << std::endl;
  }
};


int
main(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile  ";
    std::cerr << " [differenceOutputfile] [differenceBeforeRegistration] ";
    std::cerr << " [deformationField] ";
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;

  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;


  //  Software Guide : BeginLatex
  //
  //  We instantiate the type of the \code{BSplineTransform} using
  //  as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the BSpline.
  //
  //  \index{BSplineTransform!New}
  //  \index{BSplineTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int     SpaceDimension = ImageDimension;
  constexpr unsigned int SplineOrder = 3;
  using CoordinateRepType = double;

  using TransformType =
    itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder>;
  // Software Guide : EndCodeSnippet


  using OptimizerType = itk::LBFGS2Optimizerv4;


  using MetricType =
    itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType>;

  using RegistrationType =
    itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType>;

  auto metric = MetricType::New();
  auto optimizer = OptimizerType::New();
  auto registration = RegistrationType::New();


  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImageReader->GetOutput());

  fixedImageReader->Update();

  //  Software Guide : BeginLatex
  //
  //  We construct the transform object, initialize its parameters and
  //  connect that to the registration object.
  //
  //  \index{itk::RegistrationMethod!SetTransform()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  auto outputBSplineTransform = TransformType::New();

  // Initialize the fixed parameters of transform (grid size, etc).
  //
  using InitializerType =
    itk::BSplineTransformInitializer<TransformType, FixedImageType>;

  auto transformInitializer = InitializerType::New();

  unsigned int numberOfGridNodesInOneDimension = 8;

  TransformType::MeshSizeType meshSize;
  meshSize.Fill(numberOfGridNodesInOneDimension - SplineOrder);

  transformInitializer->SetTransform(outputBSplineTransform);
  transformInitializer->SetImage(fixedImage);
  transformInitializer->SetTransformDomainMeshSize(meshSize);
  transformInitializer->InitializeTransform();

  // Set transform to identity
  //
  using ParametersType = TransformType::ParametersType;
  const unsigned int numberOfParameters =
    outputBSplineTransform->GetNumberOfParameters();
  ParametersType parameters(numberOfParameters);
  parameters.Fill(0.0);
  outputBSplineTransform->SetParameters(parameters);

  registration->SetInitialTransform(outputBSplineTransform);
  registration->InPlaceOn();
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The registration process is run in three levels. The shrink factors
  //  and smoothing sigmas are set for each level.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  constexpr unsigned int numberOfLevels = 3;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize(numberOfLevels);
  shrinkFactorsPerLevel[0] = 3;
  shrinkFactorsPerLevel[1] = 2;
  shrinkFactorsPerLevel[2] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize(numberOfLevels);
  smoothingSigmasPerLevel[0] = 2;
  smoothingSigmasPerLevel[1] = 1;
  smoothingSigmasPerLevel[2] = 0;

  registration->SetNumberOfLevels(numberOfLevels);
  registration->SetSmoothingSigmasPerLevel(smoothingSigmasPerLevel);
  registration->SetShrinkFactorsPerLevel(shrinkFactorsPerLevel);
  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Create the transform adaptors to modify the flexibility
  //  of the deformable transform for each level of this
  //  multi-resolution scheme.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  RegistrationType::TransformParametersAdaptorsContainerType adaptors;

  // First, get fixed image physical dimensions
  TransformType::PhysicalDimensionsType fixedPhysicalDimensions;
  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    fixedPhysicalDimensions[i] =
      fixedImage->GetSpacing()[i] *
      static_cast<double>(
        fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1);
  }

  // Create the transform adaptors specific to B-splines
  for (unsigned int level = 0; level < numberOfLevels; ++level)
  {
    using ShrinkFilterType =
      itk::ShrinkImageFilter<FixedImageType, FixedImageType>;
    auto shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors(shrinkFactorsPerLevel[level]);
    shrinkFilter->SetInput(fixedImage);
    shrinkFilter->Update();

    // A good heuristic is to double the b-spline mesh resolution at each
    // level
    //
    TransformType::MeshSizeType requiredMeshSize;
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      requiredMeshSize[d] = meshSize[d] << level;
    }

    using BSplineAdaptorType =
      itk::BSplineTransformParametersAdaptor<TransformType>;
    auto bsplineAdaptor = BSplineAdaptorType::New();
    bsplineAdaptor->SetTransform(outputBSplineTransform);
    bsplineAdaptor->SetRequiredTransformDomainMeshSize(requiredMeshSize);
    bsplineAdaptor->SetRequiredTransformDomainOrigin(
      shrinkFilter->GetOutput()->GetOrigin());
    bsplineAdaptor->SetRequiredTransformDomainDirection(
      shrinkFilter->GetOutput()->GetDirection());
    bsplineAdaptor->SetRequiredTransformDomainPhysicalDimensions(
      fixedPhysicalDimensions);

    adaptors.push_back(bsplineAdaptor);
  }

  registration->SetTransformParametersAdaptorsPerLevel(adaptors);
  //  Software Guide : EndCodeSnippet

  // Scale estimator
  using ScalesEstimatorType =
    itk::RegistrationParameterScalesFromPhysicalShift<MetricType>;
  auto scalesEstimator = ScalesEstimatorType::New();
  scalesEstimator->SetMetric(metric);
  scalesEstimator->SetTransformForward(true);
  scalesEstimator->SetSmallParameterVariation(1.0);

  // Set Optimizer
  optimizer->SetScalesEstimator(scalesEstimator);
  optimizer->SetSolutionAccuracy(1e-4);
  optimizer->SetHessianApproximationAccuracy(5);
  optimizer->SetMaximumIterations(100);
  optimizer->SetMaximumLineSearchEvaluations(10);

  // Connect an observer
  auto observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  std::cout << "Starting Registration " << std::endl;

  try
  {
    registration->Update();
    std::cout << "Optimizer stop condition = "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // Finally we use the last transform in order to resample the image.
  //
  using ResampleFilterType =
    itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  auto resample = ResampleFilterType::New();

  resample->SetTransform(outputBSplineTransform);
  resample->SetInput(movingImageReader->GetOutput());

  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(100);

  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  using CastFilterType =
    itk::CastImageFilter<FixedImageType, OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;


  auto writer = WriterType::New();
  auto caster = CastFilterType::New();


  writer->SetFileName(argv[3]);


  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());


  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  using DifferenceFilterType =
    itk::SquaredDifferenceImageFilter<FixedImageType,
                                      FixedImageType,
                                      OutputImageType>;

  auto difference = DifferenceFilterType::New();

  auto writer2 = WriterType::New();
  writer2->SetInput(difference->GetOutput());


  // Compute the difference image between the
  // fixed and resampled moving image.
  if (argc >= 5)
  {
    difference->SetInput1(fixedImageReader->GetOutput());
    difference->SetInput2(resample->GetOutput());
    writer2->SetFileName(argv[4]);
    try
    {
      writer2->Update();
    }
    catch (const itk::ExceptionObject & err)
    {
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
    }
  }


  // Compute the difference image between the
  // fixed and moving image before registration.
  if (argc >= 6)
  {
    writer2->SetFileName(argv[5]);
    difference->SetInput1(fixedImageReader->GetOutput());
    difference->SetInput2(movingImageReader->GetOutput());
    try
    {
      writer2->Update();
    }
    catch (const itk::ExceptionObject & err)
    {
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Generate the explicit deformation field resulting from
  // the registration.
  using VectorPixelType = itk::Vector<float, ImageDimension>;
  using DisplacementFieldImageType =
    itk::Image<VectorPixelType, ImageDimension>;

  using DisplacementFieldGeneratorType =
    itk::TransformToDisplacementFieldFilter<DisplacementFieldImageType,
                                            CoordinateRepType>;

  /** Create an setup displacement field generator. */
  auto dispfieldGenerator = DisplacementFieldGeneratorType::New();
  dispfieldGenerator->UseReferenceImageOn();
  dispfieldGenerator->SetReferenceImage(fixedImage);
  dispfieldGenerator->SetTransform(outputBSplineTransform);
  try
  {
    dispfieldGenerator->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "Exception detected while generating deformation field";
    std::cerr << " : " << err << std::endl;
    return EXIT_FAILURE;
  }

  using FieldWriterType = itk::ImageFileWriter<DisplacementFieldImageType>;
  auto fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput(dispfieldGenerator->GetOutput());

  if (argc >= 7)
  {
    fieldWriter->SetFileName(argv[6]);
    try
    {
      fieldWriter->Update();
    }
    catch (const itk::ExceptionObject & excp)
    {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
