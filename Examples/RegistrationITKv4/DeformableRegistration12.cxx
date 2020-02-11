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

// Software Guide : BeginLatex
//
// This example illustrates the use of the
// \doxygen{BSplineTransform} class for performing
// registration of two $2D$ images. The example code is for the most
// part identical to the code presented in
// Section~\ref{sec:DeformableRegistration8}.  The major difference is
// that this example we set the image dimension to 2.
//
// \index{itk::BSplineTransform}
// \index{itk::BSplineTransform!DeformableRegistration}
// \index{itk::LBFGSBOptimizerv4}
//
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethodv4.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"

#include "itkTimeProbesCollectorBase.h"
#include "itkMemoryProbesCollectorBase.h"


//  Software Guide : BeginLatex
//
//  The following are the most relevant headers to this example.
//
//  \index{itk::BSplineTransform!header}
//  \index{itk::LBFGSBOptimizerv4!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBSplineTransform.h"
#include "itkLBFGSBOptimizerv4.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The parameter space of the \code{BSplineTransform} is composed by
//  the set of all the deformations associated with the nodes of the BSpline
//  grid.  This large number of parameters makes possible to represent a wide
//  variety of deformations, but it also has the price of requiring a
//  significant amount of computation time.
//
//  \index{itk::BSplineTransform!header}
//
//  Software Guide : EndLatex

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"

//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
//
#include "itkCommand.h"
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
  using OptimizerType = itk::LBFGSBOptimizerv4;
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
    if (!(itk::IterationEvent().CheckEvent(&event)))
    {
      return;
    }
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetCurrentMetricValue() << "   ";
    std::cout << optimizer->GetInfinityNormOfProjectedGradient() << std::endl;
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
    std::cerr << " [filenameForFinalTransformParameters] ";
    std::cerr << " [numberOfGridNodesInOneDimension] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;

  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;


  //  Software Guide : BeginLatex
  //
  //  We instantiate now the type of the \code{BSplineTransform} using
  //  as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the BSpline.
  //
  //  \index{BSplineTransform!New}
  //  \index{BSplineTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  FixedImageReaderType::Pointer fixedImageReader = FixedImageReaderType::New();
  fixedImageReader->SetFileName(argv[1]);
  fixedImageReader->Update();
  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();
  FixedImageType::RegionType   fixedRegion = fixedImage->GetBufferedRegion();

  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();
  movingImageReader->SetFileName(argv[2]);
  movingImageReader->Update();
  MovingImageType::ConstPointer movingImage = movingImageReader->GetOutput();

  // Software Guide : BeginCodeSnippet
  const unsigned int     SpaceDimension = ImageDimension;
  constexpr unsigned int SplineOrder = 3;
  using CoordinateRepType = double;

  using TransformType =
    itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder>;
  // Software Guide : EndCodeSnippet

  using RegistrationType =
    itk::ImageRegistrationMethodv4<FixedImageType, MovingImageType>;
  RegistrationType::Pointer registration = RegistrationType::New();

  //  Software Guide : BeginLatex
  //
  //  Final BSpline transform will be the output of the registration method.
  //
  //  Software Guide : EndLatex
  //
  // Software Guide : BeginCodeSnippet
  TransformType::Pointer transform = TransformType::New();
  // Software Guide : EndCodeSnippet

  unsigned int numberOfGridNodesInOneDimension = 7;

  if (argc > 8)
  {
    numberOfGridNodesInOneDimension = std::stoi(argv[8]);
  }

  // Software Guide : BeginCodeSnippet
  TransformType::PhysicalDimensionsType fixedPhysicalDimensions;
  TransformType::MeshSizeType           meshSize;
  TransformType::OriginType             fixedOrigin;

  for (unsigned int i = 0; i < SpaceDimension; i++)
  {
    fixedOrigin[i] = fixedImage->GetOrigin()[i];
    fixedPhysicalDimensions[i] =
      fixedImage->GetSpacing()[i] *
      static_cast<double>(fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1);
  }
  meshSize.Fill(numberOfGridNodesInOneDimension - SplineOrder);

  transform->SetTransformDomainOrigin(fixedOrigin);
  transform->SetTransformDomainPhysicalDimensions(fixedPhysicalDimensions);
  transform->SetTransformDomainMeshSize(meshSize);
  transform->SetTransformDomainDirection(fixedImage->GetDirection());

  registration->SetInitialTransform(transform);
  registration->InPlaceOn();

  using ParametersType = TransformType::ParametersType;

  const unsigned int numberOfParameters = transform->GetNumberOfParameters();

  ParametersType parameters(numberOfParameters);

  parameters.Fill(0.0);

  transform->SetParameters(parameters);
  //  Software Guide : EndCodeSnippet

  using MetricType =
    itk::MattesMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType>;
  MetricType::Pointer metric = MetricType::New();
  metric->SetNumberOfHistogramBins(32);
  metric->SetUseMovingImageGradientFilter(false);
  metric->SetUseFixedImageGradientFilter(false);
  metric->SetUseSampledPointSet(false);

  using OptimizerType = itk::LBFGSBOptimizerv4;
  OptimizerType::Pointer optimizer = OptimizerType::New();

  // Software Guide : BeginCodeSnippet
  const unsigned int                numParameters = transform->GetNumberOfParameters();
  OptimizerType::BoundSelectionType boundSelect(numParameters);
  OptimizerType::BoundValueType     upperBound(numParameters);
  OptimizerType::BoundValueType     lowerBound(numParameters);

  boundSelect.Fill(0);
  upperBound.Fill(0.0);
  lowerBound.Fill(0.0);

  optimizer->SetBoundSelection(boundSelect);
  optimizer->SetUpperBound(upperBound);
  optimizer->SetLowerBound(lowerBound);

  optimizer->SetCostFunctionConvergenceFactor(1.e7);
  optimizer->SetGradientConvergenceTolerance(1e-35);
  optimizer->SetNumberOfIterations(200);
  optimizer->SetMaximumNumberOfFunctionEvaluations(200);
  optimizer->SetMaximumNumberOfCorrections(7);
  // Software Guide : EndCodeSnippet

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  // One level registration is performed using the shrink factor 1 and smoothing sigma 1
  //
  constexpr unsigned int numberOfLevels = 1;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize(1);
  shrinkFactorsPerLevel[0] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize(1);
  smoothingSigmasPerLevel[0] = 0;

  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImage);
  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetNumberOfLevels(numberOfLevels);
  registration->SetSmoothingSigmasPerLevel(smoothingSigmasPerLevel);
  registration->SetShrinkFactorsPerLevel(shrinkFactorsPerLevel);


  // Add time and memory probes
  itk::TimeProbesCollectorBase   chronometer;
  itk::MemoryProbesCollectorBase memorymeter;

  std::cout << std::endl << "Starting Registration" << std::endl;

  try
  {
    memorymeter.Start("Registration");
    chronometer.Start("Registration");

    registration->Update();

    chronometer.Stop("Registration");
    memorymeter.Stop("Registration");

    const OptimizerType::ConstPointer outputOptimizer =
      dynamic_cast<const OptimizerType *>(registration->GetOptimizer());
    if (outputOptimizer.IsNotNull())
    {
      std::cout << "Optimizer stop condition = "
                << outputOptimizer->GetStopConditionDescription() << std::endl;
    }
    else
    {
      std::cerr << "Output optimizer is null." << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // While the registration filter is run, it updates the output transform parameters
  // with the final registration parameters
  OptimizerType::ParametersType finalParameters = transform->GetParameters();

  // Report the time and memory taken by the registration
  chronometer.Report(std::cout);
  memorymeter.Report(std::cout);

  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform(transform);
  resample->SetInput(movingImageReader->GetOutput());

  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());

  // This value is set to zero in order to make easier to perform
  // regression testing in this example. However, for didactic
  // exercise it will be better to set it to a medium gray value
  // such as 100 or 128.
  resample->SetDefaultPixelValue(0);

  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  using CastFilterType = itk::CastImageFilter<FixedImageType, OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;


  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();


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
    itk::SquaredDifferenceImageFilter<FixedImageType, FixedImageType, OutputImageType>;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput(difference->GetOutput());


  // Compute the difference image between the
  // fixed and resampled moving image.
  if (argc > 4)
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
  if (argc > 5)
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
  if (argc > 6)
  {

    using VectorType = itk::Vector<float, ImageDimension>;
    using DisplacementFieldType = itk::Image<VectorType, ImageDimension>;

    DisplacementFieldType::Pointer field = DisplacementFieldType::New();
    field->SetRegions(fixedRegion);
    field->SetOrigin(fixedImage->GetOrigin());
    field->SetSpacing(fixedImage->GetSpacing());
    field->SetDirection(fixedImage->GetDirection());
    field->Allocate();

    using FieldIterator = itk::ImageRegionIterator<DisplacementFieldType>;
    FieldIterator fi(field, fixedRegion);

    fi.GoToBegin();

    TransformType::InputPointType    fixedPoint;
    TransformType::OutputPointType   movingPoint;
    DisplacementFieldType::IndexType index;

    VectorType displacement;

    while (!fi.IsAtEnd())
    {
      index = fi.GetIndex();
      field->TransformIndexToPhysicalPoint(index, fixedPoint);
      movingPoint = transform->TransformPoint(fixedPoint);
      displacement = movingPoint - fixedPoint;
      fi.Set(displacement);
      ++fi;
    }

    using FieldWriterType = itk::ImageFileWriter<DisplacementFieldType>;
    FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

    fieldWriter->SetInput(field);

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

  // Optionally, save the transform parameters in a file
  if (argc > 7)
  {
    std::ofstream parametersFile;
    parametersFile.open(argv[7]);
    parametersFile << finalParameters << std::endl;
    parametersFile.close();
  }

  return EXIT_SUCCESS;
}
