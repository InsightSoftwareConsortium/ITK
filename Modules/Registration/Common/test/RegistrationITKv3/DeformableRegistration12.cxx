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
// \index{itk::LBFGSBOptimizer}
//
//

#include "itkImageRegistrationMethod.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

#include "itkTimeProbesCollectorBase.h"
#include "itkMemoryProbesCollectorBase.h"


//
//  The following are the most relevant headers to this example.
//
//  \index{itk::BSplineTransform!header}
//  \index{itk::LBFGSBOptimizer!header}
//

#include "itkBSplineTransform.h"
#include "itkLBFGSBOptimizer.h"

//
//  The parameter space of the \code{BSplineTransform} is composed by
//  the set of all the deformations associated with the nodes of the BSpline
//  grid.  This large number of parameters makes possible to represent a wide
//  variety of deformations, but it also has the price of requiring a
//  significant amount of computation time.
//
//  \index{itk::BSplineTransform!header}
//

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
  using OptimizerType = itk::LBFGSBOptimizer;
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
    std::cout << optimizer->GetCachedValue() << "   ";
    std::cout << optimizer->GetInfinityNormOfProjectedGradient() << std::endl;
  }
};

#include "itkTestDriverIncludeRequiredIOFactories.h"
int
main(int argc, char * argv[])
{
  RegisterRequiredFactories();
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile  ";
    std::cerr << " [differenceOutputfile] [differenceBeforeRegistration] ";
    std::cerr << " [deformationField] ";
    std::cerr << " [useExplicitPDFderivatives ] [useCachingBSplineWeights ] ";
    std::cerr << " [filenameForFinalTransformParameters] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned char;

  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;


  //
  //  We instantiate now the type of the \code{BSplineTransform} using
  //  as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the BSpline.
  //
  //  \index{BSplineTransform!New}
  //  \index{BSplineTransform!Instantiation}
  //

  const unsigned int     SpaceDimension = ImageDimension;
  constexpr unsigned int SplineOrder = 3;
  using CoordinateRepType = double;

  using TransformType = itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder>;


  using OptimizerType = itk::LBFGSBOptimizer;


  using MetricType = itk::MattesMutualInformationImageToImageMetric<FixedImageType, MovingImageType>;

  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;

  using RegistrationType = itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;

  MetricType::Pointer       metric = MetricType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();


  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetInterpolator(interpolator);


  //
  //  The transform object is constructed below and passed to the registration
  //  method.
  //  \index{itk::RegistrationMethod!SetTransform()}
  //
  //
  TransformType::Pointer transform = TransformType::New();
  registration->SetTransform(transform);

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  FixedImageReaderType::Pointer  fixedImageReader = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImageReader->GetOutput());

  fixedImageReader->Update();

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();

  registration->SetFixedImageRegion(fixedRegion);


  unsigned int numberOfGridNodesInOneDimension = 7;

  TransformType::PhysicalDimensionsType fixedPhysicalDimensions;
  TransformType::MeshSizeType           meshSize;
  TransformType::OriginType             fixedOrigin;

  for (unsigned int i = 0; i < SpaceDimension; i++)
  {
    fixedOrigin[i] = fixedImage->GetOrigin()[i];
    fixedPhysicalDimensions[i] =
      fixedImage->GetSpacing()[i] * static_cast<double>(fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1);
  }
  meshSize.Fill(numberOfGridNodesInOneDimension - SplineOrder);

  transform->SetTransformDomainOrigin(fixedOrigin);
  transform->SetTransformDomainPhysicalDimensions(fixedPhysicalDimensions);
  transform->SetTransformDomainMeshSize(meshSize);
  transform->SetTransformDomainDirection(fixedImage->GetDirection());

  using ParametersType = TransformType::ParametersType;

  const unsigned int numberOfParameters = transform->GetNumberOfParameters();

  ParametersType parameters(numberOfParameters);

  parameters.Fill(0.0);

  transform->SetParameters(parameters);

  //
  //  We now pass the parameters of the current transform as the initial
  //  parameters to be used when the registration process starts.
  //

  registration->SetInitialTransformParameters(transform->GetParameters());


  //
  //  Next we set the parameters of the LBFGSB Optimizer.
  //


  OptimizerType::BoundSelectionType boundSelect(transform->GetNumberOfParameters());
  OptimizerType::BoundValueType     upperBound(transform->GetNumberOfParameters());
  OptimizerType::BoundValueType     lowerBound(transform->GetNumberOfParameters());

  boundSelect.Fill(0);
  upperBound.Fill(0.0);
  lowerBound.Fill(0.0);

  optimizer->SetBoundSelection(boundSelect);
  optimizer->SetUpperBound(upperBound);
  optimizer->SetLowerBound(lowerBound);

  optimizer->SetCostFunctionConvergenceFactor(1.e7);
  optimizer->SetProjectedGradientTolerance(1e-35);
  optimizer->SetMaximumNumberOfIterations(200);
  optimizer->SetMaximumNumberOfEvaluations(200);
  optimizer->SetMaximumNumberOfCorrections(7);

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  metric->SetNumberOfHistogramBins(50);

  const auto numberOfSamples = static_cast<unsigned int>(fixedRegion.GetNumberOfPixels() * 60.0 / 100.0);

  metric->SetNumberOfSpatialSamples(numberOfSamples);

  if (argc > 7)
  {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the Transform
    // parameters, or doing it by progressively accumulating contributions from
    // each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives(std::stoi(argv[7]));
  }

  if (argc > 8)
  {
    // Define whether to cache the BSpline weights and indexes corresponding to
    // each one of the samples used to compute the metric. Enabling caching will
    // make the algorithm run faster but it will have a cost on the amount of
    // memory that needs to be allocated. This option is only relevant when
    // using the BSplineTransform.
    metric->SetUseCachingOfBSplineWeights(std::stoi(argv[8]));
  }

  metric->ReinitializeSeed(121212);

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

    std::cout << "Optimizer stop condition = " << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  OptimizerType::ParametersType finalParameters = registration->GetLastTransformParameters();


  // Report the time and memory taken by the registration
  chronometer.Report(std::cout);
  memorymeter.Report(std::cout);

  transform->SetParameters(finalParameters);


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

  using DifferenceFilterType = itk::SquaredDifferenceImageFilter<FixedImageType, FixedImageType, OutputImageType>;

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
  if (argc > 9)
  {
    std::ofstream parametersFile;
    parametersFile.open(argv[9]);
    parametersFile << finalParameters << std::endl;
    parametersFile.close();
  }

  return EXIT_SUCCESS;
}
