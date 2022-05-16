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

//
// This example illustrates the use of the \doxygen{BSplineTransform}
// class for performing registration of two $3D$ images. The example code is
// for the most part identical to the code presented in
// Section~\ref{sec:BSplinesMultiGridImageRegistration}. The major difference is
// that this example we set the image dimension to 3 and replace the
// \doxygen{LBFGSOptimizer} optimizer with the \doxygen{LBFGSBOptimizer}. We
// made the modification because we found that LBFGS does not behave well when
// the starting position is at or close to optimal; instead we used LBFGSB in
// unconstrained mode.
//
//
// \index{itk::BSplineTransform}
// \index{itk::BSplineTransform!DeformableRegistration}
// \index{itk::LBFGSOptimizer}
//
//

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"

#include "itkTimeProbesCollectorBase.h"
#include "itkMemoryProbesCollectorBase.h"

//
//  The following are the most relevant headers to this example.
//
//  \index{itk::BSplineTransform!header}
//  \index{itk::LBFGSOptimizer!header}
//

#include "itkBSplineTransform.h"
#include "itkLBFGSBOptimizer.h"

//
//  The parameter space of the \code{BSplineTransform} is composed by
//  the set of all the deformations associated with the nodes of the BSpline
//  grid.  This large number of parameters enables it to represent a wide
//  variety of deformations, at the cost of requiring a
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
    std::cout << optimizer->GetValue() << "   ";
    std::cout << optimizer->GetInfinityNormOfProjectedGradient() << std::endl;
  }
};


#include "itkTestDriverIncludeRequiredFactories.h"
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
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 3;
  using PixelType = float;

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


  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;

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

  unsigned int numberOfGridNodes = 8;

  TransformType::PhysicalDimensionsType fixedPhysicalDimensions;
  TransformType::MeshSizeType           meshSize;
  TransformType::OriginType             fixedOrigin;

  for (unsigned int i = 0; i < SpaceDimension; ++i)
  {
    fixedOrigin[i] = fixedImage->GetOrigin()[i];
    fixedPhysicalDimensions[i] =
      fixedImage->GetSpacing()[i] * static_cast<double>(fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1);
  }
  meshSize.Fill(numberOfGridNodes - SplineOrder);

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

  std::cout << "Intial Parameters = " << std::endl;
  std::cout << transform->GetParameters() << std::endl;

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

  optimizer->SetCostFunctionConvergenceFactor(1e+12);
  optimizer->SetProjectedGradientTolerance(1.0);
  optimizer->SetMaximumNumberOfIterations(500);
  optimizer->SetMaximumNumberOfEvaluations(500);
  optimizer->SetMaximumNumberOfCorrections(5);

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);


  // Add a time probe
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

  std::cout << "Last Transform Parameters" << std::endl;
  std::cout << finalParameters << std::endl;


  // Report the time taken by the registration
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
  resample->SetDefaultPixelValue(100);

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
  if (argc >= 7)
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

  return EXIT_SUCCESS;
}
