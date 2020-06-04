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
// This example illustrates a realistic pipeline for solving a full deformable
// registration problem.
//
// First the two images are roughly aligned by using a transform
// initialization, then they are registered using a rigid transform, that in
// turn, is used to initialize a registration with an affine transform. The
// transform resulting from the affine registration is compounded with
// a BSplineTransform. The deformable registration is computed,
// and finally the resulting transform is used to resample the moving image.
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethod.h"
#include "itkMattesMutualInformationImageToImageMetric.h"

#include "itkTimeProbesCollectorBase.h"
#include "itkMemoryProbesCollectorBase.h"

//  Software Guide : BeginLatex
//
//  The following are the most relevant headers to this example.
//
//  \index{itk::VersorRigid3DTransform!header}
//  \index{itk::AffineTransform!header}
//  \index{itk::BSplineTransform!header}
//  \index{itk::RegularStepGradientDescentOptimizer!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCenteredTransformInitializer.h"
#include "itkVersorRigid3DTransform.h"
#include "itkAffineTransform.h"
#include "itkBSplineTransform.h"
#include "itkCompositeTransform.h"
#include "itkRegularStepGradientDescentOptimizer.h"
// Software Guide : EndCodeSnippet

#include "itkBSplineResampleImageFunction.h"
#include "itkBSplineDecompositionImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"
#include "itkSqrtImageFilter.h"

#include "itkTransformFileWriter.h"

//  The following section of code implements a Command observer
//  used to monitor the evolution of the registration process.
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
  using OptimizerType = itk::RegularStepGradientDescentOptimizer;
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
    std::cout << std::endl;
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
    std::cerr << " [filenameForFinalTransformParameters] ";
    std::cerr << " [useExplicitPDFderivatives ] [useCachingBSplineWeights ] ";
    std::cerr << " [deformationField] ";
    std::cerr << " [numberOfGridNodesInsideImageInOneDimensionCoarse] ";
    std::cerr << " [numberOfGridNodesInsideImageInOneDimensionFine] ";
    std::cerr << " [maximumStepLength] [maximumNumberOfIterations]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 3;
  using PixelType = signed short;

  using FixedImageType = itk::Image<PixelType, ImageDimension>;
  using MovingImageType = itk::Image<PixelType, ImageDimension>;


  const unsigned int SpaceDimension = ImageDimension;

  constexpr unsigned int SplineOrder = 3;
  using CoordinateRepType = double;

  using RigidTransformType = itk::VersorRigid3DTransform<double>;

  using AffineTransformType = itk::AffineTransform<double, SpaceDimension>;

  using DeformableTransformType =
    itk::BSplineTransform<CoordinateRepType, SpaceDimension, SplineOrder>;

  using TransformInitializerType =
    itk::CenteredTransformInitializer<RigidTransformType,
                                      FixedImageType,
                                      MovingImageType>;


  using OptimizerType = itk::RegularStepGradientDescentOptimizer;


  using MetricType =
    itk::MattesMutualInformationImageToImageMetric<FixedImageType,
                                                   MovingImageType>;

  using InterpolatorType =
    itk::LinearInterpolateImageFunction<MovingImageType, double>;

  using RegistrationType =
    itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;

  MetricType::Pointer       metric = MetricType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();


  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetInterpolator(interpolator);

  // Auxiliary identity transform.
  using IdentityTransformType =
    itk::IdentityTransform<double, SpaceDimension>;
  IdentityTransformType::Pointer identityTransform =
    IdentityTransformType::New();


  //   Read the Fixed and Moving images.
  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  FixedImageReaderType::Pointer fixedImageReader =
    FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader =
    MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  try
  {
    fixedImageReader->Update();
    movingImageReader->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImage(fixedImage);
  registration->SetMovingImage(movingImageReader->GetOutput());

  // Add a time and memory probes collector for profiling the computation time
  // of every stage.
  itk::TimeProbesCollectorBase   chronometer;
  itk::MemoryProbesCollectorBase memorymeter;


  // Setup the metric parameters
  metric->SetNumberOfHistogramBins(50);

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();

  const unsigned int numberOfPixels = fixedRegion.GetNumberOfPixels();

  metric->ReinitializeSeed(76926294);


  if (argc > 7)
  {
    // Define whether to calculate the metric derivative by explicitly
    // computing the derivatives of the joint PDF with respect to the
    // Transform parameters, or doing it by progressively accumulating
    // contributions from each bin in the joint PDF.
    metric->SetUseExplicitPDFDerivatives(std::stoi(argv[7]));
  }

  if (argc > 8)
  {
    // Define whether to cache the BSpline weights and indexes corresponding
    // to each one of the samples used to compute the metric. Enabling caching
    // will make the algorithm run faster but it will have a cost on the
    // amount of memory that needs to be allocated. This option is only
    // relevant when using the BSplineTransform.
    metric->SetUseCachingOfBSplineWeights(std::stoi(argv[8]));
  }


  //  Initialize a rigid transform by using Image Intensity Moments
  TransformInitializerType::Pointer initializer =
    TransformInitializerType::New();

  RigidTransformType::Pointer rigidTransform = RigidTransformType::New();

  initializer->SetTransform(rigidTransform);
  initializer->SetFixedImage(fixedImageReader->GetOutput());
  initializer->SetMovingImage(movingImageReader->GetOutput());
  initializer->MomentsOn();


  std::cout << "Starting Rigid Transform Initialization " << std::endl;

  memorymeter.Start("Rigid Initialization");
  chronometer.Start("Rigid Initialization");

  initializer->InitializeTransform();

  chronometer.Stop("Rigid Initialization");
  memorymeter.Stop("Rigid Initialization");

  std::cout << "Rigid Transform Initialization completed" << std::endl;
  std::cout << std::endl;

  registration->SetFixedImageRegion(fixedRegion);
  registration->SetInitialTransformParameters(
    rigidTransform->GetParameters());

  registration->SetTransform(rigidTransform);

  //  Define optimizer normalization to compensate for different dynamic range
  //  of rotations and translations.
  using OptimizerScalesType = OptimizerType::ScalesType;
  OptimizerScalesType optimizerScales(
    rigidTransform->GetNumberOfParameters());
  const double translationScale = 1.0 / 1000.0;

  optimizerScales[0] = 1.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = 1.0;
  optimizerScales[3] = translationScale;
  optimizerScales[4] = translationScale;
  optimizerScales[5] = translationScale;

  optimizer->SetScales(optimizerScales);

  optimizer->SetMaximumStepLength(0.2000);
  optimizer->SetMinimumStepLength(0.0001);

  optimizer->SetNumberOfIterations(200);

  //
  // The rigid transform has 6 parameters we use therefore a few samples to
  // run this stage.
  //
  // Regulating the number of samples in the Metric is equivalent to
  // performing multi-resolution registration because it is indeed a
  // sub-sampling of the image.
  metric->SetNumberOfSpatialSamples(10000L);

  // Create the Command observer and register it with the optimizer.
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);


  std::cout << "Starting Rigid Registration " << std::endl;

  try
  {
    memorymeter.Start("Rigid Registration");
    chronometer.Start("Rigid Registration");

    registration->Update();

    chronometer.Stop("Rigid Registration");
    memorymeter.Stop("Rigid Registration");

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

  std::cout << "Rigid Registration completed" << std::endl;
  std::cout << std::endl;

  rigidTransform->SetParameters(registration->GetLastTransformParameters());


  //  Perform Affine Registration
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();

  affineTransform->SetCenter(rigidTransform->GetCenter());
  affineTransform->SetTranslation(rigidTransform->GetTranslation());
  affineTransform->SetMatrix(rigidTransform->GetMatrix());

  registration->SetTransform(affineTransform);
  registration->SetInitialTransformParameters(
    affineTransform->GetParameters());

  optimizerScales =
    OptimizerScalesType(affineTransform->GetNumberOfParameters());

  optimizerScales[0] = 1.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = 1.0;
  optimizerScales[3] = 1.0;
  optimizerScales[4] = 1.0;
  optimizerScales[5] = 1.0;
  optimizerScales[6] = 1.0;
  optimizerScales[7] = 1.0;
  optimizerScales[8] = 1.0;

  optimizerScales[9] = translationScale;
  optimizerScales[10] = translationScale;
  optimizerScales[11] = translationScale;

  optimizer->SetScales(optimizerScales);

  optimizer->SetMaximumStepLength(0.2000);
  optimizer->SetMinimumStepLength(0.0001);

  optimizer->SetNumberOfIterations(200);

  // The Affine transform has 12 parameters we use therefore a more samples to
  // run this stage.
  //
  // Regulating the number of samples in the Metric is equivalent to
  // performing multi-resolution registration because it is indeed a
  // sub-sampling of the image.
  metric->SetNumberOfSpatialSamples(50000L);


  std::cout << "Starting Affine Registration " << std::endl;

  try
  {
    memorymeter.Start("Affine Registration");
    chronometer.Start("Affine Registration");

    registration->Update();

    chronometer.Stop("Affine Registration");
    memorymeter.Stop("Affine Registration");
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Affine Registration completed" << std::endl;
  std::cout << std::endl;

  affineTransform->SetParameters(registration->GetLastTransformParameters());


  //  Perform Deformable Registration
  DeformableTransformType::Pointer bsplineTransformCoarse =
    DeformableTransformType::New();

  unsigned int numberOfGridNodesInOneDimensionCoarse = 5;

  DeformableTransformType::PhysicalDimensionsType fixedPhysicalDimensions;
  DeformableTransformType::MeshSizeType           meshSize;
  DeformableTransformType::OriginType             fixedOrigin;

  for (unsigned int i = 0; i < SpaceDimension; i++)
  {
    fixedOrigin[i] = fixedImage->GetOrigin()[i];
    fixedPhysicalDimensions[i] =
      fixedImage->GetSpacing()[i] *
      static_cast<double>(
        fixedImage->GetLargestPossibleRegion().GetSize()[i] - 1);
  }
  meshSize.Fill(numberOfGridNodesInOneDimensionCoarse - SplineOrder);

  bsplineTransformCoarse->SetTransformDomainOrigin(fixedOrigin);
  bsplineTransformCoarse->SetTransformDomainPhysicalDimensions(
    fixedPhysicalDimensions);
  bsplineTransformCoarse->SetTransformDomainMeshSize(meshSize);
  bsplineTransformCoarse->SetTransformDomainDirection(
    fixedImage->GetDirection());

  using ParametersType = DeformableTransformType::ParametersType;

  unsigned int numberOfBSplineParameters =
    bsplineTransformCoarse->GetNumberOfParameters();


  optimizerScales = OptimizerScalesType(numberOfBSplineParameters);
  optimizerScales.Fill(1.0);

  optimizer->SetScales(optimizerScales);


  ParametersType initialDeformableTransformParameters(
    numberOfBSplineParameters);
  initialDeformableTransformParameters.Fill(0.0);

  using CompositeTransformType =
    itk::CompositeTransform<double, SpaceDimension>;
  typename CompositeTransformType::Pointer compositeTransform =
    CompositeTransformType::New();
  compositeTransform->AddTransform(affineTransform);
  compositeTransform->AddTransform(bsplineTransformCoarse);
  compositeTransform->SetOnlyMostRecentTransformToOptimizeOn();

  bsplineTransformCoarse->SetParameters(initialDeformableTransformParameters);

  registration->SetInitialTransformParameters(
    bsplineTransformCoarse->GetParameters());
  registration->SetTransform(compositeTransform);

  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Next we set the parameters of the RegularStepGradientDescentOptimizer
  //  object.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength(10.0);
  optimizer->SetMinimumStepLength(0.01);

  optimizer->SetRelaxationFactor(0.7);
  optimizer->SetNumberOfIterations(50);
  // Software Guide : EndCodeSnippet


  // Optionally, get the step length from the command line arguments
  if (argc > 11)
  {
    optimizer->SetMaximumStepLength(std::stod(argv[12]));
  }

  // Optionally, get the number of iterations from the command line arguments
  if (argc > 12)
  {
    optimizer->SetNumberOfIterations(std::stoi(argv[13]));
  }


  // The BSpline transform has a large number of parameters, we use therefore
  // a much larger number of samples to run this stage.
  //
  // Regulating the number of samples in the Metric is equivalent to
  // performing multi-resolution registration because it is indeed a
  // sub-sampling of the image.
  metric->SetNumberOfSpatialSamples(numberOfBSplineParameters * 100);

  std::cout << std::endl
            << "Starting Deformable Registration Coarse Grid" << std::endl;

  try
  {
    memorymeter.Start("Deformable Registration Coarse");
    chronometer.Start("Deformable Registration Coarse");

    registration->Update();

    chronometer.Stop("Deformable Registration Coarse");
    memorymeter.Stop("Deformable Registration Coarse");
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Deformable Registration Coarse Grid completed" << std::endl;
  std::cout << std::endl;

  OptimizerType::ParametersType finalParameters =
    registration->GetLastTransformParameters();

  bsplineTransformCoarse->SetParameters(finalParameters);

  //  Software Guide : BeginLatex
  //
  //  Once the registration has finished with the low resolution grid, we
  //  proceed to instantiate a higher resolution
  //  \code{BSplineTransform}.
  //
  //  Software Guide : EndLatex

  DeformableTransformType::Pointer bsplineTransformFine =
    DeformableTransformType::New();

  unsigned int numberOfGridNodesInOneDimensionFine = 20;

  meshSize.Fill(numberOfGridNodesInOneDimensionFine - SplineOrder);

  bsplineTransformFine->SetTransformDomainOrigin(fixedOrigin);
  bsplineTransformFine->SetTransformDomainPhysicalDimensions(
    fixedPhysicalDimensions);
  bsplineTransformFine->SetTransformDomainMeshSize(meshSize);
  bsplineTransformFine->SetTransformDomainDirection(
    fixedImage->GetDirection());

  numberOfBSplineParameters = bsplineTransformFine->GetNumberOfParameters();

  ParametersType parametersHigh(numberOfBSplineParameters);
  parametersHigh.Fill(0.0);

  //  Software Guide : BeginLatex
  //
  //  Now we need to initialize the BSpline coefficients of the higher
  //  resolution transform. This is done by first computing the actual
  //  deformation field at the higher resolution from the lower resolution
  //  BSpline coefficients. Then a BSpline decomposition is done to obtain the
  //  BSpline coefficient of the higher resolution transform.
  //
  //  Software Guide : EndLatex

  unsigned int counter = 0;

  for (unsigned int k = 0; k < SpaceDimension; k++)
  {
    using ParametersImageType = DeformableTransformType::ImageType;
    using ResamplerType =
      itk::ResampleImageFilter<ParametersImageType, ParametersImageType>;
    ResamplerType::Pointer upsampler = ResamplerType::New();

    using FunctionType =
      itk::BSplineResampleImageFunction<ParametersImageType, double>;
    FunctionType::Pointer function = FunctionType::New();

    upsampler->SetInput(bsplineTransformCoarse->GetCoefficientImages()[k]);
    upsampler->SetInterpolator(function);
    upsampler->SetTransform(identityTransform);
    upsampler->SetSize(bsplineTransformFine->GetCoefficientImages()[k]
                         ->GetLargestPossibleRegion()
                         .GetSize());
    upsampler->SetOutputSpacing(
      bsplineTransformFine->GetCoefficientImages()[k]->GetSpacing());
    upsampler->SetOutputOrigin(
      bsplineTransformFine->GetCoefficientImages()[k]->GetOrigin());

    using DecompositionType =
      itk::BSplineDecompositionImageFilter<ParametersImageType,
                                           ParametersImageType>;
    DecompositionType::Pointer decomposition = DecompositionType::New();

    decomposition->SetSplineOrder(SplineOrder);
    decomposition->SetInput(upsampler->GetOutput());
    decomposition->Update();

    ParametersImageType::Pointer newCoefficients = decomposition->GetOutput();

    // copy the coefficients into the parameter array
    using Iterator = itk::ImageRegionIterator<ParametersImageType>;
    Iterator it(newCoefficients,
                bsplineTransformFine->GetCoefficientImages()[k]
                  ->GetLargestPossibleRegion());
    while (!it.IsAtEnd())
    {
      parametersHigh[counter++] = it.Get();
      ++it;
    }
  }


  optimizerScales = OptimizerScalesType(numberOfBSplineParameters);
  optimizerScales.Fill(1.0);

  optimizer->SetScales(optimizerScales);

  bsplineTransformFine->SetParameters(parametersHigh);

  //  Software Guide : BeginLatex
  //
  //  We now pass the parameters of the high resolution transform as the
  //  initial parameters to be used in a second stage of the registration
  //  process.
  //
  //  Software Guide : EndLatex

  std::cout << "Starting Registration with high resolution transform"
            << std::endl;

  // Software Guide : BeginCodeSnippet
  compositeTransform->RemoveTransform(); // remove bsplineTransformCoarse
  compositeTransform->AddTransform(bsplineTransformFine);
  compositeTransform->SetOnlyMostRecentTransformToOptimizeOn();
  registration->SetInitialTransformParameters(
    bsplineTransformFine->GetParameters());
  //
  // The BSpline transform at fine scale has a very large number of
  // parameters, we use therefore a much larger number of samples to run this
  // stage. In this case, however, the number of transform parameters is
  // closer to the number of pixels in the image. Therefore we use the
  // geometric mean of the two numbers to ensure that the number of samples is
  // larger than the number of transform parameters and smaller than the
  // number of samples.
  //
  // Regulating the number of samples in the Metric is equivalent to
  // performing multi-resolution registration because it is indeed a
  // sub-sampling of the image.
  const auto numberOfSamples = static_cast<unsigned long>(
    std::sqrt(static_cast<double>(numberOfBSplineParameters) *
              static_cast<double>(numberOfPixels)));
  metric->SetNumberOfSpatialSamples(numberOfSamples);

  try
  {
    memorymeter.Start("Deformable Registration Fine");
    chronometer.Start("Deformable Registration Fine");
    registration->Update();
    chronometer.Stop("Deformable Registration Fine");
    memorymeter.Stop("Deformable Registration Fine");
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }
  // Software Guide : EndCodeSnippet

  std::cout << "Deformable Registration Fine Grid completed" << std::endl;
  std::cout << std::endl;


  // Report the time and memory taken by the registration
  chronometer.Report(std::cout);
  memorymeter.Report(std::cout);

  finalParameters = registration->GetLastTransformParameters();

  bsplineTransformFine->SetParameters(finalParameters);


  using ResampleFilterType =
    itk::ResampleImageFilter<MovingImageType, FixedImageType>;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform(bsplineTransformFine);
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

  using OutputPixelType = signed short;

  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  using CastFilterType =
    itk::CastImageFilter<FixedImageType, OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;


  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();


  writer->SetFileName(argv[3]);


  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());

  std::cout << "Writing resampled moving image...";

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

  std::cout << " Done!" << std::endl;


  using DifferenceFilterType =
    itk::SquaredDifferenceImageFilter<FixedImageType,
                                      FixedImageType,
                                      OutputImageType>;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();
  using SqrtFilterType =
    itk::SqrtImageFilter<OutputImageType, OutputImageType>;
  SqrtFilterType::Pointer sqrtFilter = SqrtFilterType::New();
  sqrtFilter->SetInput(difference->GetOutput());

  using DifferenceImageWriterType = itk::ImageFileWriter<OutputImageType>;

  DifferenceImageWriterType::Pointer writer2 =
    DifferenceImageWriterType::New();
  writer2->SetInput(sqrtFilter->GetOutput());


  // Compute the difference image between the
  // fixed and resampled moving image.
  if (argc > 4)
  {
    difference->SetInput1(fixedImageReader->GetOutput());
    difference->SetInput2(resample->GetOutput());
    writer2->SetFileName(argv[4]);

    std::cout << "Writing difference image after registration...";

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

    std::cout << " Done!" << std::endl;
  }


  // Compute the difference image between the
  // fixed and moving image before registration.
  if (argc > 5)
  {
    writer2->SetFileName(argv[5]);
    difference->SetInput1(fixedImageReader->GetOutput());
    resample->SetTransform(identityTransform);

    std::cout << "Writing difference image before registration...";

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

    std::cout << " Done!" << std::endl;
  }

  // Generate the explicit deformation field resulting from
  // the registration.
  if (argc > 9)
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

    DeformableTransformType::InputPointType  fixedPoint;
    DeformableTransformType::OutputPointType movingPoint;
    DisplacementFieldType::IndexType         index;

    VectorType displacement;

    while (!fi.IsAtEnd())
    {
      index = fi.GetIndex();
      field->TransformIndexToPhysicalPoint(index, fixedPoint);
      movingPoint = bsplineTransformFine->TransformPoint(fixedPoint);
      displacement = movingPoint - fixedPoint;
      fi.Set(displacement);
      ++fi;
    }

    using FieldWriterType = itk::ImageFileWriter<DisplacementFieldType>;
    FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

    fieldWriter->SetInput(field);

    fieldWriter->SetFileName(argv[9]);

    std::cout << "Writing deformation field ...";

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

    std::cout << " Done!" << std::endl;
  }

  // Optionally, save the transform parameters in a file
  if (argc > 6)
  {
    std::cout << "Writing transform parameter file ...";
    using TransformWriterType = itk::TransformFileWriter;
    TransformWriterType::Pointer transformWriter = TransformWriterType::New();
    transformWriter->AddTransform(bsplineTransformFine);
    transformWriter->SetFileName(argv[6]);
    transformWriter->Update();
    std::cout << " Done!" << std::endl;
  }

  return EXIT_SUCCESS;
}
