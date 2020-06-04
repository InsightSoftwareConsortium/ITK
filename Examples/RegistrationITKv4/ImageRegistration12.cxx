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
// This example illustrates the use of \code{SpatialObject}s as masks for
// selecting the pixels that should contribute to the computation of Image
// Metrics. This example is almost identical to ImageRegistration6 with the
// exception that the \code{SpatialObject} masks are created and passed to the
// image metric.
//
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethodv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegularStepGradientDescentOptimizerv4.h"

#include "itkEuler2DTransform.h"
#include "itkCenteredTransformInitializer.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"

//  Software Guide : BeginLatex
//
//  The most important header in this example is the one corresponding to the
//  \doxygen{ImageMaskSpatialObject} class.
//
//  \index{itk::ImageMaskSpatialObject!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageMaskSpatialObject.h"
// Software Guide : EndCodeSnippet

//
//  The following section of code implements a command observer
//  that will monitor the evolution of the registration process.
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
  using OptimizerType = itk::RegularStepGradientDescentOptimizerv4<double>;
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
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << optimizer->GetCurrentPosition() << std::endl;
  }
};

int
main(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile fixedImageMaskFile";
    std::cerr << " outputImagefile  [differenceOutputfile] ";
    std::cerr << " [differenceBeforeRegistration] " << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = float;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using TransformType = itk::Euler2DTransform<double>;

  using OptimizerType = itk::RegularStepGradientDescentOptimizerv4<double>;
  using MetricType =
    itk::MeanSquaresImageToImageMetricv4<FixedImageType, MovingImageType>;
  using RegistrationType = itk::
    ImageRegistrationMethodv4<FixedImageType, MovingImageType, TransformType>;

  MetricType::Pointer       metric = MetricType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  RegistrationType::Pointer registration = RegistrationType::New();

  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);

  TransformType::Pointer transform = TransformType::New();
  registration->SetInitialTransform(transform);
  registration->InPlaceOn();

  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;
  FixedImageReaderType::Pointer fixedImageReader =
    FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader =
    MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);

  registration->SetFixedImage(fixedImageReader->GetOutput());
  registration->SetMovingImage(movingImageReader->GetOutput());
  fixedImageReader->Update();

  using TransformInitializerType =
    itk::CenteredTransformInitializer<TransformType,
                                      FixedImageType,
                                      MovingImageType>;
  TransformInitializerType::Pointer initializer =
    TransformInitializerType::New();

  initializer->SetTransform(transform);
  initializer->SetFixedImage(fixedImageReader->GetOutput());
  initializer->SetMovingImage(movingImageReader->GetOutput());

  initializer->MomentsOn();

  initializer->InitializeTransform();

  transform->SetAngle(0.0);

  using OptimizerScalesType = OptimizerType::ScalesType;
  OptimizerScalesType optimizerScales(transform->GetNumberOfParameters());
  const double        translationScale = 1.0 / 1000.0;

  optimizerScales[0] = 1.0;
  optimizerScales[1] = translationScale;
  optimizerScales[2] = translationScale;

  optimizer->SetScales(optimizerScales);

  optimizer->SetLearningRate(0.1);
  optimizer->SetMinimumStepLength(0.001);
  optimizer->SetNumberOfIterations(200);

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);

  //  Software Guide : BeginLatex
  //
  //  Here we instantiate the type of the \doxygen{ImageMaskSpatialObject}
  //  using the same dimension of the images to be registered.
  //
  //  \index{itk::ImageMaskSpatialObject!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using MaskType = itk::ImageMaskSpatialObject<Dimension>;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then we use the type for creating the spatial object mask that will
  //  restrict the registration to a reduced region of the image.
  //
  //  \index{itk::ImageMaskSpatialObject!New}
  //  \index{itk::ImageMaskSpatialObject!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MaskType::Pointer spatialObjectMask = MaskType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The mask in this case is read from a binary file using the
  //  \code{ImageFileReader} instantiated for an \code{unsigned char} pixel
  //  type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using ImageMaskType = itk::Image<unsigned char, Dimension>;

  using MaskReaderType = itk::ImageFileReader<ImageMaskType>;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The reader is constructed and a filename is passed to it.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MaskReaderType::Pointer maskReader = MaskReaderType::New();

  maskReader->SetFileName(argv[3]);
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  As usual, the reader is triggered by invoking its \code{Update()}
  //  method. Since this may eventually throw an exception, the call must be
  //  placed in a \code{try/catch} block. Note that a full fledged application
  //  will place this \code{try/catch} block at a much higher level, probably
  //  under the control of the GUI.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
  {
    maskReader->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The output of the mask reader is connected as input to the
  //  \code{ImageMaskSpatialObject}.
  //
  //  \index{itk::ImageMaskSpatialObject!SetImage()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  spatialObjectMask->SetImage(maskReader->GetOutput());
  spatialObjectMask->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally, the spatial object mask is passed to the image metric.
  //
  //  \index{itk::ImageToImageMetricv4!SetFixedImageMask()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  metric->SetFixedImageMask(spatialObjectMask);
  // Software Guide : EndCodeSnippet

  // One level registration process without shrinking and smoothing.
  //
  constexpr unsigned int numberOfLevels = 1;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize(1);
  shrinkFactorsPerLevel[0] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize(1);
  smoothingSigmasPerLevel[0] = 0;

  registration->SetNumberOfLevels(numberOfLevels);
  registration->SetSmoothingSigmasPerLevel(smoothingSigmasPerLevel);
  registration->SetShrinkFactorsPerLevel(shrinkFactorsPerLevel);

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

  OptimizerType::ParametersType finalParameters = transform->GetParameters();

  const double finalAngle = finalParameters[0];
  const double finalTranslationX = finalParameters[1];
  const double finalTranslationY = finalParameters[2];

  const double rotationCenterX =
    registration->GetOutput()->Get()->GetFixedParameters()[0];
  const double rotationCenterY =
    registration->GetOutput()->Get()->GetFixedParameters()[1];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  const double       bestValue = optimizer->GetValue();

  // Print out results
  //
  const double finalAngleInDegrees = finalAngle * 45.0 / std::atan(1.0);

  std::cout << "Result = " << std::endl;
  std::cout << " Angle (radians) " << finalAngle << std::endl;
  std::cout << " Angle (degrees) " << finalAngleInDegrees << std::endl;
  std::cout << " Translation X  = " << finalTranslationX << std::endl;
  std::cout << " Translation Y  = " << finalTranslationY << std::endl;
  std::cout << " Fixed Center X = " << rotationCenterX << std::endl;
  std::cout << " Fixed Center Y = " << rotationCenterY << std::endl;
  std::cout << " Iterations     = " << numberOfIterations << std::endl;
  std::cout << " Metric value   = " << bestValue << std::endl;

  //  Software Guide : BeginLatex
  //
  //  Let's execute this example over some of the images provided in
  //  \code{Examples/Data}, for example:
  //
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png}
  //  \item \code{BrainProtonDensitySliceR10X13Y17.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first
  //  image by $10$ degrees and shifting it $13mm$ in $X$ and $17mm$ in
  //  $Y$. Both images have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration5}.
  //
  //  The registration converges after $20$ iterations and produces the
  //  following results:
  //
  //  \begin{verbatim}
  //
  //  Angle (radians) 0.174712
  //  Angle (degrees) 10.0103
  //  Translation X = 12.4521
  //  Translation Y = 16.0765
  //
  //  \end{verbatim}
  //
  //  These values are a very close match to the true misalignments
  //  introduced in the moving image.
  //
  //  Now we resample the moving image using the transform resulting from the
  //  registration process.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::MatrixType matrix = transform->GetMatrix();
  TransformType::OffsetType offset = transform->GetOffset();

  std::cout << "Matrix = " << std::endl << matrix << std::endl;
  std::cout << "Offset = " << std::endl << offset << std::endl;
  // Software Guide : EndCodeSnippet

  using ResampleFilterType =
    itk::ResampleImageFilter<MovingImageType, FixedImageType>;
  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters(finalParameters);
  finalTransform->SetFixedParameters(transform->GetFixedParameters());

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform(finalTransform);
  resample->SetInput(movingImageReader->GetOutput());

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resample->SetOutputOrigin(fixedImage->GetOrigin());
  resample->SetOutputSpacing(fixedImage->GetSpacing());
  resample->SetOutputDirection(fixedImage->GetDirection());
  resample->SetDefaultPixelValue(100);

  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using CastFilterType =
    itk::CastImageFilter<FixedImageType, OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;

  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();

  writer->SetFileName(argv[4]);

  caster->SetInput(resample->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();

  using DifferenceFilterType =
    itk::SquaredDifferenceImageFilter<FixedImageType,
                                      FixedImageType,
                                      OutputImageType>;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput(difference->GetOutput());

  // Compute the difference image between the
  // fixed and resampled moving image.
  if (argc >= 6)
  {
    difference->SetInput1(fixedImageReader->GetOutput());
    difference->SetInput2(resample->GetOutput());
    writer2->SetFileName(argv[5]);
    writer2->Update();
  }

  // Compute the difference image between the
  // fixed and moving image before registration.
  if (argc >= 7)
  {
    writer2->SetFileName(argv[6]);
    difference->SetInput1(fixedImageReader->GetOutput());
    difference->SetInput2(movingImageReader->GetOutput());
    writer2->Update();
  }

  return EXIT_SUCCESS;
}
