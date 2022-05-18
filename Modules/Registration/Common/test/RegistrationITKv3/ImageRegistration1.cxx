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

//    INPUTS:  {BrainProtonDensitySliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceShifted13x17y.png}
//    OUTPUTS: {ImageRegistration1Output.png}
//    OUTPUTS: {ImageRegistration1DifferenceAfter.png}
//    OUTPUTS: {ImageRegistration1DifferenceBefore.png}

//
// This example illustrates the use of the image registration framework in
// Insight.  It should be read as a ``Hello World'' for ITK registration.
// Instead of means to an end, this example should be read as a basic
// introduction to the elements typically involved when solving a problem
// of image registration.
//
// \index{itk::Image!Instantiation}
// \index{itk::Image!Header}
//
// A registration method requires the following set of components: two input
// images, a transform, a metric, an interpolator and an optimizer. Some of
// these components are parameterized by the image type for which the
// registration is intended.  The following header files provide declarations
// of common types used for these components.
//


#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSubtractImageFilter.h"


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

    if (!itk::IterationEvent().CheckEvent(&event))
    {
      return;
    }

    std::cout << optimizer->GetCurrentIteration() << " = ";
    std::cout << optimizer->GetValue() << " : ";
    std::cout << optimizer->GetCurrentPosition() << std::endl;
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
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile [differenceImageAfter]";
    std::cerr << "[differenceImageBefore] [useEstimator]" << std::endl;
    return EXIT_FAILURE;
  }


  //
  // The type of each registration component should
  // be instantiated first. We start by selecting the image
  // dimension and the types to be used for representing image pixels.
  //

  constexpr unsigned int Dimension = 2;
  using PixelType = float;


  //
  //  The types of the input images are instantiated by the following lines.
  //

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;


  //
  //  The transform that will map the fixed image space into the moving image
  //  space is defined below.
  //

  using TransformType = itk::TranslationTransform<double, Dimension>;


  //
  //  An optimizer is required to explore the parameter space of the transform
  //  in search of optimal values of the metric.
  //

  using OptimizerType = itk::RegularStepGradientDescentOptimizer;


  //
  //  The metric will compare how well the two images match each other. Metric
  //  types are usually templated over the image types as seen in
  //  the following type declaration.
  //

  using MetricType = itk::MeanSquaresImageToImageMetric<FixedImageType, MovingImageType>;

  using InterpolatorType = itk::LinearInterpolateImageFunction<MovingImageType, double>;
  //
  //  The registration method type is instantiated using the types of the
  //  fixed and moving images as well as the output transform type. This class
  //  is responsible for interconnecting all the components that we have described so far.
  //

  using RegistrationType = itk::ImageRegistrationMethod<FixedImageType, MovingImageType>;


  //
  //  Each one of the registration components is created using its
  //  \code{New()} method and is assigned to its respective
  //  \doxygen{SmartPointer}.
  //

  MetricType::Pointer       metric = MetricType::New();
  TransformType::Pointer    transform = TransformType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();


  //
  //  Each component is now connected to the instance of the registration method.
  //  \index{itk::RegistrationMethod!SetMetric()}
  //  \index{itk::RegistrationMethod!SetOptimizer()}
  //  \index{itk::RegistrationMethod!SetTransform()}
  //  \index{itk::RegistrationMethod!SetFixedImage()}
  //  \index{itk::RegistrationMethod!SetMovingImage()}
  //  \index{itk::RegistrationMethod!SetInterpolator()}
  //

  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetTransform(transform);
  registration->SetInterpolator(interpolator);


  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;
  FixedImageReaderType::Pointer  fixedImageReader = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);


  //
  //  In this example, the fixed and moving images are read from files. This
  //  requires the \doxygen{ImageRegistrationMethod} to acquire its inputs from
  //  the output of the readers.
  //

  registration->SetFixedImage(fixedImageReader->GetOutput());
  registration->SetMovingImage(movingImageReader->GetOutput());


  //
  //  The registration can be restricted to consider only a particular region
  //  of the fixed image as input to the metric computation. This region is
  //  defined with the \code{SetFixedImageRegion()} method.  You could use this
  //  feature to reduce the computational time of the registration or to avoid
  //  unwanted objects present in the image from affecting the registration outcome.
  //  In this example we use the full available content of the image. This
  //  region is identified by the \code{BufferedRegion} of the fixed image.
  //  Note that for this region to be valid the reader must first invoke its
  //  \code{Update()} method.
  //
  //  \index{itk::ImageRegistrationMethod!SetFixedImageRegion()}
  //  \index{itk::Image!GetBufferedRegion()}
  //

  fixedImageReader->Update();
  registration->SetFixedImageRegion(fixedImageReader->GetOutput()->GetBufferedRegion());


  //
  //  The parameters of the transform are initialized by passing them in an
  //  array. This can be used to setup an initial known correction of the
  //  misalignment. In this particular case, a translation transform is
  //  being used for the registration. The array of parameters for this
  //  transform is simply composed of the translation values along each
  //  dimension. Setting the values of the parameters to zero
  //  initializes the transform to an \emph{Identity} transform. Note that the
  //  array constructor requires the number of elements to be passed as an
  //  argument.
  //
  //  \index{itk::TranslationTransform!GetNumberOfParameters()}
  //  \index{itk::RegistrationMethod!SetInitialTransformParameters()}
  //

  using ParametersType = RegistrationType::ParametersType;
  ParametersType initialParameters(transform->GetNumberOfParameters());

  initialParameters[0] = 0.0; // Initial offset in mm along X
  initialParameters[1] = 0.0; // Initial offset in mm along Y

  registration->SetInitialTransformParameters(initialParameters);


  //
  //  At this point the registration method is ready for execution. The
  //  optimizer is the component that drives the execution of the
  //  registration.  However, the ImageRegistrationMethod class
  //  orchestrates the ensemble to make sure that everything is in place
  //  before control is passed to the optimizer.
  //
  //  It is usually desirable to fine tune the parameters of the optimizer.
  //  Each optimizer has particular parameters that must be interpreted in the
  //  context of the optimization strategy it implements. The optimizer used in
  //  this example is a variant of gradient descent that attempts to prevent it
  //  from taking steps that are too large. At each iteration, this optimizer
  //  will take a step along the direction of the \doxygen{ImageToImageMetric}
  //  derivative. The initial length of the step is defined by the user. Each
  //  time the direction of the derivative abruptly changes, the optimizer
  //  assumes that a local extrema has been passed and reacts by reducing the
  //  step length by a half. After several reductions of the step length, the
  //  optimizer may be moving in a very restricted area of the transform
  //  parameter space. The user can define how small the step length should be
  //  to consider convergence to have been reached. This is equivalent to defining
  //  the precision with which the final transform should be known.
  //
  //  The initial step length is defined with the method
  //  \code{SetMaximumStepLength()}, while the tolerance for convergence is
  //  defined with the method \code{SetMinimumStepLength()}.
  //
  //  \index{itk::Regular\-Setp\-Gradient\-Descent\-Optimizer!SetMaximumStepLength()}
  //  \index{itk::Regular\-Step\-Gradient\-Descent\-Optimizer!SetMinimumStepLength()}
  //

  optimizer->SetMaximumStepLength(4.00);
  optimizer->SetMinimumStepLength(0.01);


  //
  //  In case the optimizer never succeeds reaching the desired
  //  precision tolerance, it is prudent to establish a limit on the number of
  //  iterations to be performed. This maximum number is defined with the
  //  method \code{SetNumberOfIterations()}.
  //
  //  \index{itk::Regular\-Setp\-Gradient\-Descent\-Optimizer!SetNumberOfIterations()}
  //

  optimizer->SetNumberOfIterations(200);


  // Connect an observer
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver(itk::IterationEvent(), observer);


  //
  //  The registration process is triggered by an invocation of the
  //  \code{Update()} method. If something goes wrong during the
  //  initialization or execution of the registration an exception will be
  //  thrown. We should therefore place the \code{Update()} method
  //  inside a \code{try/catch} block as illustrated in the following lines.
  //

  try
  {
    registration->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }


  //
  //  In a real life application, you may attempt to recover from the error by
  //  taking more effective actions in the catch block. Here we are simply
  //  printing out a message and then terminating the execution of the program.
  //
  //
  //  The result of the registration process is an array of parameters that
  //  defines the spatial transformation in an unique way. This final result is
  //  obtained using the \code{GetLastTransformParameters()} method.
  //
  //  \index{itk::RegistrationMethod!GetLastTransformParameters()}
  //

  ParametersType finalParameters = registration->GetLastTransformParameters();


  //
  //  In the case of the \doxygen{TranslationTransform}, there is a
  //  straightforward interpretation of the parameters.  Each element of the
  //  array corresponds to a translation along one spatial dimension.
  //

  const double TranslationAlongX = finalParameters[0];
  const double TranslationAlongY = finalParameters[1];


  //
  //  The optimizer can be queried for the actual number of iterations
  //  performed to reach convergence.  The \code{GetCurrentIteration()}
  //  method returns this value. A large number of iterations may be an
  //  indication that the maximum step length has been set too small, which
  //  is undesirable since it results in long computational times.
  //
  //  \index{itk::Regular\-Setp\-Gradient\-Descent\-Optimizer!GetCurrentIteration()}
  //

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  //
  //  The value of the image metric corresponding to the last set of parameters
  //  can be obtained with the \code{GetValue()} method of the optimizer.
  //

  const double bestValue = optimizer->GetValue();


  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue << std::endl;


  //
  //  Let's execute this example over two of the images provided in
  //  \code{Examples/Data}:
  //
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png}
  //  \item \code{BrainProtonDensitySliceShifted13x17y.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally translating the first
  //  image by $(13,17)$ millimeters. Both images have unit-spacing and
  //  are shown in Figure \ref{fig:FixedMovingImageRegistration1}. The
  //  registration takes 18 iterations and the resulting transform parameters are:
  //
  //  \begin{verbatim}
  //  Translation X = 12.9959
  //  Translation Y = 17.0001
  //  \end{verbatim}
  //
  //  As expected, these values match quite well the misalignment that we
  //  intentionally introduced in the moving image.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceShifted13x17y}
  // \itkcaption[Fixed and Moving images in registration framework]{Fixed and
  // Moving image provided as input to the registration method.}
  // \label{fig:FixedMovingImageRegistration1}
  // \end{figure}
  //
  //


  //
  //  It is common, as the last step of a registration task, to use the
  //  resulting transform to map the moving image into the fixed image space.
  //  This is easily done with the \doxygen{ResampleImageFilter}. Please
  //  refer to Section~\ref{sec:ResampleImageFilter} for details on the use
  //  of this filter.  First, a ResampleImageFilter type is instantiated
  //  using the image types. It is convenient to use the fixed image type as
  //  the output type since it is likely that the transformed moving image
  //  will be compared with the fixed image.
  //

  using ResampleFilterType = itk::ResampleImageFilter<MovingImageType, FixedImageType>;


  //
  //  A resampling filter is created and the moving image is connected as
  //  its input.
  //

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();
  resampler->SetInput(movingImageReader->GetOutput());


  //
  //  The Transform that is produced as output of the Registration method is
  //  also passed as input to the resampling filter. Note the use of the
  //  methods \code{GetOutput()} and \code{Get()}. This combination is needed
  //  here because the registration method acts as a filter whose output is a
  //  transform decorated in the form of a \doxygen{DataObject}. For details in
  //  this construction you may want to read the documentation of the
  //  \doxygen{DataObjectDecorator}.
  //
  //  \index{itk::ImageRegistrationMethod!Resampling image}
  //  \index{itk::ImageRegistrationMethod!Pipeline}
  //  \index{itk::ImageRegistrationMethod!DataObjectDecorator}
  //  \index{itk::ImageRegistrationMethod!GetOutput()}
  //  \index{itk::DataObjectDecorator!Use in Registration}
  //  \index{itk::DataObjectDecorator!Get()}
  //

  resampler->SetTransform(registration->GetOutput()->Get());


  //
  //  As described in Section \ref{sec:ResampleImageFilter}, the
  //  ResampleImageFilter requires additional parameters to be specified, in
  //  particular, the spacing, origin and size of the output image. The default
  //  pixel value is also set to a distinct gray level in order to highlight
  //  the regions that are mapped outside of the moving image.
  //

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  resampler->SetSize(fixedImage->GetLargestPossibleRegion().GetSize());
  resampler->SetOutputOrigin(fixedImage->GetOrigin());
  resampler->SetOutputSpacing(fixedImage->GetSpacing());
  resampler->SetOutputDirection(fixedImage->GetDirection());
  resampler->SetDefaultPixelValue(100);


  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration1Output}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration1DifferenceBefore}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration1DifferenceAfter}
  // \itkcaption[HelloWorld registration output images]{Mapped moving image and its
  // difference with the fixed image before and after registration}
  // \label{fig:ImageRegistration1Output}
  // \end{figure}
  //


  //
  //  The output of the filter is passed to a writer that will store the
  //  image in a file. An \doxygen{CastImageFilter} is used to convert the
  //  pixel type of the resampled image to the final type used by the
  //  writer. The cast and writer filters are instantiated below.
  //

  using OutputPixelType = unsigned char;

  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using CastFilterType = itk::CastImageFilter<FixedImageType, OutputImageType>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;


  //
  //  The filters are created by invoking their \code{New()}
  //  method.
  //

  WriterType::Pointer     writer = WriterType::New();
  CastFilterType::Pointer caster = CastFilterType::New();


  writer->SetFileName(argv[3]);


  //
  //  The filters are connected together and the \code{Update()} method of the
  //  writer is invoked in order to trigger the execution of the pipeline.
  //

  caster->SetInput(resampler->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();


  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ImageRegistration1Pipeline}
  // \itkcaption[Pipeline structure of the registration example]{Pipeline
  // structure of the registration example.}
  // \label{fig:ImageRegistration1Pipeline}
  // \end{figure}
  //
  //


  //
  //  The fixed image and the transformed moving image can easily be compared
  //  using the \doxygen{SubtractImageFilter}. This pixel-wise filter computes
  //  the difference between homologous pixels of its two input images.
  //

  using DifferenceFilterType = itk::SubtractImageFilter<FixedImageType, FixedImageType, FixedImageType>;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  difference->SetInput1(fixedImageReader->GetOutput());
  difference->SetInput2(resampler->GetOutput());


  //
  //  Note that the use of subtraction as a method for comparing the images is
  //  appropriate here because we chose to represent the images using a pixel
  //  type \code{float}. A different filter would have been used if the pixel
  //  type of the images were any of the \code{unsigned} integer types.
  //


  //
  //  Since the differences between the two images may correspond to very low
  //  values of intensity, we rescale those intensities with a
  //  \doxygen{RescaleIntensityImageFilter} in order to make them more visible.
  //  This rescaling will also make it possible to visualize the negative values
  //  even if we save the difference image in a file format that only supports
  //  unsigned pixel values\footnote{This is the case of PNG, BMP, JPEG and
  //  TIFF among other common file formats.}.  We also reduce the
  //  \code{DefaultPixelValue} to ``1'' in order to prevent that value from
  //  absorbing the dynamic range of the differences between the two images.
  //

  using RescalerType = itk::RescaleIntensityImageFilter<FixedImageType, OutputImageType>;

  RescalerType::Pointer intensityRescaler = RescalerType::New();

  intensityRescaler->SetInput(difference->GetOutput());
  intensityRescaler->SetOutputMinimum(0);
  intensityRescaler->SetOutputMaximum(255);

  resampler->SetDefaultPixelValue(1);


  //
  //  Its output can be passed to another writer.
  //

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput(intensityRescaler->GetOutput());


  if (argc > 4)
  {
    writer2->SetFileName(argv[4]);
    writer2->Update();
  }


  //
  //  For the purpose of comparison, the difference between the fixed image and
  //  the moving image before registration can also be computed by simply
  //  setting the transform to an identity transform. Note that the resampling
  //  is still necessary because the moving image does not necessarily have the
  //  same spacing, origin and number of pixels as the fixed image. Therefore a
  //  pixel-by-pixel operation cannot in general be performed. The resampling
  //  process with an identity transform will ensure that we have a
  //  representation of the moving image in the grid of the fixed image.
  //

  TransformType::Pointer identityTransform = TransformType::New();
  identityTransform->SetIdentity();
  resampler->SetTransform(identityTransform);


  if (argc > 5)
  {
    writer2->SetFileName(argv[5]);
    writer2->Update();
  }


  //
  //  The complete pipeline structure of the current example is presented in
  //  Figure~\ref{fig:ImageRegistration1Pipeline}.  The components of the
  //  registration method are depicted as well.  Figure
  //  \ref{fig:ImageRegistration1Output} (left) shows the result of resampling
  //  the moving image in order to map it onto the fixed image space. The top
  //  and right borders of the image appear in the gray level selected with the
  //  \code{SetDefaultPixelValue()} in the ResampleImageFilter. The center
  //  image shows the difference between the fixed image and the original
  //  moving image (i.e. the difference before the registration is
  //  performed). The right image shows the difference between the fixed image
  //  and the transformed moving image (i.e. after the registration has
  //  been performed).  Both difference images have been rescaled in intensity
  //  in order to highlight those pixels where differences exist.  Note that
  //  the final registration is still off by a fraction of a pixel, which
  //  causes bands around edges of anatomical structures to appear in the
  //  difference image. A perfect registration would have produced a null
  //  difference image.
  //


  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.44\textwidth]{ImageRegistration1TraceTranslations}
  // \includegraphics[height=0.44\textwidth]{ImageRegistration1TraceMetric}
  // \itkcaption[Trace of translations and metrics during registration]{The sequence
  // of translations and metric values at each iteration of the optimizer.}
  // \label{fig:ImageRegistration1Trace}
  // \end{figure}
  //
  //  It is always useful to keep in mind that registration is essentially an
  //  optimization problem. Figure \ref{fig:ImageRegistration1Trace} helps to
  //  reinforce this notion by showing the trace of translations and values of
  //  the image metric at each iteration of the optimizer. It can be seen from
  //  the top figure that the step length is reduced progressively as the
  //  optimizer gets closer to the metric extrema. The bottom plot clearly
  //  shows how the metric value decreases as the optimization advances. The
  //  log plot helps to highlight the normal oscillations of the optimizer
  //  around the extrema value.
  //


  return EXIT_SUCCESS;
}
