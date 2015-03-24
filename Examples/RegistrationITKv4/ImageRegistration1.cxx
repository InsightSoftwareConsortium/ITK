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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceShifted13x17y.png}
//    OUTPUTS: {ImageRegistration1Output.png}
//    OUTPUTS: {ImageRegistration1DifferenceAfter.png}
//    OUTPUTS: {ImageRegistration1DifferenceBefore.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
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
// images, a transform, a metric and an optimizer. Some of these components
// are parameterized by the image type for which the registration is intended.
// The following header files provide declarations of common types used for
// these components.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethodv4.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegularStepGradientDescentOptimizerv4.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSubtractImageFilter.h"


class CommandIterationUpdate : public itk::Command
{
public:
  typedef CommandIterationUpdate   Self;
  typedef itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:

  typedef itk::RegularStepGradientDescentOptimizerv4<double> OptimizerType;
  typedef const OptimizerType*                               OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
  {
    Execute( (const itk::Object *)caller, event);
  }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
  {
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );

    if( ! itk::IterationEvent().CheckEvent( &event ) )
      {
      return;
      }

    std::cout << optimizer->GetCurrentIteration() << " = ";
    std::cout << optimizer->GetValue() << " : ";
    std::cout << optimizer->GetCurrentPosition() << std::endl;
  }

};


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile [differenceImageAfter]";
    std::cerr << "[differenceImageBefore] [useEstimator]" << std::endl;
    return EXIT_FAILURE;
    }


  // Software Guide : BeginLatex
  //
  // The type of each registration component should
  // be instantiated first. We start by selecting the image
  // dimension and the types to be used for representing image pixels.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The types of the input images are instantiated by the following lines.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The transform that will map the fixed image space into the moving image
  //  space is defined below.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension > TransformType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  An optimizer is required to explore the parameter space of the transform
  //  in search of optimal values of the metric.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RegularStepGradientDescentOptimizerv4<double> OptimizerType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The metric will compare how well the two images match each other. Metric
  //  types are usually templated over the image types as seen in
  //  the following type declaration.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::MeanSquaresImageToImageMetricv4<
                                          FixedImageType,
                                          MovingImageType >    MetricType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The registration method type is instantiated using the types of the
  //  fixed and moving images as well as the output transform type. This class
  //  is responsible for interconnecting all the components that we have described so far.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageRegistrationMethodv4<
                                    FixedImageType,
                                    MovingImageType,
                                    TransformType   >    RegistrationType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Each one of the registration components is created using its
  //  \code{New()} method and is assigned to its respective
  //  \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Each component is now connected to the instance of the registration method.
  //
  //  \index{itk::RegistrationMethodv4!SetMetric()}
  //  \index{itk::RegistrationMethodv4!SetOptimizer()}
  //  \index{itk::RegistrationMethodv4!SetFixedImage()}
  //  \index{itk::RegistrationMethodv4!SetMovingImage()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In this example the transform object does not need to be created and passed to the
  //  registration method like above since the registration filter will instantiate an internal
  //  transform object using the transform type that is passed to it as a template parameter.
  //
  //  Metric needs an interpolator to evaluate the intensities of the fixed and
  //  moving images at non-grid positions. The types of fixed and moving interpolators
  //  are declared here.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::LinearInterpolateImageFunction<
                                        FixedImageType,
                                        double > FixedLinearInterpolatorType;

  typedef itk::LinearInterpolateImageFunction<
                                        MovingImageType,
                                        double > MovingLinearInterpolatorType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then, fixed and moving interpolators are created and passed to the metric.
  //  Since linear interpolators are used as default, we could skip the following
  //  step in this example.
  //
  //  \index{itk::MeanSquaresImageToImageMetricv4!SetFixedInterpolator()}
  //  \index{itk::MeanSquaresImageToImageMetricv4!SetMovingInterpolator()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FixedLinearInterpolatorType::Pointer fixedInterpolator =
    FixedLinearInterpolatorType::New();
  MovingLinearInterpolatorType::Pointer movingInterpolator =
    MovingLinearInterpolatorType::New();

  metric->SetFixedInterpolator(  fixedInterpolator  );
  metric->SetMovingInterpolator(  movingInterpolator  );
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< FixedImageType  >   FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType >   MovingImageReaderType;
  FixedImageReaderType::Pointer   fixedImageReader     = FixedImageReaderType::New();
  MovingImageReaderType::Pointer  movingImageReader    = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );


  //  Software Guide : BeginLatex
  //
  //  In this example, the fixed and moving images are read from files. This
  //  requires the \doxygen{ImageRegistrationMethodv4} to acquire its inputs
  //  from the output of the readers.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Now the registration process should be initialized. ITKv4 registration
  //  framework provides initial transforms for both fixed and moving images.
  //  These transforms can be used to setup an initial known correction of the
  //  misalignment between the virtual domain and fixed/moving image spaces.
  //  In this particular case, a translation transform is being used for
  //  initialization of the moving image space.
  //  The array of parameters for the initial moving transform is simply composed
  //  of the translation values along each dimension. Setting the values of the
  //  parameters to zero initializes the transform to an \emph{Identity} transform.
  //  Note that the array constructor requires the number of elements to be passed
  //  as an argument.
  //
  //  \index{itk::TranslationTransform!GetNumberOfParameters()}
  //  \index{itk::RegistrationMethodv4!SetMovingInitialTransform()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer movingInitialTransform = TransformType::New();

  TransformType::ParametersType initialParameters(
    movingInitialTransform->GetNumberOfParameters() );
  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y

  movingInitialTransform->SetParameters( initialParameters );

  registration->SetMovingInitialTransform( movingInitialTransform );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  In the registration filter this moving initial transform will be added to a
  //  composite transform that already includes an instantiation of the output optimizable
  //  transform; then, the resultant composite transform will be used by the optimizer to
  //  evaluate the metric values at each iteration.
  //
  //  Despite this, the fixed initial transform does not contribute to the
  //  optimization process. It is only used to access the fixed image from the
  //  virtual image space where the metric evaluation happens.
  //
  //  Virtual images are a new concept added to the ITKv4 registration framework,
  //  which potentially lets us to do the registration process in a physical domain
  //  totally different from the fixed and moving image domains.
  //  In fact, the region over which metric evaluation is performed is called virtual image
  //  domain. This domain defines the resolution at which the evaluation is performed,
  //  as well as the physical coordinate system.
  //
  //  The virtual reference domain is taken from the ``virtual image'' buffered region, and
  //  the input images should be accessed from this reference space using the fixed and moving
  //  initial transforms.
  //
  //  The legacy intuitive registration framework can be considered as a special
  //  case where the virtual domain is the same as the fixed image domain. As this case
  //  practically happens in most of the real life applications, the virtual image is set
  //  to be the same as the fixed image by default. However, the user can define the virtual
  //  domain differently than the fixed image domain by calling either \code{SetVirtualDomain}
  //  or \code{SetVirtualDomainFromImage}.
  //
  //  In this example, like the most examples of this chapter, the virtual image is considered
  //  the same as the fixed image. Since the registration process happens in the fixed image
  //  physical domain, the fixed initial transform maintains its default value of identity and
  //  does not need to be set.
  //
  //  However, a ``Hello World!'' example should show all the basics, so
  //  all the registration components are explicity set here.
  //
  //  In the next section of this chapter, you will get a better understanding
  //  from behind the scenes of the registration process when the initial fixed
  //  transform is not identity.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer   identityTransform = TransformType::New();
  identityTransform->SetIdentity();

  registration->SetFixedInitialTransform( identityTransform );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Note that the above process shows only one way of initializing the registration
  //  configuration. Another option is to initialize the output optimizable transform directly.
  //  In this approach, a transform object is created, initialized, and then passed to
  //  the registration method via \code{SetInitialTransform()}. This approach is shown in
  //  section~\ref{sec:RigidRegistrationIn2D}.
  //
  //  At this point the registration method is ready for execution. The
  //  optimizer is the component that drives the execution of the
  //  registration.  However, the ImageRegistrationMethodv4 class
  //  orchestrates the ensemble to make sure that everything is in place
  //  before control is passed to the optimizer.
  //
  //  It is usually desirable to fine tune the parameters of the optimizer.
  //  Each optimizer has particular parameters that must be interpreted in the
  //  context of the optimization strategy it implements. The optimizer used in
  //  this example is a variant of gradient descent that attempts to prevent it
  //  from taking steps that are too large. At each iteration, this optimizer
  //  will take a step along the direction of the \doxygen{ImageToImageMetricv4}
  //  derivative. Each time the direction of the derivative abruptly changes,
  //  the optimizer assumes that a local extrema has been passed and reacts by
  //  reducing the step length by a relaxation factor. The reducing factor
  //  should have a value between 0 and 1. This factor is set to 0.5 by default,
  //  and it can be changed to a different value via \code{SetRelaxationFactor()}.
  //  Also, the default value for the initial step length is 1, and this value can
  //  be changed manually with the method \code{SetLearningRate()}.
  //
  //  In addition to manual settings, the initial step size can also be estimated
  //  automatically, either at each iteration or only at the first iteration,
  //  by assigning a ScalesEstimator (as will be seen in later examples).
  //
  //  After several reductions of the step length, the optimizer may be moving
  //  in a very restricted area of the transform parameter space. By the method
  //  \code{SetMinimumStepLength()}, the user can define how small the step length
  //  should be to consider convergence to have been reached. This is equivalent
  //  to defining the precision with which the final transform should be known.
  //  User can also set some other stop criteria manually like maximum number of
  //  iterations.
  //
  //  In other gradient descent-based optimizers of the ITKv4 framework, such as
  //  \doxygen{GradientDescentLineSearchOptimizerv4} and \doxygen{ConjugateGradientLineSearchOptimizerv4},
  //  the convergence criteria are set via \code{SetMinimumConvergenceValue()} which is computed based on
  //  the results of the last few iterations. The number of iterations involved in computations
  //  are defined by the convergence window size via \code{SetConvergenceWindowSize()} which is shown
  //  in later examples of this chapter.
  //
  //  Also note that unlike the previous versions, ITKv4 optimizers do not have a ``maximize/minimize''
  //  option to modify the effect of the metric derivatives. Each assigned metric is assumed to
  //  return a parameter derivative result that "improves" the optimization.
  //
  //  \index{itk::Gradient\-Descent\-Optimizerv4\-Template!SetLearningRate()}
  //  \index{itk::Gradient\-Descent\-Optimizerv4\-Template!SetMinimumStepLength()}
  //  \index{itk::Gradient\-Descent\-Optimizerv4\-Template!SetRelaxationFactor()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetLearningRate( 4 );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetRelaxationFactor( 0.5 );
  // Software Guide : EndCodeSnippet

  bool useEstimator = false;
  if( argc > 6 )
    {
    useEstimator = atoi(argv[6]) != 0;
    }

  if( useEstimator )
    {
    typedef itk::RegistrationParameterScalesFromPhysicalShift<MetricType> ScalesEstimatorType;
    ScalesEstimatorType::Pointer scalesEstimator = ScalesEstimatorType::New();
    scalesEstimator->SetMetric( metric );
    scalesEstimator->SetTransformForward( true );
    optimizer->SetScalesEstimator( scalesEstimator );
    optimizer->SetDoEstimateLearningRateOnce( true );
    }


  //  Software Guide : BeginLatex
  //
  //  In case the optimizer never succeeds reaching the desired
  //  precision tolerance, it is prudent to establish a limit on the number of
  //  iterations to be performed. This maximum number is defined with the
  //  method \code{SetNumberOfIterations()}.
  //
  //  \index{itk::Gradient\-Descent\-Optimizerv4\-Template!SetNumberOfIterations()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->SetNumberOfIterations( 200 );
  // Software Guide : EndCodeSnippet


  // Connect an observer
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  //  Software Guide : BeginLatex
  //
  //  ITKv4 facilitates a multi-level registration framework whereby each stage is
  //  different in the resolution of its virtual space and the smoothness of the
  //  fixed and moving images.
  //  These criteria need to be defined before registration starts. Otherwise,
  //  the default values will be used.
  //  In this example, we run a simple registration in one level with no
  //  space shrinking or smoothing on the input data.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfLevels = 1;

  RegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 1 );
  shrinkFactorsPerLevel[0] = 1;

  RegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 1 );
  smoothingSigmasPerLevel[0] = 0;

  registration->SetNumberOfLevels ( numberOfLevels );
  registration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  registration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The registration process is triggered by an invocation of the
  //  \code{Update()} method. If something goes wrong during the
  //  initialization or execution of the registration an exception will be
  //  thrown. We should therefore place the \code{Update()} method
  //  inside a \code{try/catch} block as illustrated in the following lines.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    registration->Update();
    std::cout << "Optimizer stop condition: "
    << registration->GetOptimizer()->GetStopConditionDescription()
    << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In a real life application, you may attempt to recover from the error by
  //  taking more effective actions in the catch block. Here we are simply
  //  printing out a message and then terminating the execution of the program.
  //
  //
  //  The result of the registration process is obtained using the \code{GetTransform()}
  //  method that returns a constant pointer to the output transform.
  //
  //  \index{itk::ImageRegistrationMethodv4!GetTransform()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::ConstPointer transform = registration->GetTransform();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In the case of the \doxygen{TranslationTransform}, there is a
  //  straightforward interpretation of the parameters.  Each element of the
  //  array corresponds to a translation along one spatial dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::ParametersType finalParameters = transform->GetParameters();
  const double TranslationAlongX = finalParameters[0];
  const double TranslationAlongY = finalParameters[1];
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The optimizer can be queried for the actual number of iterations
  //  performed to reach convergence.  The \code{GetCurrentIteration()}
  //  method returns this value. A large number of iterations may be an
  //  indication that the learning rate has been set too small, which
  //  is undesirable since it results in long computational times.
  //
  //  \index{itk::Gradient\-Descent\-Optimizerv4\-Template!GetCurrentIteration()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The value of the image metric corresponding to the last set of parameters
  //  can be obtained with the \code{GetValue()} method of the optimizer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const double bestValue = optimizer->GetValue();
  // Software Guide : EndCodeSnippet


  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX  << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


  //  Software Guide : BeginLatex
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
  //  registration takes 20 iterations and the resulting transform parameters are:
  //
  //  \begin{verbatim}
  //  Translation X = 13.0012
  //  Translation Y = 16.9999
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
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  It is common, as the last step of a registration task, to use the
  //  resulting transform to map the moving image into the fixed image space.
  //
  //  Before the mapping process, notice that we have not used the direct initialization
  //  of the output transform in this example, so the parameters of the moving initial
  //  transform are not reflected in the output parameters of the registration filter.
  //  Hence, a composite transform is needed to concatenate both initial and output
  //  transforms together.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CompositeTransform<
                                 double,
                                 Dimension > CompositeTransformType;
  CompositeTransformType::Pointer outputCompositeTransform =
    CompositeTransformType::New();
  outputCompositeTransform->AddTransform( movingInitialTransform );
  outputCompositeTransform->AddTransform(
    registration->GetModifiableTransform() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Now the mapping process is easily done with the \doxygen{ResampleImageFilter}.
  //  Please refer to Section~\ref{sec:ResampleImageFilter} for details on the use
  //  of this filter.  First, a ResampleImageFilter type is instantiated
  //  using the image types. It is convenient to use the fixed image type as
  //  the output type since it is likely that the transformed moving image
  //  will be compared with the fixed image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  A resampling filter is created and the moving image is connected as
  //  its input.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ResampleFilterType::Pointer resampler = ResampleFilterType::New();
  resampler->SetInput( movingImageReader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The created output composite transform is also passed as input to the
  //  resampling filter.
  //
  //  \index{itk::ImageRegistrationMethod!Resampling image}
  //  \index{itk::ImageRegistrationMethod!Pipeline}
  //  \index{itk::ImageRegistrationMethod!DataObjectDecorator}
  //  \index{itk::ImageRegistrationMethod!GetOutput()}
  //  \index{itk::DataObjectDecorator!Use in Registration}
  //  \index{itk::DataObjectDecorator!Get()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  resampler->SetTransform( outputCompositeTransform );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  As described in Section \ref{sec:ResampleImageFilter}, the
  //  ResampleImageFilter requires additional parameters to be specified, in
  //  particular, the spacing, origin and size of the output image. The default
  //  pixel value is also set to a distinct gray level in order to highlight
  //  the regions that are mapped outside of the moving image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  resampler->SetSize( fixedImage->GetLargestPossibleRegion().GetSize() );
  resampler->SetOutputOrigin(  fixedImage->GetOrigin() );
  resampler->SetOutputSpacing( fixedImage->GetSpacing() );
  resampler->SetOutputDirection( fixedImage->GetDirection() );
  resampler->SetDefaultPixelValue( 100 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
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
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  The output of the filter is passed to a writer that will store the
  //  image in a file. An \doxygen{CastImageFilter} is used to convert the
  //  pixel type of the resampled image to the final type used by the
  //  writer. The cast and writer filters are instantiated below.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char                            OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType >          CastFilterType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filters are created by invoking their \code{New()}
  //  method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();
  // Software Guide : EndCodeSnippet


  writer->SetFileName( argv[3] );


  //  Software Guide : BeginLatex
  //
  //  The filters are connected together and the \code{Update()} method of the
  //  writer is invoked in order to trigger the execution of the pipeline.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  caster->SetInput( resampler->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
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
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  The fixed image and the transformed moving image can easily be compared
  //  using the \doxygen{SubtractImageFilter}. This pixel-wise filter computes
  //  the difference between homologous pixels of its two input images.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::SubtractImageFilter<
                                  FixedImageType,
                                  FixedImageType,
                                  FixedImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  difference->SetInput1( fixedImageReader->GetOutput() );
  difference->SetInput2( resampler->GetOutput() );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  //  Note that the use of subtraction as a method for comparing the images is
  //  appropriate here because we chose to represent the images using a pixel
  //  type \code{float}. A different filter would have been used if the pixel
  //  type of the images were any of the \code{unsigned} integer types.
  //
  // Software Guide : EndLatex


  //  Software Guide : BeginLatex
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
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RescaleIntensityImageFilter<
                                  FixedImageType,
                                  OutputImageType >   RescalerType;

  RescalerType::Pointer intensityRescaler = RescalerType::New();

  intensityRescaler->SetInput( difference->GetOutput() );
  intensityRescaler->SetOutputMinimum(   0 );
  intensityRescaler->SetOutputMaximum( 255 );

  resampler->SetDefaultPixelValue( 1 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Its output can be passed to another writer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( intensityRescaler->GetOutput() );
  // Software Guide : EndCodeSnippet


  if( argc > 4 )
    {
    writer2->SetFileName( argv[4] );
    writer2->Update();
    }


  //  Software Guide : BeginLatex
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
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  resampler->SetTransform( identityTransform );
  // Software Guide : EndCodeSnippet


  if( argc > 5 )
    {
    writer2->SetFileName( argv[5] );
    writer2->Update();
    }


  //  Software Guide : BeginLatex
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
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
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
  //  In this section, we used a very simple example to introduce the basic components
  //  of a registration process in ITKv4. However, studying this example alone is not
  //  enough to start using the \doxygen{ImageRegistrationMethodv4}.
  //  In order to choose the best registration practice for a specific application,
  //  knowledge of other registration method instantiations and their capabilities are
  //  required.
  //  For example, direct initialization of the output optimizable transform is shown in
  //  section~\ref{sec:RigidRegistrationIn2D}. This method can simplify the registration
  //  process in many cases. Also, multi-resolution and multistage registration approaches
  //  are illustrated in sections~\ref{sec:MultiResolutionRegistration} and
  //  ~\ref{sec:MultiStageRegistration}.
  //  These examples illustrate the flexibility in the usage of ITKv4 registration method
  //  framework that can help to provide faster and more reliable registration processes.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
