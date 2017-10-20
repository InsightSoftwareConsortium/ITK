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
//    INPUTS:  {BrainProtonDensitySliceRotated10.png}
//    OUTPUTS: {ImageRegistration5Output.png}
//    OUTPUTS: {ImageRegistration5DifferenceAfter.png}
//    OUTPUTS: {ImageRegistration5DifferenceBefore.png}
//    ARGUMENTS:    0.1
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceR10X13Y17.png}
//    OUTPUTS: {ImageRegistration5Output2.png}
//    OUTPUTS: {ImageRegistration5DifferenceAfter2.png}
//    OUTPUTS: {ImageRegistration5DifferenceBefore2.png}
//    ARGUMENTS:    1.0
//  Software Guide : EndCommandLineArgs


// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{CenteredRigid2DTransform}
// for performing rigid registration in $2D$. The example code is for the
// most part identical to that presented in Section
// \ref{sec:IntroductionImageRegistration}.  The main difference is the use
// of the CenteredRigid2DTransform here instead of the
// \doxygen{TranslationTransform}.
//
// \index{itk::CenteredRigid2DTransform}
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"


//  Software Guide : BeginLatex
//
//  In addition to the headers included in previous examples, the
//  following header must also be included.
//
//  \index{itk::CenteredRigid2DTransform!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCenteredRigid2DTransform.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"


//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
#include "itkCommand.h"
class CommandIterationUpdate : public itk::Command
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef itk::SmartPointer<Self>   Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:
  typedef itk::RegularStepGradientDescentOptimizer  OptimizerType;
  typedef   const OptimizerType *                   OptimizerPointer;

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
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
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
    std::cerr << " outputImagefile  [differenceAfterRegistration] ";
    std::cerr << " [differenceBeforeRegistration] ";
    std::cerr << " [initialStepLength] "<< std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  unsigned char   PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //
  //  The transform type is instantiated using the code below. The only
  //  template parameter for this class is the representation type of the
  //  space coordinates.
  //
  //  \index{itk::CenteredRigid2DTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredRigid2DTransform< double > TransformType;
  // Software Guide : EndCodeSnippet


  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::MeanSquaresImageToImageMetric<
                                    FixedImageType,
                                    MovingImageType >    MetricType;
  typedef itk:: LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double          >    InterpolatorType;
  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType >    RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetInterpolator(  interpolator  );


  //  Software Guide : BeginLatex
  //
  //  The transform object is constructed below and passed to the registration
  //  method.
  //
  //  \index{itk::CenteredRigid2DTransform!New()}
  //  \index{itk::CenteredRigid2DTransform!Pointer}
  //  \index{itk::RegistrationMethod!SetTransform()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer  transform = TransformType::New();
  registration->SetTransform( transform );
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );


  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );
  fixedImageReader->Update();

  registration->SetFixedImageRegion(
     fixedImageReader->GetOutput()->GetBufferedRegion() );


  //  Software Guide : BeginLatex
  //
  //  In this example, the input images are taken from readers. The code
  //  below updates the readers in order to ensure that the image parameters
  //  (size, origin and spacing) are valid when used to initialize the
  //  transform.  We intend to use the center of the fixed image as the
  //  rotation center and then use the vector between the fixed image center
  //  and the moving image center as the initial translation to be applied
  //  after the rotation.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  fixedImageReader->Update();
  movingImageReader->Update();
  // Software Guide : EndCodeSnippet

  typedef FixedImageType::SpacingType    SpacingType;
  typedef FixedImageType::PointType      OriginType;
  typedef FixedImageType::RegionType     RegionType;
  typedef FixedImageType::SizeType       SizeType;

  //  Software Guide : BeginLatex
  //
  //  The center of rotation is computed using the origin, size and spacing of
  //  the fixed image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  const SpacingType fixedSpacing = fixedImage->GetSpacing();
  const OriginType  fixedOrigin  = fixedImage->GetOrigin();
  const RegionType  fixedRegion  = fixedImage->GetLargestPossibleRegion();
  const SizeType    fixedSize    = fixedRegion.GetSize();

  TransformType::InputPointType centerFixed;

  centerFixed[0] = fixedOrigin[0] + fixedSpacing[0] * fixedSize[0] / 2.0;
  centerFixed[1] = fixedOrigin[1] + fixedSpacing[1] * fixedSize[1] / 2.0;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The center of the moving image is computed in a similar way.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MovingImageType::Pointer movingImage = movingImageReader->GetOutput();

  const SpacingType movingSpacing = movingImage->GetSpacing();
  const OriginType  movingOrigin  = movingImage->GetOrigin();
  const RegionType  movingRegion  = movingImage->GetLargestPossibleRegion();
  const SizeType    movingSize    = movingRegion.GetSize();

  TransformType::InputPointType centerMoving;

  centerMoving[0] = movingOrigin[0] + movingSpacing[0] * movingSize[0] / 2.0;
  centerMoving[1] = movingOrigin[1] + movingSpacing[1] * movingSize[1] / 2.0;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //   The most straightforward method of initializing the transform parameters
  //   is to configure the transform and then get its parameters with the
  //   method \code{GetParameters()}. Here we initialize the transform by
  //   passing the center of the fixed image as the rotation center with the
  //   \code{SetCenter()} method. Then the translation is set as the vector
  //   relating the center of the moving image to the center of the fixed
  //   image.  This last vector is passed with the method
  //   \code{SetTranslation()}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  transform->SetCenter( centerFixed );
  transform->SetTranslation( centerMoving - centerFixed );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Let's finally initialize the rotation with a zero angle.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  transform->SetAngle( 0.0 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Now we pass the current transform's parameters as the initial
  //  parameters to be used when the registration process starts.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransformParameters( transform->GetParameters() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Keeping in mind that the scale of units in rotation and translation is
  //  quite different, we take advantage of the scaling functionality provided
  //  by the optimizers. We know that the first element of the parameters array
  //  corresponds to the angle that is measured in radians, while the other
  //  parameters correspond to translations that are measured in millimeters.
  //  For this reason we use small factors in the scales associated with
  //  translations and the coordinates of the rotation center .
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef OptimizerType::ScalesType       OptimizerScalesType;
  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );
  const double translationScale = 1.0 / 1000.0;

  optimizerScales[0] = 1.0;
  optimizerScales[1] = translationScale;
  optimizerScales[2] = translationScale;
  optimizerScales[3] = translationScale;
  optimizerScales[4] = translationScale;

  optimizer->SetScales( optimizerScales );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Next we set the normal parameters of the optimization method. In this
  //  case we are using an \doxygen{RegularStepGradientDescentOptimizer}.
  //  Below, we define the optimization parameters like the relaxation factor,
  //  initial step length, minimal step length and number of iterations. These
  //  last two act as stopping criteria for the optimization.
  //
  //  \index{Regular\-Step\-Gradient\-Descent\-Optimizer!SetRelaxationFactor()}
  //  \index{Regular\-Step\-Gradient\-Descent\-Optimizer!SetMaximumStepLength()}
  //  \index{Regular\-Step\-Gradient\-Descent\-Optimizer!SetMinimumStepLength()}
  //  \index{Regular\-Step\-Gradient\-Descent\-Optimizer!SetNumberOfIterations()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double initialStepLength = 0.1;
  // Software Guide : EndCodeSnippet

  if( argc > 6 )
    {
    initialStepLength = atof( argv[6] );
    }

  // Software Guide : BeginCodeSnippet
  optimizer->SetRelaxationFactor( 0.6 );
  optimizer->SetMaximumStepLength( initialStepLength );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 200 );
  // Software Guide : EndCodeSnippet


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

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

  OptimizerType::ParametersType finalParameters =
                    registration->GetLastTransformParameters();

  const double finalAngle           = finalParameters[0];
  const double finalRotationCenterX = finalParameters[1];
  const double finalRotationCenterY = finalParameters[2];
  const double finalTranslationX    = finalParameters[3];
  const double finalTranslationY    = finalParameters[4];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();


  // Print out results
  //
  const double finalAngleInDegrees = finalAngle * 180.0 / itk::Math::pi;

  std::cout << "Result = " << std::endl;
  std::cout << " Angle (radians)   = " << finalAngle  << std::endl;
  std::cout << " Angle (degrees)   = " << finalAngleInDegrees  << std::endl;
  std::cout << " Center X      = " << finalRotationCenterX  << std::endl;
  std::cout << " Center Y      = " << finalRotationCenterY  << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


  //  Software Guide : BeginLatex
  //
  //  Let's execute this example over two of the images provided in
  //  \code{Examples/Data}:
  //
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png}
  //  \item \code{BrainProtonDensitySliceRotated10.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first image
  //  by $10$ degrees around the geometrical center of the image. Both images
  //  have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration5}. The registration takes $20$
  //  iterations and produces the results:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [0.177458, 110.489, 128.488, 0.0106296, 0.00194103]
  //  \end{verbatim}
  //  \end{center}
  //
  //  These results are interpreted as
  //
  //  \begin{itemize}
  //  \item Angle         =                  $0.177458$     radians
  //  \item Center        = $( 110.489    , 128.488      )$ millimeters
  //  \item Translation   = $(   0.0106296,   0.00194103 )$ millimeters
  //  \end{itemize}
  //
  //  As expected, these values match the misalignment intentionally introduced
  //  into the moving image quite well, since $10$ degrees is about $0.174532$
  //  radians.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceRotated10}
  // \itkcaption[Rigid2D Registration input images]{Fixed and moving images
  // are provided as input to the registration method using the CenteredRigid2D
  // transform.}
  // \label{fig:FixedMovingImageRegistration5}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5Output}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5DifferenceBefore}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5DifferenceAfter}
  // \itkcaption[Rigid2D Registration output images]{Resampled moving image
  // (left). Differences between the fixed and moving images, before (center)
  // and after (right) registration using the CenteredRigid2D transform.}
  // \label{fig:ImageRegistration5Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration5Outputs} shows from left to right the
  // resampled moving image after registration, the difference between fixed
  // and moving images before registration, and the difference between fixed
  // and resampled moving image after registration. It can be seen from the
  // last difference image that the rotational component has been solved but
  // that a small centering misalignment persists.
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceMetric1}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceAngle1}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceTranslations1}
  // \itkcaption[Rigid2D Registration output plots]{Metric values, rotation
  // angle and translations during registration with the CenteredRigid2D
  // transform.}
  // \label{fig:ImageRegistration5Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration5Plots} shows plots of the main output
  //  parameters produced from the registration process. This includes, the
  //  metric values at every iteration, the angle values at every iteration,
  //  and the translation components of the transform as the registration
  //  progress.
  //
  //  Software Guide : EndLatex


  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );
  finalTransform->SetFixedParameters( transform->GetFixedParameters() );

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 100 );

  typedef itk::ImageFileWriter< FixedImageType >  WriterFixedType;

  WriterFixedType::Pointer      writer =  WriterFixedType::New();

  writer->SetFileName( argv[3] );

  writer->SetInput( resample->GetOutput()   );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "ExceptionObject while writing the resampled image !" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image< float, Dimension > DifferenceImageType;

  typedef itk::SubtractImageFilter<
                           FixedImageType,
                           FixedImageType,
                           DifferenceImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::RescaleIntensityImageFilter<
                                  DifferenceImageType,
                                  OutputImageType >   RescalerType;

  RescalerType::Pointer intensityRescaler = RescalerType::New();

  intensityRescaler->SetOutputMinimum(   0 );
  intensityRescaler->SetOutputMaximum( 255 );

  difference->SetInput1( fixedImageReader->GetOutput() );
  difference->SetInput2( resample->GetOutput() );

  resample->SetDefaultPixelValue( 1 );

  intensityRescaler->SetInput( difference->GetOutput() );

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer      writer2 =  WriterType::New();

  writer2->SetInput( intensityRescaler->GetOutput() );


  try
    {
    // Compute the difference image between the
    // fixed and moving image after registration.
    if( argc > 4 )
      {
      writer2->SetFileName( argv[4] );
      writer2->Update();
      }

    // Compute the difference image between the
    // fixed and resampled moving image after registration.
    TransformType::Pointer identityTransform = TransformType::New();
    identityTransform->SetIdentity();
    resample->SetTransform( identityTransform );
    if( argc > 5 )
      {
      writer2->SetFileName( argv[5] );
      writer2->Update();
      }
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing difference images" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Let's now consider the case in which rotations and translations are
  //  present in the initial registration, as in the following pair
  //  of images:
  //
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png}
  //  \item \code{BrainProtonDensitySliceR10X13Y17.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first image
  //  by $10$ degrees and then translating it $13mm$ in $X$ and $17mm$ in $Y$.
  //  Both images have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration5b}. In order to accelerate
  //  convergence it is convenient to use a larger step length as shown here.
  //
  //  \code{optimizer->SetMaximumStepLength( 1.0 );}
  //
  //  The registration now takes $46$ iterations and produces the following
  //  results:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [0.174454, 110.361, 128.647, 12.977, 15.9761]
  //  \end{verbatim}
  //  \end{center}
  //
  //  These parameters are interpreted as
  //
  //  \begin{itemize}
  //  \item Angle         =                     $0.174454$   radians
  //  \item Center        = $( 110.361     , 128.647      )$ millimeters
  //  \item Translation   = $(  12.977     ,  15.9761     )$ millimeters
  //  \end{itemize}
  //
  //  These values approximately match the initial misalignment intentionally
  //  introduced into the moving image, since $10$ degrees is about $0.174532$
  //  radians. The horizontal translation is well resolved while the vertical
  //  translation ends up being off by about one millimeter.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17}
  // \itkcaption[Rigid2D Registration input images]{Fixed and moving images
  // provided as input to the registration method using the CenteredRigid2D
  // transform.}
  // \label{fig:FixedMovingImageRegistration5b}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5Output2}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5DifferenceBefore2}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5DifferenceAfter2}
  // \itkcaption[Rigid2D Registration output images]{Resampled moving image
  // (left). Differences between the fixed and moving images, before (center)
  // and after (right) registration with the CenteredRigid2D transform.}
  // \label{fig:ImageRegistration5Outputs2}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration5Outputs2} shows the output of the
  // registration. The rightmost image of this figure shows the difference
  // between the fixed image and the resampled moving image after registration.
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceMetric2}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceAngle2}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceTranslations2}
  // \itkcaption[Rigid2D Registration output plots]{Metric values, rotation
  // angle and translations during the registration using the CenteredRigid2D
  // transform on an image with rotation and translation mis-registration.}
  // \label{fig:ImageRegistration5Plots2}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration5Plots2} shows plots of the main output
  //  registration parameters when the rotation and translations are combined.
  //  These results include, the metric values at every iteration, the angle
  //  values at every iteration, and the translation components of the
  //  registration as the registration converges. It can be seen from the
  //  smoothness of these plots that a larger step length could have been
  //  supported easily by the optimizer. You may want to modify this value in
  //  order to get a better idea of how to tune the parameters.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
