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
//    INPUTS:  {BrainProtonDensitySliceR10X13Y17.png}
//    OUTPUTS: {ImageRegistration6Output.png}
//    OUTPUTS: {ImageRegistration6DifferenceBefore.png}
//    OUTPUTS: {ImageRegistration6DifferenceAfter.png}
//  Software Guide : EndCommandLineArgs


// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{CenteredRigid2DTransform}
// for performing registration. The example code is for the most part
// identical to the one presented in Section~\ref{sec:RigidRegistrationIn2D}.
// Even though this current example is done in $2D$, the class
// \doxygen{CenteredTransformInitializer} is quite generic and could be used
// in other dimensions. The objective of the initializer class is to simplify
// the computation of the center of rotation and the translation required to
// initialize certain transforms such as the
// CenteredRigid2DTransform. The initializer accepts two images and
// a transform as inputs. The images are considered to be the fixed and
// moving images of the registration problem, while the transform is the one
// used to register the images.
//
// The CenteredRigid2DTransform supports two modes of operation. In the first
// mode, the centers of the images are computed as space coordinates using the
// image origin, size and spacing. The center of the fixed image is assigned as
// the rotational center of the transform while the vector going from the fixed
// image center to the moving image center is passed as the initial translation
// of the transform. In the second mode, the image centers are not computed
// geometrically but by using the moments of the intensity gray levels. The
// center of mass of each image is computed using the helper class
// \doxygen{ImageMomentsCalculator}.  The center of mass of the fixed image is
// passed as the rotational center of the transform while the vector going from
// the fixed image center of mass to the moving image center of mass is passed
// as the initial translation of the transform. This second mode of operation
// is quite convenient when the anatomical structures of interest are not
// centered in the image. In such cases the alignment of the centers of mass
// provides a better rough initial registration than the simple use of the
// geometrical centers.  The validity of the initial registration should be
// questioned when the two images are acquired in different imaging modalities.
// In those cases, the center of mass of intensities in one modality does not
// necessarily match the center of mass of intensities in the other imaging
// modality.
//
// \index{itk::CenteredRigid2DTransform}
// \index{itk::ImageMomentsCalculator}
//
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethodv4.h"
#include "itkMeanSquaresImageToImageMetricv4.h"
#include "itkRegularStepGradientDescentOptimizerv4.h"


//  Software Guide : BeginLatex
//
//  The following are the most relevant headers in this example.
//
//  \index{itk::CenteredRigid2DTransform!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredTransformInitializer.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSubtractImageFilter.h"


//
//  The following section of code implements a command observer
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
  typedef itk::RegularStepGradientDescentOptimizerv4<double>  OptimizerType;
  typedef   const OptimizerType *                             OptimizerPointer;

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
    std::cerr << " outputImagefile  [differenceBeforeRegistration] ";
    std::cerr << " [differenceAfterRegistration] "<< std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //
  //  The transform type is instantiated using the code below. The only
  //  template parameter of this class is the representation type of the
  //  space coordinates.
  //
  //  \index{itk::CenteredRigid2DTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredRigid2DTransform< double > TransformType;
  // Software Guide : EndCodeSnippet


  typedef itk::RegularStepGradientDescentOptimizerv4<double>  OptimizerType;
  typedef itk::MeanSquaresImageToImageMetricv4<
                                    FixedImageType,
                                    MovingImageType >         MetricType;
  typedef itk::ImageRegistrationMethodv4<
                                    FixedImageType,
                                    MovingImageType >         RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();


  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );


  //  Software Guide : BeginLatex
  //
  //  Like the previous section, a direct initialization method is used here.
  //  The transform object is constructed below. This transform will
  //  be initialized, and its initial parameters will be considered as
  //  the parameters to be used when the registration process begins.
  //
  //  \index{itk::CenteredRigid2DTransform!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer  transform = TransformType::New();
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;
  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );


  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );


  //  Software Guide : BeginLatex
  //
  //  The input images are taken from readers. It is not necessary to
  //  explicitly call \code{Update()} on the readers since the
  //  CenteredTransformInitializer class will do it as part of its
  //  initialization. The following code instantiates the initializer. This
  //  class is templated over the fixed and moving images type as well as the
  //  transform type. An initializer is then constructed by calling the
  //  \code{New()} method and assigning the result to a
  //  \doxygen{SmartPointer}.
  //
  // \index{itk::CenteredRigid2DTransform!Instantiation}
  // \index{itk::CenteredRigid2DTransform!New()}
  // \index{itk::CenteredRigid2DTransform!SmartPointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredTransformInitializer<
    TransformType,
    FixedImageType,
    MovingImageType > TransformInitializerType;

  TransformInitializerType::Pointer initializer =
    TransformInitializerType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The initializer is now connected to the transform and to the fixed and
  //  moving images.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  initializer->SetTransform(   transform );
  initializer->SetFixedImage(  fixedImageReader->GetOutput() );
  initializer->SetMovingImage( movingImageReader->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The use of the geometrical centers is selected by calling
  //  \code{GeometryOn()} while the use of center of mass is selected by
  //  calling \code{MomentsOn()}.  Below we select the center of mass mode.
  //
  //  \index{CenteredTransformInitializer!MomentsOn()}
  //  \index{CenteredTransformInitializer!GeometryOn()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  initializer->MomentsOn();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally, the computation of the center and translation is triggered by
  //  the \code{InitializeTransform()} method. The resulting values will be
  //  passed directly to the transform.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  initializer->InitializeTransform();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The remaining parameters of the transform are initialized as before.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  transform->SetAngle( 0.0 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Now the initialized transform object will be set to the registration method,
  //  and the starting point of the registration is defined by its initial parameters.
  //
  //  If the \code{InPlaceOn()} method is called, this initialized transform will be the output transform
  //  object or ``grafted'' to the output. Otherwise, this ``InitialTransform'' will be deep-copied or
  //  ``cloned'' to the output.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransform( transform );
  registration->InPlaceOn();
  // Software Guide : EndCodeSnippet


  typedef OptimizerType::ScalesType       OptimizerScalesType;
  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );
  const double translationScale = 1.0 / 1000.0;

  optimizerScales[0] = 1.0;
  optimizerScales[1] = translationScale;
  optimizerScales[2] = translationScale;
  optimizerScales[3] = translationScale;
  optimizerScales[4] = translationScale;

  optimizer->SetScales( optimizerScales );

  optimizer->SetLearningRate( 0.1 );
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 200 );


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  // One level registration process without shrinking and smoothing.
  //
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


  //  Software Guide : BeginLatex
  //
  //  Since the registration filter has \code{InPlace} set, the transform object
  //  is grafted to the output and is updated by the registration method.
  //
  //  Software Guide : EndLatex

  TransformType::ParametersType finalParameters = transform->GetParameters();


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
  std::cout << " Angle (radians) " << finalAngle  << std::endl;
  std::cout << " Angle (degrees) " << finalAngleInDegrees  << std::endl;
  std::cout << " Center X      = " << finalRotationCenterX  << std::endl;
  std::cout << " Center Y      = " << finalRotationCenterY  << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


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
  //  \ref{fig:FixedMovingImageRegistration5}. The registration takes $22$
  //  iterations and produces:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [0.17429, 111.172, 131.563, 12.4582, 16.0724]
  //  \end{verbatim}
  //  \end{center}
  //
  //  These parameters are interpreted as
  //
  //  \begin{itemize}
  //  \item Angle         =                  $0.17429$     radians
  //  \item Center        = $( 111.172    , 131.563      )$ millimeters
  //  \item Translation   = $(  12.4582   ,  16.0724     )$ millimeters
  //  \end{itemize}
  //
  //  Note that the reported translation is not the translation of $(13,17)$
  //  that might be expected. The reason is that the five parameters of the
  //  CenteredRigid2DTransform are redundant. The actual movement
  //  in space is described by only $3$ parameters. This means that there are
  //  infinite combinations of rotation center and translations that will
  //  represent the same actual movement in space. It is more illustrative in
  //  this case to take a look at the actual rotation matrix and offset
  //  resulting from the five parameters.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TransformType::MatrixType matrix = transform->GetMatrix();
  TransformType::OffsetType offset = transform->GetOffset();

  std::cout << "Matrix = " << std::endl << matrix << std::endl;
  std::cout << "Offset = " << std::endl << offset << std::endl;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Which produces the following output.
  //
  //  \begin{verbatim}
  //  Matrix =
  //     0.98485 -0.173409
  //     0.173409 0.98485
  //
  //  Offset =
  //     [36.9567, -1.21272]
  //  \end{verbatim}
  //
  //  This output illustrates how counter-intuitive the mix of center of
  //  rotation and translations can be. Figure
  //  \ref{fig:TranslationAndRotationCenter} will clarify this situation. The
  //  figure shows the original image on the left. A rotation of $10^{\circ}$
  //  around the center of the image is shown in the middle. The same rotation
  //  performed around the origin of coordinates is shown on the right. It can
  //  be seen here that changing the center of rotation introduces additional
  //  translations.
  //
  //  Let's analyze what happens to the center of the image that we just
  //  registered. Under the point of view of rotating $10^{\circ}$ around the
  //  center and then applying a translation of $(13mm,17mm)$. The image has
  //  a size of $(221 \times 257)$ pixels and unit spacing. Hence its center
  //  has coordinates $(110.5,128.5)$. Since the rotation is done around this
  //  point, the center behaves as the fixed point of the transformation and
  //  remains unchanged. Then with the $(13mm,17mm)$ translation it is mapped
  //  to $(123.5,145.5)$ which becomes its final position.
  //
  //  The matrix and offset that we obtained at the end of the registration
  //  indicate that this should be equivalent to a rotation of $10^{\circ}$
  //  around the origin, followed by a translation of $(36.95,-1.21)$. Let's
  //  compute this in detail. First the rotation of the image center by
  //  $10^{\circ}$ around the origin will move the point to
  //  $(86.52,147.97)$. Now, applying a translation of $(36.95,-1.21)$ maps
  //  this point to $(123.47,146.76)$, which is close to the result of our
  //  previous computation.
  //
  //  It is unlikely that we could have chosen these translations as the
  //  initial guess, since we tend to think about images in a coordinate
  //  system whose origin is in the center of the image.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{TranslationAndRotationCenter}
  // \itkcaption[Effect of changing the center of rotation]{Effect of changing
  // the center of rotation.}
  // \label{fig:TranslationAndRotationCenter}
  // \end{figure}
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  You may be wondering why the actual movement is represented by three
  //  parameters when we take the trouble of using five. In particular, why
  //  use a $5$-dimensional optimizer space instead of a $3$-dimensional
  //  one? The answer is that by using five parameters we have a much simpler
  //  way of initializing the transform with the rotation matrix and
  //  offset. Using the minimum three parameters it is not obvious how to
  //  determine what the initial rotation and translations should be.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17}
  // \itkcaption[CenteredTransformInitializer input images]{Fixed and moving
  // images provided as input to the registration method using
  // CenteredTransformInitializer.}
  // \label{fig:FixedMovingImageRegistration6}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration6Output}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration6DifferenceBefore}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration6DifferenceAfter}
  // \itkcaption[CenteredTransformInitializer output images]{Resampled moving
  // image (left). Differences between fixed and moving images, before
  // registration (center) and after registration (right) with the
  // CenteredTransformInitializer.}
  // \label{fig:ImageRegistration6Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration6Outputs} shows the output of the
  // registration. The image on the right of this figure shows the differences
  // between the fixed image and the resampled moving image after registration.
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration6TraceMetric}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration6TraceAngle}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration6TraceTranslations}
  // \itkcaption[CenteredTransformInitializer output plots]{Plots of the Metric,
  // rotation angle, center of rotation and translations during the
  // registration using CenteredTransformInitializer.}
  // \label{fig:ImageRegistration6Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration6Plots} plots the output parameters of
  //  the registration process. It includes the metric values at every
  //  iteration, the angle values at every iteration, and the values of the
  //  translation components as the registration progresses. Note that this is
  //  the complementary translation as used in the transform, not the actual
  //  total translation that is used in the transform offset. We could modify
  //  the observer to print the total offset instead of printing the array of
  //  parameters. Let's call that an exercise for the reader!
  //
  //  Software Guide : EndLatex


  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( transform );
  resample->SetInput( movingImageReader->GetOutput() );

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 100 );

  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType > CastFilterType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();


  writer->SetFileName( argv[3] );


  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();

  // Now compute the difference between the images
  // before and after registration.
  //
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
    if( argc > 5 )
      {
      writer2->SetFileName( argv[5] );
      writer2->Update();
      }

    // Compute the difference image between the
    // fixed and resampled moving image after registration.
    TransformType::Pointer identityTransform = TransformType::New();
    identityTransform->SetIdentity();
    resample->SetTransform( identityTransform );
    if( argc > 4 )
      {
      writer2->SetFileName( argv[4] );
      writer2->Update();
      }
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing difference images" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
