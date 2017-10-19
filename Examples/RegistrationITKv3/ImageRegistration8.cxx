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
//    INPUTS: brainweb1e1a10f20.mha
//    INPUTS: brainweb1e1a10f20Rot10Tx15.mha
//    ARGUMENTS: ImageRegistration8Output.mhd
//    ARGUMENTS: ImageRegistration8DifferenceBefore.mhd
//    ARGUMENTS: ImageRegistration8DifferenceAfter.mhd
//    OUTPUTS: {ImageRegistration8Output.png}
//    OUTPUTS: {ImageRegistration8DifferenceBefore.png}
//    OUTPUTS: {ImageRegistration8DifferenceAfter.png}
//    OUTPUTS: {ImageRegistration8RegisteredSlice.png}
//  Software Guide : EndCommandLineArgs
// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{VersorRigid3DTransform}
// class for performing registration of two $3D$ images. The example code is
// for the most part identical to the code presented in
// Section~\ref{sec:RigidRegistrationIn2D}.  The major difference is that this
// example is done in $3D$. The class \doxygen{CenteredTransformInitializer} is
// used to initialize the center and translation of the transform.  The case of
// rigid registration of 3D images is probably one of the most commonly found
// cases of image registration.
//
//
// \index{itk::Versor\-Rigid3D\-Transform}
// \index{itk::Centered\-Transform\-Initializer!In 3D}
//
//
// Software Guide : EndLatex
#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"

//  Software Guide : BeginLatex
//
//  The following are the most relevant headers of this example.
//
//  \index{itk::Versor\-Rigid3D\-Transform!header}
//  \index{itk::Centered\-Transform\-Initializer!header}
//
//  Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
#include "itkVersorRigid3DTransform.h"
#include "itkCenteredTransformInitializer.h"
// Software Guide : EndCodeSnippet
//  Software Guide : BeginLatex
//
//  The parameter space of the \code{VersorRigid3DTransform} is not a vector
//  space, due to the fact that addition is not a closed operation in the space
//  of versor components. This precludes the use of standard gradient descent
//  algorithms for optimizing the parameter space of this transform. A special
//  optimizer should be used in this registration configuration. The optimizer
//  designed for this transform is the
//  \doxygen{VersorRigid3DTransformOptimizer}. This optimizer uses Versor
//  composition for updating the first three components of the parameters
//  array, and Vector addition for updating the last three components of the
//  parameters array~\cite{Hamilton1866,Joly1905}.
//
//  \index{itk::Versor\-Rigid3D\-Transform\-Optimizer!header}
//
//  Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
#include "itkVersorRigid3DTransformOptimizer.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkExtractImageFilter.h"

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
  typedef itk::VersorRigid3DTransformOptimizer OptimizerType;
  typedef   const OptimizerType *              OptimizerPointer;
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
    std::cerr << " [differenceAfterRegistration] ";
    std::cerr << " [sliceBeforeRegistration] ";
    std::cerr << " [sliceDifferenceBeforeRegistration] ";
    std::cerr << " [sliceDifferenceAfterRegistration] ";
    std::cerr << " [sliceAfterRegistration] " << std::endl;
    return EXIT_FAILURE;
    }
  const unsigned int                          Dimension = 3;
  typedef  float                              PixelType;
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  //  Software Guide : BeginLatex
  //
  //  The Transform class is instantiated using the code below. The only
  //  template parameter to this class is the representation type of the
  //  space coordinates.
  //
  //  \index{itk::Versor\-Rigid3D\-Transform!Instantiation}
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::VersorRigid3DTransform< double > TransformType;
  // Software Guide : EndCodeSnippet
  typedef itk::VersorRigid3DTransformOptimizer                                  OptimizerType;
  typedef itk::MeanSquaresImageToImageMetric< FixedImageType, MovingImageType > MetricType;
  typedef itk:: LinearInterpolateImageFunction< MovingImageType, double >       InterpolatorType;
  typedef itk::ImageRegistrationMethod< FixedImageType, MovingImageType >       RegistrationType;

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
  //  \index{itk::Versor\-Rigid3D\-Transform!New()}
  //  \index{itk::Versor\-Rigid3D\-Transform!Pointer}
  //  \index{itk::Registration\-Method!SetTransform()}
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
  //  The input images are taken from readers. It is not necessary here to
  //  explicitly call \code{Update()} on the readers since the
  //  \doxygen{CenteredTransformInitializer} will do it as part of its
  //  computations. The following code instantiates the type of the
  //  initializer. This class is templated over the fixed and moving image type
  //  as well as the transform type. An initializer is then constructed by
  //  calling the \code{New()} method and assigning the result to a smart
  //  pointer.
  //
  // \index{itk::Centered\-Transform\-Initializer!Instantiation}
  // \index{itk::Centered\-Transform\-Initializer!New()}
  // \index{itk::Centered\-Transform\-Initializer!SmartPointer}
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredTransformInitializer< TransformType,
                                             FixedImageType,
                                             MovingImageType
                                                 >  TransformInitializerType;
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
  //  \index{Centered\-Transform\-Initializer!MomentsOn()}
  //  \index{Centered\-Transform\-Initializer!GeometryOn()}
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
  //  The rotation part of the transform is initialized using a
  //  \doxygen{Versor} which is simply a unit quaternion.  The
  //  \code{VersorType} can be obtained from the transform traits. The versor
  //  itself defines the type of the vector used to indicate the rotation axis.
  //  This trait can be extracted as \code{VectorType}. The following lines
  //  create a versor object and initialize its parameters by passing a
  //  rotation axis and an angle.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef TransformType::VersorType  VersorType;
  typedef VersorType::VectorType     VectorType;
  VersorType     rotation;
  VectorType     axis;
  axis[0] = 0.0;
  axis[1] = 0.0;
  axis[2] = 1.0;
  const double angle = 0;
  rotation.Set(  axis, angle  );
  transform->SetRotation( rotation );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We now pass the parameters of the current transform as the initial
  //  parameters to be used when the registration process starts.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransformParameters( transform->GetParameters() );
  // Software Guide : EndCodeSnippet
  typedef OptimizerType::ScalesType       OptimizerScalesType;
  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );
  const double translationScale = 1.0 / 1000.0;
  optimizerScales[0] = 1.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = 1.0;
  optimizerScales[3] = translationScale;
  optimizerScales[4] = translationScale;
  optimizerScales[5] = translationScale;
  optimizer->SetScales( optimizerScales );
  optimizer->SetMaximumStepLength( 0.2000  );
  optimizer->SetMinimumStepLength( 0.0001 );
  optimizer->SetNumberOfIterations( 200 );

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

  const double versorX              = finalParameters[0];
  const double versorY              = finalParameters[1];
  const double versorZ              = finalParameters[2];
  const double finalTranslationX    = finalParameters[3];
  const double finalTranslationY    = finalParameters[4];
  const double finalTranslationZ    = finalParameters[5];
  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  const double bestValue = optimizer->GetValue();

  // Print out results
  //
  std::cout << std::endl << std::endl;
  std::cout << "Result = " << std::endl;
  std::cout << " versor X      = " << versorX  << std::endl;
  std::cout << " versor Y      = " << versorY  << std::endl;
  std::cout << " versor Z      = " << versorZ  << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
  std::cout << " Translation Z = " << finalTranslationZ  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;

  //  Software Guide : BeginLatex
  //
  //  Let's execute this example over some of the images available in the ftp
  //  site
  //
  //  \url{ftp://public.kitware.com/pub/itk/Data/BrainWeb}
  //
  //  Note that the images in the ftp site are compressed in \code{.tgz} files.
  //  You should download these files an uncompress them in your local system.
  //  After decompressing and extracting the files you could take a pair of
  //  volumes, for example the pair:
  //
  //  \begin{itemize}
  //  \item \code{brainweb1e1a10f20.mha}
  //  \item \code{brainweb1e1a10f20Rot10Tx15.mha}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first image
  //  by $10$ degrees around the origin and shifting it $15mm$ in $X$.  The
  //  registration takes $24$ iterations and produces:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [-6.03744e-05, 5.91487e-06, -0.0871932, 2.64659, -17.4637, -0.00232496]
  //  \end{verbatim}
  //  \end{center}
  //
  //  That are interpreted as
  //
  //  \begin{itemize}
  //  \item Versor        = $(-6.03744e-05, 5.91487e-06, -0.0871932)$
  //  \item Translation   = $(2.64659,  -17.4637,  -0.00232496)$ millimeters
  //  \end{itemize}
  //
  //  This Versor is equivalent to a rotation of $9.98$ degrees around the $Z$
  //  axis.
  //
  //  Note that the reported translation is not the translation of $(15.0,0.0,0.0)$
  //  that we may be naively expecting. The reason is that the
  //  \code{VersorRigid3DTransform} is applying the rotation around the center
  //  found by the \code{CenteredTransformInitializer} and then adding the
  //  translation vector shown above.
  //
  //  It is more illustrative in this case to take a look at the actual
  //  rotation matrix and offset resulting form the $6$ parameters.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  transform->SetParameters( finalParameters );
  TransformType::MatrixType matrix = transform->GetMatrix();
  TransformType::OffsetType offset = transform->GetOffset();
  std::cout << "Matrix = " << std::endl << matrix << std::endl;
  std::cout << "Offset = " << std::endl << offset << std::endl;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The output of this print statements is
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  Matrix =
  //      0.984795 0.173722 2.23132e-05
  //      -0.173722 0.984795 0.000119257
  //      -1.25621e-06 -0.00012132 1
  //
  //  Offset =
  //      [-15.0105, -0.00672343, 0.0110854]
  //  \end{verbatim}
  //  \end{center}
  //
  //  From the rotation matrix it is possible to deduce that the rotation is
  //  happening in the X,Y plane and that the angle is on the order of
  //  $\arcsin{(0.173722)}$ which is very close to 10 degrees, as we expected.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17}
  // \itkcaption[CenteredTransformInitializer input images]{Fixed and moving image
  // provided as input to the registration method using
  // CenteredTransformInitializer.}
  // \label{fig:FixedMovingImageRegistration8}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration8Output}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration8DifferenceBefore}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration8DifferenceAfter}
  // \itkcaption[CenteredTransformInitializer output images]{Resampled moving
  // image (left). Differences between fixed and moving images, before (center)
  // and after (right) registration with the
  // CenteredTransformInitializer.}
  // \label{fig:ImageRegistration8Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration8Outputs} shows the output of the
  // registration. The center image in this figure shows the differences
  // between the fixed image and the resampled moving image before the
  // registration. The image on the right side presents the difference between
  // the fixed image and the resampled moving image after the registration has
  // been performed. Note that these images are individual slices extracted
  // from the actual volumes. For details, look at the source code of this
  // example, where the ExtractImageFilter is used to extract a slice from the
  // the center of each one of the volumes. One of the main purposes of this
  // example is to illustrate that the toolkit can perform registration on
  // images of any dimension. The only limitations are, as usual, the amount of
  // memory available for the images and the amount of computation time that it
  // will take to complete the optimization process.
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration8TraceMetric}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration8TraceAngle}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration8TraceTranslations}
  // \itkcaption[CenteredTransformInitializer output plots]{Plots of the metric,
  // rotation angle, center of rotation and translations during the
  // registration using CenteredTransformInitializer.}
  // \label{fig:ImageRegistration8Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration8Plots} shows the plots of the main
  //  output parameters of the registration process. The metric values at every
  //  iteration. The Z component of the versor is plotted as an indication of
  //  how the rotation progress. The X,Y translation components of the
  //  registration are plotted at every iteration too.
  //
  //  Shell and Gnuplot scripts for generating the diagrams in
  //  Figure~\ref{fig:ImageRegistration8Plots} are available in the \code{ITKSoftwareGuide}
  //  Git repository under the directory
  //
  //  \code{ITKSoftwareGuide/SoftwareGuide/Art}.
  //
  //  You are strongly encouraged to run the example code, since only in this
  //  way you can gain a first hand experience with the behavior of the
  //  registration process. Once again, this is a simple reflection of the
  //  philosophy that we put forward in this book:
  //
  //  \emph{If you can not replicate it, then it does not exist!}.
  //
  //  We have seen enough published papers with pretty pictures, presenting
  //  results that in practice are impossible to replicate. That is vanity, not
  //  science.
  //
  //  Software Guide : EndLatex

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;
  TransformType::Pointer finalTransform = TransformType::New();
  finalTransform->SetCenter( transform->GetCenter() );
  finalTransform->SetParameters( finalParameters );
  finalTransform->SetFixedParameters( transform->GetFixedParameters() );
  ResampleFilterType::Pointer resampler = ResampleFilterType::New();
  resampler->SetTransform( finalTransform );
  resampler->SetInput( movingImageReader->GetOutput() );
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  resampler->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resampler->SetOutputOrigin(  fixedImage->GetOrigin() );
  resampler->SetOutputSpacing( fixedImage->GetSpacing() );
  resampler->SetOutputDirection( fixedImage->GetDirection() );
  resampler->SetDefaultPixelValue( 100 );

  typedef  unsigned char                                          OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension >                OutputImageType;
  typedef itk::CastImageFilter< FixedImageType, OutputImageType > CastFilterType;
  typedef itk::ImageFileWriter< OutputImageType >                 WriterType;

  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();

  writer->SetFileName( argv[3] );

  caster->SetInput( resampler->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();

  typedef itk::SubtractImageFilter<
                                  FixedImageType,
                                  FixedImageType,
                                  FixedImageType > DifferenceFilterType;
  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  typedef itk::RescaleIntensityImageFilter<
                                  FixedImageType,
                                  OutputImageType >   RescalerType;
  RescalerType::Pointer intensityRescaler = RescalerType::New();
  intensityRescaler->SetInput( difference->GetOutput() );
  intensityRescaler->SetOutputMinimum(   0 );
  intensityRescaler->SetOutputMaximum( 255 );
  difference->SetInput1( fixedImageReader->GetOutput() );
  difference->SetInput2( resampler->GetOutput() );
  resampler->SetDefaultPixelValue( 1 );
  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( intensityRescaler->GetOutput() );

  // Compute the difference image between the
  // fixed and resampled moving image.
  if( argc > 5 )
    {
    writer2->SetFileName( argv[5] );
    writer2->Update();
    }

  typedef itk::IdentityTransform< double, Dimension > IdentityTransformType;
  IdentityTransformType::Pointer identity = IdentityTransformType::New();
  // Compute the difference image between the
  // fixed and moving image before registration.
  if( argc > 4 )
    {
    resampler->SetTransform( identity );
    writer2->SetFileName( argv[4] );
    writer2->Update();
    }
  //
  //  Here we extract slices from the input volume, and the difference volumes
  //  produced before and after the registration.  These slices are presented as
  //  figures in the Software Guide.
  //
  //
  typedef itk::Image< OutputPixelType, 2 > OutputSliceType;
  typedef itk::ExtractImageFilter<
                          OutputImageType,
                          OutputSliceType > ExtractFilterType;
  ExtractFilterType::Pointer extractor = ExtractFilterType::New();
  extractor->SetDirectionCollapseToSubmatrix();
  extractor->InPlaceOn();

  FixedImageType::RegionType inputRegion =
                               fixedImage->GetLargestPossibleRegion();
  FixedImageType::SizeType  size  = inputRegion.GetSize();
  FixedImageType::IndexType start = inputRegion.GetIndex();
  // Select one slice as output
  size[2]  =  0;
  start[2] = 90;
  FixedImageType::RegionType desiredRegion;
  desiredRegion.SetSize(  size  );
  desiredRegion.SetIndex( start );
  extractor->SetExtractionRegion( desiredRegion );
  typedef itk::ImageFileWriter< OutputSliceType > SliceWriterType;
  SliceWriterType::Pointer sliceWriter = SliceWriterType::New();
  sliceWriter->SetInput( extractor->GetOutput() );
  if( argc > 6 )
    {
    extractor->SetInput( caster->GetOutput() );
    resampler->SetTransform( identity );
    sliceWriter->SetFileName( argv[6] );
    sliceWriter->Update();
    }
  if( argc > 7 )
    {
    extractor->SetInput( intensityRescaler->GetOutput() );
    resampler->SetTransform( identity );
    sliceWriter->SetFileName( argv[7] );
    sliceWriter->Update();
    }
  if( argc > 8 )
    {
    resampler->SetTransform( finalTransform );
    sliceWriter->SetFileName( argv[8] );
    sliceWriter->Update();
    }
  if( argc > 9 )
    {
    extractor->SetInput( caster->GetOutput() );
    resampler->SetTransform( finalTransform );
    sliceWriter->SetFileName( argv[9] );
    sliceWriter->Update();
    }
  return EXIT_SUCCESS;
}
