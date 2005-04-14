/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration5o.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

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
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkOrientedImage.h"


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
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"


//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
#include "itkCommand.h"
class CommandIterationUpdate : public itk::Command 
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:
  typedef itk::RegularStepGradientDescentOptimizer     OptimizerType;
  typedef   const OptimizerType   *    OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
      Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
      OptimizerPointer optimizer = 
        dynamic_cast< OptimizerPointer >( object );
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
    std::cerr << " outputImagefile  [differenceOutputfile] ";
    std::cerr << " [differenceBeforeRegistration] "<< std::endl;
    return 1;
    }
  
  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;

  typedef itk::OrientedImage< PixelType, Dimension >  FixedImageType;
  typedef itk::OrientedImage< PixelType, Dimension >  MovingImageType;


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

  
  //  Software Guide : BeginLatex
  //  
  //  The center of rotation is computed using the origin, size and spacing of
  //  the fixed image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  const FixedImageType::SpacingType& 
    fixedSpacing = fixedImage->GetSpacing();
  const FixedImageType::PointType&
    fixedOrigin  = fixedImage->GetOrigin();
  
  FixedImageType::SizeType fixedSize  = 
          fixedImage->GetLargestPossibleRegion().GetSize();
  
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

  const MovingImageType::SpacingType&
    movingSpacing = movingImage->GetSpacing();
  const MovingImageType::PointType&
    movingOrigin = movingImage->GetOrigin();
  
  MovingImageType::SizeType movingSize = 
            movingImage->GetLargestPossibleRegion().GetSize();
  
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
  //   relating the center of moving image to the center of the fixed image.
  //   This last definition is passed with the method \code{SetTranslation()}.
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
  //  quite different, we take advantage of the scaling functionality
  //  provided by the optimizers. We know that the first element of the
  //  parameters array corresponds to the angle. For this reason we use small
  //  factors in the scales associated with translations and the rotation
  //  center.
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
  //  Below, we define the optimization parameters like initial step length,
  //  minimal step length and number of iterations. These last two act as
  //  stopping criteria for the optimization.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( 0.1    ); 
  optimizer->SetMinimumStepLength( 0.001 );
  optimizer->SetNumberOfIterations( 200 );
  // Software Guide : EndCodeSnippet


  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
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
  const double finalAngleInDegrees = finalAngle * 45.0 / atan(1.0);

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
  //  by $10$ degrees. Both images have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration5}. The registration takes $16$
  //  iterations and produces the results:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [0.177491, 110.487, 128.489, 0.0111713, 0.00250842]
  //  \end{verbatim}
  //  \end{center}
  //
  //  These results are interpreted as
  //
  //  \begin{itemize}
  //  \item Angle         =                     $0.177491$   radians
  //  \item Center        = $( 110.487     , 128.489      )$ millimeters
  //  \item Translation   = $(   0.0111713,   0.00250842 )$ millimeters
  //  \end{itemize}
  // 
  //  As expected, these values match the misalignment
  //  intentionally introduced into the moving image quite well, since $10$ degrees is
  //  about $0.174532$ radians.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20.eps}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceRotated10.eps}
  // \itkcaption[Rigid2D Registration input images]{Fixed and moving images
  // are provided as input to the registration method using the CenteredRigid2D
  // transform.}
  // \label{fig:FixedMovingImageRegistration5}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5Output.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5DifferenceBefore.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5DifferenceAfter.eps} 
  // \itkcaption[Rigid2D Registration output images]{Resampled moving image
  // (left). Differences between the fixed and moving images, before (center) 
  // and after (right) registration using the CenteredRigid2D transform.}
  // \label{fig:ImageRegistration5Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration5Outputs} shows the output of the
  // registration. The rightmost image of this figure shows the squared
  // magnitude difference between the fixed image and the resampled
  // moving image. It can be seen on the difference image that the rotational
  // component has been solved but that the centering misalignment persists.
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceMetric.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceAngle.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceTranslations.eps} 
  // \itkcaption[Rigid2D Registration output plots]{Metric values, rotation
  // angle and translations during registration with the CenteredRigid2D
  // transform.}
  // \label{fig:ImageRegistration5Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration5Plots} shows plots of the main output
  //  parameters produced from the registration process. The metric values at
  //  every iteration are shown on the top. The angle values are shown on the
  //  bottom left plot while the translation components of the registration are
  //  presented on the bottom right plot.
  //
  //  Software Guide : EndLatex 


  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetDefaultPixelValue( 100 );
  
  typedef  unsigned char  OutputPixelType;

  typedef itk::OrientedImage< OutputPixelType, Dimension > OutputImageType;
  
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


  typedef itk::SquaredDifferenceImageFilter< 
                                  FixedImageType, 
                                  FixedImageType, 
                                  OutputImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( difference->GetOutput() );  
  

  // Compute the difference image between the 
  // fixed and resampled moving image.
  if( argc >= 5 )
    {
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( resample->GetOutput() );
    writer2->SetFileName( argv[4] );
    writer2->Update();
    }


  // Compute the difference image between the 
  // fixed and moving image before registration.
  if( argc >= 6 )
    {
    writer2->SetFileName( argv[5] );
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( movingImageReader->GetOutput() );
    writer2->Update();
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
  //  The registration now takes $92$ iterations and produces the following
  //  results:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [0.174474, 109.658, 129.124, 12.9044, 15.8459]
  //  \end{verbatim}
  //  \end{center}
  //
  //  These are interpreted as
  //
  //  \begin{itemize}
  //  \item Angle         =                     $0.174474$   radians
  //  \item Center        = $( 109.658     , 129.124      )$ millimeters
  //  \item Translation   = $(  12.9044    ,  15.8459     )$ millimeters
  //  \end{itemize}
  //  
  //  These values approximately match the initial misalignment intentionally
  //  introduced into the moving image, since $10$ degrees is about $0.174532$
  //  radians. The horizontal translation is well resolved while the vertical
  //  translation ends up being off by a bit more than one millimeter.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20.eps}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17.eps}
  // \itkcaption[Rigid2D Registration input images]{Fixed and moving images
  // provided as input to the registration method using the CenteredRigid2D
  // transform.}
  // \label{fig:FixedMovingImageRegistration5b}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5Output2.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5DifferenceBefore2.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration5DifferenceAfter2.eps} 
  // \itkcaption[Rigid2D Registration output images]{Resampled moving image
  // (left). Differences between the fixed and moving images, before (center) 
  // and after (right) registration with the CenteredRigid2D transform.}
  // \label{fig:ImageRegistration5Outputs2}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration5Outputs2} shows the output of the
  // registration. The rightmost image of this figure shows the squared
  // magnitude difference between the fixed image and the resampled
  // moving image. 
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceMetric2.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceAngle2.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration5TraceTranslations2.eps} 
  // \itkcaption[Rigid2D Registration output plots]{Metric values, rotation
  // angle and translations during the registration using the CenteredRigid2D
  // transform on an image with rotation and translation mis-registration.}
  // \label{fig:ImageRegistration5Plots2}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration5Plots2} shows plots of the main
  //  output registration parameters when the rotation and translations are
  //  combined. The metric values at every iteration are shown at the
  //  top. The angle values are shown in the bottom left plot, while the
  //  translation components of the registration are presented in the bottom 
  //  right plot. It can be seen from the smoothness of these plots that a larger
  //  step length could easily have been supported by the optimizer. You may
  //  want to modify this value in order to get a better idea of how to
  //  tune the parameters.
  //
  //  Software Guide : EndLatex 


  return 0;
}

