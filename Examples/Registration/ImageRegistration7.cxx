/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration7.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{Similarity2DTransform} for
// performing registration in $2D$. The code of this example is for the most
// part identical to the one presented in
// \ref{sec:InitializingRegistrationWithMoments}.  The main difference is the
// use of the \doxygen{Similarity2DTransform} here instead of the
// \doxygen{CenteredRigid2DTransform}.
//
// A similarity transform can be seen as a composition of rotations,
// translations and uniform scaling. It preseves angles and map lines into
// lines. This transform is implemented in the toolkit as deriving from a rigid
// $2D$ transform and with a scale parameter added.
//
// When using this transform, attention should be paid to the fact that scaling
// and translations are not independent.  In the same way that rotations can
// locally be seen as translations, scaling also result in local displacements.
// Scaling is performed in general with respect to the origin of coordinates.
// However, we already saw how ambiguous that could be in the case of
// rotations. For this reason, this transform also allows users to setup a
// specific center. This center is use both for rotation and scaling.
//
//
// \index{itk::Similarity2DTransform!textbf}
//
// Software Guide : EndLatex 

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"


#include "itkCenteredTransformInitializer.h"



//  Software Guide : BeginLatex
//  
//  In addition to the headers included in previous examples, here the
//  following header must be included.
//
//  \index{itk::Similarity2DTransform!header}
// 
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkSimilarity2DTransform.h"
// Software Guide : EndCodeSnippet




#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"




//
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
    if( typeid( event ) != typeid( itk::IterationEvent ) )
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

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //  
  //  The Transform type is instantiated using the code below. The only
  //  template parameter of this class is the representation type of the space
  //  coordinates.
  //
  //  \index{itk::Similarity2DTransform!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Similarity2DTransform< double > TransformType;
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
  //  \index{itk::Similarity2DTransform!New()}
  //  \index{itk::Similarity2DTransform!Pointer}
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
  //  In this example, we use again the helper class
  //  \doxygen{CenteredTransformInitializer} in order to compute a reasonable
  //  value for the initial center of rotation and the translation.
  //
  //  Software Guide : EndLatex 

   typedef itk::CenteredTransformInitializer< 
                                    TransformType, 
                                    FixedImageType, 
                                    MovingImageType >  TransformInitializerType;

  TransformInitializerType::Pointer initializer = TransformInitializerType::New();

  initializer->SetTransform(   transform );

  initializer->SetFixedImage(  fixedImageReader->GetOutput() );
  initializer->SetMovingImage( movingImageReader->GetOutput() );

  initializer->MomentsOn();

  initializer->InitializeTransform();


  //  Software Guide : BeginLatex
  //  
  //  The remaining parameters of the transform are initialized below.
  //
  //  \index{itk::Similarity2DTransform!SetScale()}
  //  \index{itk::Similarity2DTransform!SetAngle()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  transform->SetScale( atof( argv[7] ) );
  transform->SetAngle( atof( argv[8] ) );
  // Software Guide : EndCodeSnippet


  std::cout << transform->GetParameters() << std::endl;



  //  Software Guide : BeginLatex
  //  
  //  We pass now the parameter of the current transform as the initial
  //  parameters to be used when the registration process starts.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransformParameters( 
                                 transform->GetParameters() );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  Keeping in mind that the scale of units in scaling, rotation and
  //  translation are quite different, we take advantage of the scaling
  //  functionality provided by the optimizers. We know that the first element
  //  of the parameters array corresponds to the scale factor, the second
  //  corresponds to the angle, third and forth are the center of rotation and
  //  fifth and sixth are the remaining translation. We use henceforth small
  //  factors in the scales associated with translations and the rotation
  //  center. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef OptimizerType::ScalesType       OptimizerScalesType;

  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );

  const double translationScale = 1.0 / 100.0;

  optimizerScales[0] = 10.0;
  optimizerScales[1] =  1.0;
  optimizerScales[2] =  translationScale;
  optimizerScales[3] =  translationScale;
  optimizerScales[4] =  translationScale;
  optimizerScales[5] =  translationScale;

  optimizer->SetScales( optimizerScales );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  We set also the normal parameters of the optimization method. In this
  //  case we are using A \doxygen{RegularStepGradientDescentOptimizer}. Below, we
  //  define the optimization parameters like initial step length, minimal step
  //  length and number of iterations. These last two act as stopping criteria
  //  for the optimization.
  //
  //  Software Guide : EndLatex 

  const double steplength = atof( argv[6] );

  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( steplength ); 
  optimizer->SetMinimumStepLength( 0.001 );

  optimizer->SetNumberOfIterations( 500 );
  // Software Guide : EndCodeSnippet




  //
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


  const double finalScale           = finalParameters[0];
  const double finalAngle           = finalParameters[1];
  const double finalRotationCenterX = finalParameters[2];
  const double finalRotationCenterY = finalParameters[3];
  const double finalTranslationX    = finalParameters[4];
  const double finalTranslationY    = finalParameters[5];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();

  //
  // Print out results
  //
  const double finalAngleInDegrees = finalAngle * 45.0 / atan(1.0);

  std::cout << "Result = " << std::endl;
  std::cout << " Scale         = " << finalScale  << std::endl;
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
  //  \code{Insight/Examples/Data}, for example:
  //  
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png} 
  //  \item \code{BrainProtonDensitySliceR10X13Y17S12.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first image
  //  by $10$ degrees, scaling by $1/1.2$ and then translating by $(-13,-17)$..
  //  Both images have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration7}. The registration takes $16$
  //  iterations and produce as result the parameters:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [ ]
  //  \end{verbatim}
  //  \end{center}
  //
  //  That are interpreted as
  //
  //  \begin{itemize}
  //  \item Angle         =                     $0.177491$   radians
  //  \item Center        = $( 110.487     , 128.489      )$ millimeters
  //  \item Translation   = $(   0.0111713,   0.00250842 )$ millimeters
  //  \end{itemize}
  //  
  // 
  //  These values match pretty well the misalignment
  //  intentionally introduced in the moving image. Since $10$ degrees is about
  //  $0.174532$ radians.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20.eps}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17S12.eps}
  // \itkcaption{Fixed and Moving image provided as input to the registration
  // method using Similarity2D transform.}
  // \label{fig:FixedMovingImageRegistration7}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration7Output.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration7DifferenceBefore.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration7DifferenceAfter.eps} 
  // \itkcaption{Resampled moving image (left). Differences between fixed and
  // moving images, before (center) and after (right) registration with the
  // Similarity2D transform.}
  // \label{fig:ImageRegistration7Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration7Outputs} shows the output of the
  // registration. The right most image of this Figure shows the squared
  // magnitude of pixel differences between the fixed image and the resampled
  // moving image. 
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration7TraceMetric.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration7TraceAngle.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration7TraceTranslations.eps} 
  // \itkcaption{Plots of the Metric, rotation angle and translations during the registration using 
  // Similarity2D transform.}
  // \label{fig:ImageRegistration7Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration7Plots} shows the plots of the main
  //  output parameters of the registration process. The metric values at every
  //  iteration are shown on the top. The angle values are shown in the plot at
  //  left while the translation components of the registration are presented
  //  in the plot at right.
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

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
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


  return 0;

}

