/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration9.cxx
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
// This example illustrates the use of the \doxygen{CenteredAffineTransform}
// for performing registration in $2D$. The code of this example is for the
// most part identical to the one presented in
// \ref{sec:InitializingRegistrationWithMoments}.  The main difference is the
// use of the \doxygen{CenteredAffineTransform} here instead of the
// \doxygen{CenteredRigid2DTransform}. We will focus then in the most relevant
// changes in the current code and skip the basic elements already explained in
// previous examples.
//
// \index{itk::CenteredAffineTransform!textbf}
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
//  Let's start by including the header file of the
//  \doxygen{CenteredAffineTransform}.
//
//  \index{itk::CenteredAffineTransform!header}
// 
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCenteredAffineTransform.h"
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
      std::cout << optimizer->GetCurrentPosition();
  }
};







int main( int argc, char *argv[] )
{


  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << "   fixedImageFile  movingImageFile " << std::endl;
    std::cerr << "   outputImagefile  [differenceOutputfile] [differenceBeforeRegistration] " << std::endl;
    std::cerr << "   [stepLength] [maxNumberOfIterations] "<< std::endl;
    return 1;
    }
  



  //  Software Guide : BeginLatex
  //
  //  We define then the types of the images to be registered.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The Transform type is instantiated using the code below. The template
  //  parameters of this class are the representation type of the space
  //  coordinates and the space dimension.
  //
  //  \index{itk::CenteredAffineTransform!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredAffineTransform< 
                                  double, 
                                  Dimension  >     TransformType;
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
  //  \index{itk::CenteredAffineTransform!New()}
  //  \index{itk::CenteredAffineTransform!Pointer}
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
  //  value for the initial center of rotation and the translation. The
  //  initializer is set to use the center of mass of each image as the initial
  //  correspondance correction.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
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
  // Software Guide : EndCodeSnippet


  // Display initial transform paramters
  std::cout << "Initial Transform Parameters " << std::endl;
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
  //  functionality provided by the optimizers. We know that the first $N
  //  \times N$ elements of the parameters array corresponds to the rotation
  //  matrix factor, the next $N$ correspond to the rotation center, and the
  //  last $N$ are the components of the translation to be applied after
  //  multiplication with the matrix is performed.
  //
  //  Software Guide : EndLatex 


  double translationScale = 1.0 / 1000.0;

  if( argc > 8 )
    {
    translationScale = atof( argv[8] );
    }


  // Software Guide : BeginCodeSnippet
  typedef OptimizerType::ScalesType       OptimizerScalesType;

  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );


  optimizerScales[0] =  1.0;
  optimizerScales[1] =  1.0;
  optimizerScales[2] =  1.0;
  optimizerScales[3] =  1.0;
  optimizerScales[4] =  translationScale;
  optimizerScales[5] =  translationScale;
  optimizerScales[6] =  translationScale;
  optimizerScales[7] =  translationScale;

  optimizer->SetScales( optimizerScales );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  We set also the normal parameters of the optimization method. In this
  //  case we are using an \doxygen{RegularStepGradientDescentOptimizer}. Below,
  //  we define the optimization parameters like initial step length, minimal
  //  step length and number of iterations. These last two act as stopping
  //  criteria for the optimization.
  //
  //  Software Guide : EndLatex 

  double steplength = 0.1;

  if( argc > 6 )
    {
    steplength = atof( argv[6] );
    }


  unsigned int maxNumberOfIterations = 50;

  if( argc > 7 )
    {
    maxNumberOfIterations = atoi( argv[7] );
    }


  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( steplength ); 
  optimizer->SetMinimumStepLength( 0.001 );

  optimizer->SetNumberOfIterations( maxNumberOfIterations );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  We also set the optimizer to do minimization by calling the
  //  \code{MinimizeOn()} method.
  //
  //  \index{itk::RegularStepGradientDescentOptimizer!MinimizeOn()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  optimizer->MinimizeOn();
  // Software Guide : EndCodeSnippet




  //
  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );





  //  Software Guide : BeginLatex
  //
  //  Finally we trigger the execution of the registration method by calling
  //  the \code{Update()} method. The call is placed in a \code{try/catch}
  //  block in case any exceptions are thrown.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
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
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  Once the optimization converges, we recover the parameters from the
  //  registratino method. This is done through the
  //  \code{GetLastTransformParameters()} method. We can also recover the final
  //  value of the metric with the \code{GetValue()} method and the final
  //  number of iterations with the \code{GetCurrentIteration()} method.
  //
  //  \index{itk::RegistrationMethod!GetValue()}
  //  \index{itk::RegistrationMethod!GetCurrentIteration()}
  //  \index{itk::RegistrationMethod!GetLastTransformParameters()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  OptimizerType::ParametersType finalParameters = 
                    registration->GetLastTransformParameters();


  const double finalRotationCenterX = finalParameters[4];
  const double finalRotationCenterY = finalParameters[5];
  const double finalTranslationX    = finalParameters[6];
  const double finalTranslationY    = finalParameters[7];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();
  // Software Guide : EndCodeSnippet

  //
  // Print out results
  //
  std::cout << "Result = " << std::endl;
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
  //  \item \code{BrainProtonDensitySliceR10X13Y17.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first image
  //  by $10$ degrees and then translating by $(-13,-17)$..  Both images have
  //  unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration9}. We execute the code using as
  //  parameters the following: steplenght=1.0, translationScale= 0.0001 and
  //  maximum number of iterations = 300. With these images and parameters the
  //  registration takes $240$ iterations and produce as result the transform
  //  parameters:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //   239 44.3138   
  //  [0.984935, -0.172989, 0.172608, 0.984926, 
  //    123.249, 147.12, 9.58612, 17.9202]
  //  \end{verbatim}
  //  \end{center}
  //
  //  That are interpreted as
  //
  //  \begin{itemize}
  //  \item Iterations   = 239
  //  \item Final Metric = 44.3138
  //  \item Center       = $( 123.249,   147.12   )$ millimeters
  //  \item Translation  = $(   9.58612,  17.9202 )$ millimeters
  //  \end{itemize}
  //  
  //  The second component of the matrix values is usually associated with
  //  $\sin{\theta}$. In this case the value $0.1729$ that indicates a rotation
  //  of $9.95$ degrees. Which matches pretty well the theoretical value of
  //  $10.0$ degrees of the miss-registered image.
  // 
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=6cm]{BrainProtonDensitySliceBorder20.eps}
  // \includegraphics[width=6cm]{BrainProtonDensitySliceR10X13Y17S12.eps}
  // \caption[CenteredAffineTransform registration]{Fixed and Moving image
  // provided as input to the registration method using CenteredAffineTransform
  // transform.}
  // \label{fig:FixedMovingImageRegistration9}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=5cm]{ImageRegistration9Output.eps}
  // \includegraphics[width=5cm]{ImageRegistration9DifferenceBefore.eps}
  // \includegraphics[width=5cm]{ImageRegistration9DifferenceAfter.eps} 
  // \caption[CenteredAffineTransform ouput images]{Resampled moving image
  // (left). Differences between fixed and moving images, before (center) and
  // after (right) registration with the
  // CenteredAffineTransform transform.}
  // \label{fig:ImageRegistration9Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration9Outputs} shows the output of the
  // registration. The right most image of this Figure shows the squared
  // magnitude of pixel differences between the fixed image and the resampled
  // moving image. 
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=5cm]{ImageRegistration9TraceMetric.eps}
  // \includegraphics[height=5cm]{ImageRegistration9TraceAngle.eps}
  // \includegraphics[height=5cm]{ImageRegistration9TraceTranslations.eps} 
  // \caption[CenteredAffineTransform output plots]{Plots of the Metric,
  // rotation angle and translations during the registration using
  // CenteredAffineTransform transform.}
  // \label{fig:ImageRegistration9Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration9Plots} shows the plots of the main
  //  output parameters of the registration process. The metric values at every
  //  iteration are shown on the top. The angle values are shown in the plot at
  //  left while the translation components of the registration are presented
  //  in the plot at right. Note that the final total offset of the transform
  //  is to be computed as a combination of the shift due rotation plus the
  //  explicit translation set on the transform.
  //
  //  Software Guide : EndLatex 




  //  The following code is used to dump output images to files.
  //  They illustrate the final results of the registration.

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
  if( argc > 4 )
    {
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( resample->GetOutput() );
    writer2->SetFileName( argv[4] );
    writer2->Update();
    }


  // Compute the difference image between the 
  // fixed and moving image before registration.
  if( argc > 5 )
    {
    writer2->SetFileName( argv[5] );
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( movingImageReader->GetOutput() );
    writer2->Update();
    }


  return 0;

}

