/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration9.cxx
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
// This example illustrates the use of the \doxygen{CenteredAffineTransform}
// for performing registration in $2D$. The example code is, for the most part,
// identical to that in
// \ref{sec:InitializingRegistrationWithMoments}.  The main difference is the
// use of the CenteredAffineTransform here instead of the
// \doxygen{CenteredRigid2DTransform}. We will focus on the most
// relevant changes in the current code and skip the basic elements already
// explained in previous examples.
//
// \index{itk::CenteredAffineTransform}
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
//  Let's start by including the header file of the CenteredAffineTransform.
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
//  The following piece of code implements an observer
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
  //  The transform type is instantiated using the code below. The template
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
  //  In this example, we again use the
  //  \doxygen{CenteredTransformInitializer} helper class in order to compute
  //  a reasonable value for the initial center of rotation and the
  //  translation. The initializer is set to use the center of mass of each
  //  image as the initial correspondence correction.
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
  //  Now we pass the parameters of the current transform as the initial
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
  //  \times N$ elements of the parameters array correspond to the rotation
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
  //  We also set the usual parameters of the optimization method. In this
  //  case we are using an
  //  \doxygen{RegularStepGradientDescentOptimizer}. Below, we define the
  //  optimization parameters like initial step length, minimal step length
  //  and number of iterations. These last two act as stopping criteria for
  //  the optimization.
  //
  //  Software Guide : EndLatex 

  double steplength = 0.1;

  if( argc > 6 )
    {
    steplength = atof( argv[6] );
    }


  unsigned int maxNumberOfIterations = 300;

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
  //  \index{itk::Regular\-Step\-Gradient\-Descent\-Optimizer!MinimizeOn()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  optimizer->MinimizeOn();
  // Software Guide : EndCodeSnippet


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
  //  registration method. This is done with the
  //  \code{GetLastTransformParameters()} method. We can also recover the
  //  final value of the metric with the \code{GetValue()} method and the
  //  final number of iterations with the \code{GetCurrentIteration()}
  //  method.
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


  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Center X      = " << finalRotationCenterX  << std::endl;
  std::cout << " Center Y      = " << finalRotationCenterY  << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;
  
  //Compute the rotation angle and scaling from SVD of the matrix
  // \todo Find a way to figure out if the scales are along X or along Y.
  // VNL returns the eigenvalues ordered from largest to smallest.
  
  vnl_matrix<double> p(2, 2);
  p[0][0] = (double) finalParameters[0];
  p[0][1] = (double) finalParameters[1];
  p[1][0] = (double) finalParameters[2];
  p[1][1] = (double) finalParameters[3];
  vnl_svd<double> svd(p);
  vnl_matrix<double> r(2, 2);
  r = svd.U() * vnl_transpose(svd.V());
  double angle = asin(r[1][0]);
  
  std::cout << " Scale 1         = " << svd.W(0)                 << std::endl;
  std::cout << " Scale 2         = " << svd.W(1)                 << std::endl;
  std::cout << " Angle (degrees) = " << angle * 45.0 / atan(1.0) << std::endl;
  

  //  Software Guide : BeginLatex
  //  
  //  Let's execute this example over two of the images provided in
  //  \code{Examples/Data}:
  //  
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png} 
  //  \item \code{BrainProtonDensitySliceR10X13Y17.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first
  //  image by $10$ degrees and then translating by $(-13,-17)$.  Both images
  //  have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration9}. We execute the code using the
  //  following parameters: step length=1.0, translation scale= 0.0001 and
  //  maximum number of iterations = 300. With these images and parameters
  //  the registration takes $240$ iterations and produces
  //
  //  \begin{center}
  //  \begin{verbatim}
  //   239 44.3138   
  //  [0.984935, -0.172989, 0.172608, 0.984926, 
  //    123.249, 147.12, 9.58612, 17.9202]
  //  \end{verbatim}
  //  \end{center}
  //
  //  These results are interpreted as
  //
  //  \begin{itemize}
  //  \item Iterations   = 239
  //  \item Final Metric = 44.3138
  //  \item Center       = $( 123.249,   147.12   )$ millimeters
  //  \item Translation  = $(   9.58612,  17.9202 )$ millimeters
  //  \end{itemize}
  //  
  //  The second component of the matrix values is usually associated with
  //  $\sin{\theta}$. In this case the value $0.1729$ corresponds to a rotation 
  //  of $9.95$ degrees, which is approximately the intentional misalignment of 
  //  $10.0$ degrees.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20.eps}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17S12.eps}
  // \itkcaption[CenteredAffineTransform registration]{Fixed and moving images
  // provided as input to the registration method using the CenteredAffineTransform.}
  // \label{fig:FixedMovingImageRegistration9}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration9Output.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration9DifferenceBefore.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration9DifferenceAfter.eps} 
  // \itkcaption[CenteredAffineTransform ouput images]{The resampled moving image
  // (left), and the difference between the fixed and moving images before (center) 
  // and after (right) registration with the 
  // CenteredAffineTransform transform.}
  // \label{fig:ImageRegistration9Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration9Outputs} shows the output of the
  // registration. The right most image of this figure shows the squared
  // magnitude difference between the fixed image and the resampled
  // moving image. 
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration9TraceMetric.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration9TraceAngle.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration9TraceTranslations.eps} 
  // \itkcaption[CenteredAffineTransform output plots]{Metric values,
  // rotation angle and translations during the registration using the 
  // CenteredAffineTransform transform.}
  // \label{fig:ImageRegistration9Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration9Plots} shows the plots of the main
  //  output parameters of the registration process. The metric values at every
  //  iteration are shown on the top plot. The angle values are shown on the bottom left plot,
  //  while the translation components of the registration are presented
  //  on the bottom right plot. Note that the final total offset of the transform
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

