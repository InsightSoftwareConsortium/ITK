/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration4.cxx
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
// In this example, we will solve the our simple multi-modality problem
// using another implementation of mutual information. One of main 
// difference between \code{MattesMutualInformationImageToImageMetric}
// and \code{MutualInformationImageToImageMetric} is than only one
// spatial sample set is used for the whole registration process instead
// of using new samples every iteration. The use of a single sample
// set results in a much smoother cost function and hence allows
// the use of more intelligent optimizers. In this example, we
// will use \code{RegularStepGradientDescentOptimizer}.
// Another noticeable difference is that pre-normalization of the
// images is not necessary as the metric rescales internally when
// building up the discrete density functions. 
// Other differences between
// the two mutual information implementation is described in detail
// in Section \ref{sec:MutualInformationMetric}.
//
// \index{itk::ImageRegistrationMethod!Multi-Modality|textbf}
//
// The following headers declare the basic components of 
// the registration method.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"



int main( int argc, char **argv )
{


  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile [differenceImage]" << std::endl;
    return 1;
    }
  
  const    unsigned int    Dimension = 2;
  typedef  unsigned short  PixelType;
  
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //  
  //  In this example the image types and all registration components
  //  apart from the metric is declared as in section 
  //  \ref{sec:IntroductionImageRegistration}.
  //
  //  Software Guide : EndLatex 

  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType    > RegistrationType;

  //  Software Guide : BeginLatex
  //  
  //  The Mattes mutual information metric type is 
  //  instantiated using the image types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::MattesMutualInformationImageToImageMetric< 
                                          FixedImageType, 
                                          MovingImageType >    MetricType;
  // Software Guide : EndCodeSnippet



  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );
  


  //  Software Guide : BeginLatex
  //  
  //  The metric is created using the \code{New()} method and then
  //  connected to the registration object.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  MetricType::Pointer         metric        = MetricType::New();
  
  registration->SetMetric( metric  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The metric requires a two of parameters to be selected: the number
  //  of bins used to compute the entropy and the number of spatial samples
  //  used to compute the density estimates. In a typical scenario, 50 
  //  histogram bins is sufficient and the metric is relatively insensitive
  //  to changes in the number of bins. The number of spatial samples
  //  to be used depends on the content of the image. If the images are
  //  smooth and does not contain much detail, then using approximatedly
  //  one percent of the pixels will do. On the other hand, if the images
  //  are detailed, it may be necessary to use a much higher proportion,
  //  say 20 percent.
  //
  //  \index{itk::MattesMutualInformationImageToImageMetric!SetNumberOfHistogramBins()}
  //  \index{itk::MattesMutualInformationImageToImageMetric!SetNumberOfSpatialSamples()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  metric->SetNumberOfHistogramBins( 50 );
  metric->SetNumberOfSpatialSamples( 1000 );
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
  


  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y
  
  registration->SetInitialTransformParameters( initialParameters );


  //  Software Guide : BeginLatex
  //  
  //  Another significant difference in the metric is that it
  //  computes the negative mutual information and hence we
  //  need to minimize the cost function in this case. In this
  //  example we will use the same optimizer parameters as in
  //  section \ref{sec:sec:IntroductionImageRegistration}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( 4.00 );  
  optimizer->SetMinimumStepLength( 0.01 );
  optimizer->SetNumberOfIterations( 200 );

  // Software Guide : EndCodeSnippet


  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    } 

  ParametersType finalParameters = registration->GetLastTransformParameters();
  
  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];
  
  unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  
  double bestValue = optimizer->GetValue();


  //
  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX  << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


  //  Software Guide : BeginLatex
  //  
  //  Let's execute this example using the same multi-modality images
  //  as before.
  //  The registration converged after 24 iterations and produce as result the
  //  parameters:
  //
  //  \begin{verbatim}
  //  Translation X = 13.1719
  //  Translation Y = 16.9006
  //  \end{verbatim}
  // 
  //  These values are very close match to 
  //  the true misaligment introduced in the moving image.
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



  //  Software Guide : BeginLatex
  // 
  // \begin{figure}
  // \center
  // \includegraphics[width=5cm]{ImageRegistration4Output.eps}
  // \includegraphics[width=5cm]{ImageRegistration4CheckerboardBefore.eps}
  // \includegraphics[width=5cm]{ImageRegistration4CheckerboardAfter.eps}
  // \caption{Mapped moving image (left) and composition of fixed and moving
  // images before (center) and after (right) registration.}
  // \label{fig:ImageRegistration4Output}
  // \end{figure}
  //
  //  The result of the resampling the moving image is presented in the left
  //  side of Figure \ref{fig:ImageRegistration4Output}. The center and right
  //  parts of the figure present a checkerboard composite of the fixed and
  //  moving images before and after registration.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=7cm]{ImageRegistration2TraceTranslations.eps}
  // \includegraphics[width=7cm]{ImageRegistration2TraceTranslations2.eps}
  // \caption{Sequence of translations during the registration process. Left,
  // iterations form 0 to 200. Right iterations from 150 to 200.}
  // \label{fig:ImageRegistration2TraceTranslations}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration2TraceTranslations} presents the
  //  sequence of translations followed by the optimizer as it searched the
  //  parameter space. The left plot shows iterations $0$ to $200$ while the
  //  right figure zooms into iterations $150$ to $200$. The area covered by
  //  the right figure has been highlighted by a rectangle in the left image.
  //  It can be seen that after a certain number of iterations the optimizer
  //  oscillates within a one or two pixels of the true solution. 
  //  At this point it is
  //  clear that more iterations will not help. Instead it is time to modify
  //  some of the parameters of the registration process. For example, 
  //  reducing the learning rate of the optimizer and continuing the
  //  registration so that smaller steps are taken.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=7cm]{ImageRegistration2TraceMetric.eps}
  // \includegraphics[width=7cm]{ImageRegistration2TraceMetric2.eps}
  // \caption{Sequence of metric values during the registration process. Left,
  // iterations form 0 to 300. Right, iterations from 100 to 200.}
  // \label{fig:ImageRegistration2TraceMetric}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration2TraceMetric} shows the sequence of
  //  metric values computed as the optimizer searched the parameter space.
  //  The left plot shows values when iterations are extended from $0$ to $300$
  //  while the right figure zooms into iterations $100$ to $200$. 
  //  The fluctuations in the measure value is due to the stochastic
  //  nature in which the measure is computed. At each call of 
  //  \code{GetValue()}, two new sets of intensity samples is randomly 
  //  taken from the image to compute the density and entrophy estimates.
  //  Even with the fluctuations, overall the measure initially increases
  //  with the number of iterations.
  //  After about 150 iterations the metric value oscilates
  //  without further noticeable convergence. 
  //  The trace plots in Figure \ref{fig:ImageRegistration2TraceMetric}
  //  highlights one of the difficulties with using this particular metric:
  //  the stochastic oscillations makes it difficult to determine
  //  convergence and limits the use of more sophisticated optimizations
  //  methods. As explained above,
  //  the reduction of the learning rate as the registration progresses
  //  is very important to get precise results.
  //
  //  This example highlight the importance of tracking the evolution of the
  //  registration method in order to get some insight on the characteristics
  //  of the particular problem at hand and the components being used. 
  //  The behavior revealed by these plots
  //  usually helps to identify possible improvements in the setup of the
  //  registration parameters. 
  //
  //  Software Guide : EndLatex 

  return 0;

}

