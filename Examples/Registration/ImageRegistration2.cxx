/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration2.cxx
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
// Some of the most challenging cases of image registration is when images of
// different modalities are involved. In such cases, metrics based on direct
// comparison of gray levels are not applicable. It has been extensively shown
// that metrics based on the evaluation of mutual information provide
// the best mechanisms to overcome the difficulties of multi-modality
// registration.
//
// \index{itk::ImageRegistrationMethod!Multi-Modality|textbf}
//
// The following example illustrates in a minimal program how multiple imaging
// modalities can be registered using Insight components. The first
// difference is the use of \code{MutualInformationImageToImageMetric} as the
// cost-function to be optimized and the second difference is the use
// of \code{GradientDescentOptimizer}. Due to the stochastic nature of
// the way the metric measure is computed, the values are too noisy to
// work successfully with \code{RegularStepGradientDescentOptimizer}.
// Therefore we will use the simpler \code{GradientDescentOptimizer} with
// a user defined learning rate. The following headers declare the basic
// components of the registration method.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkGradientDescentOptimizer.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//  
//  One way of simplifying the computation of the mutual information is
//  to normalize the statistical distribution of the two input images. The
//  filter \code{itk::NormalizeImageFilter} is the perfect tool for this task.
//  It rescales the intensities of the input images in order to produce an
//  output image with zero mean and unit variance. This filter has been
//  discussed on section \ref{sec:CastingImageFilters}.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkNormalizeImageFilter.h"
// Software Guide : EndCodeSnippet




#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"



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
  typedef  itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:
  typedef   itk::GradientDescentOptimizer     OptimizerType;
  typedef   const OptimizerType   *           OptimizerPointer;

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
  
  // Software Guide : BeginLatex
  // 
  // The types of images should be declared first. 
  //
  // Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet 
  const    unsigned int    Dimension = 2;
  typedef  unsigned short  PixelType;
  
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  It is convenient to declare an internal image type given that mutual
  //  information will perform better on images with normalized statistical
  //  distribution. The fixed and moving images will be normalized and
  //  converted to this internal type.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef   float     InternalPixelType;

  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The rest of the image registration components are instantiated as
  //  illustrated in section \ref{sec:IntroductionImageRegistration} but
  //  considering the new \code{InternalImageType}.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::GradientDescentOptimizer                  OptimizerType;
  typedef itk::LinearInterpolateImageFunction< 
                                    InternalImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod< 
                                    InternalImageType, 
                                    InternalImageType >  RegistrationType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The mutual information metric type is instantiated using the image
  //  types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::MutualInformationImageToImageMetric< 
                                          InternalImageType, 
                                          InternalImageType >    MetricType;
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
  //  The metric requires a number of parameters to be selected. Among them,
  //  the standard deviation of the Gaussian kernel for the fixed image
  //  density estimate, the standard deviation of the kernel for the moving
  //  image density and the number of samples use to compute the
  //  densities and entropy values. Details on the concept behind 
  //  the computation of the metric can be found in section 
  //  \ref{sec:MutualInformationMetric}. Our experience with the toolkit
  //  has found that a kernel standard deviation of 0.4 works well for images 
  //  which has normalized to mean of zero and unit variance. 
  //  We will follow this empricial rule in this example.
  //
  //  \index{itk::MutualInformationImageToImageMetric!SetFixedImageStandardDeviation()}
  //  \index{itk::MutualInformationImageToImageMetric!SetMovingImageStandardDeviation()}
  //  \index{itk::MutualInformationImageToImageMetric!SetNumberOfSpatialSamples()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  metric->SetFixedImageStandardDeviation(  0.4 );
  metric->SetMovingImageStandardDeviation( 0.4 );

  metric->SetNumberOfSpatialSamples( 50 );
  // Software Guide : EndCodeSnippet




  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );




  //  Software Guide : BeginLatex
  //  
  //  The normalization filters are declared using the fixed and moving image
  //  types as input and the internal image type as output.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::NormalizeImageFilter< 
                        FixedImageType, InternalImageType > FixedNormalizeFilterType;
  
  typedef itk::NormalizeImageFilter< 
                        MovingImageType, InternalImageType > MovingNormalizeFilterType;

  FixedNormalizeFilterType::Pointer fixedNormalizer = FixedNormalizeFilterType::New();

  MovingNormalizeFilterType::Pointer movingNormalizer = MovingNormalizeFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The output of the readers is connected as input to the normalization
  //  filters. The inputs to the registration method are taken from the
  //  normalization filters. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fixedNormalizer->SetInput(  fixedImageReader->GetOutput() );
  movingNormalizer->SetInput( movingImageReader->GetOutput() );

  registration->SetFixedImage(    fixedNormalizer->GetOutput()    );
  registration->SetMovingImage(   movingNormalizer->GetOutput()   );
  // Software Guide : EndCodeSnippet


  fixedNormalizer->Update();

  registration->SetFixedImageRegion( 
       fixedNormalizer->GetOutput()->GetBufferedRegion() );
  


  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y
  
  registration->SetInitialTransformParameters( initialParameters );


  //  Software Guide : BeginLatex
  //  
  //  Since larger values of mutual information indicates better matches
  //  than smaller values, we need to maximize the cost function in this 
  //  example.
  //  By default the \code{GradientDescentOptimizer} is set to minimize the value
  //  of the cost-function. It is henceforth necessary to modify its default
  //  behavior by invoking the \code{MaximizeOn()} method.
  //  Additionally, we need to define the optimizer's step size using
  //  the \code{SetLearningRate()} method.
  //
  //  \index{itk::GradientDescentOptimizer!MaximizeOn()}
  //  \index{itk::ImageRegistrationMethod!Maximize vs Minimize}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  optimizer->SetLearningRate( 20.0 );
  optimizer->SetNumberOfIterations( 200 );

  optimizer->MaximizeOn();
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
  //  Let's execute this example over some of the images provided in
  //  \code{Insight/Examples/Data}, for example:
  //  
  //  \begin{itemize}
  //  \item \code{BrainT1SliceBorder20.png} 
  //  \item \code{BrainProtonDensitySliceShifted13x17y.png}
  //  \end{itemize}
  //
  //  \begin{figure}
  //  \center
  //  \includegraphics[width=6cm]{BrainT1SliceBorder20.eps}
  //  \includegraphics[width=6cm]{BrainProtonDensitySliceShifted13x17y.eps}
  //  \caption{T1 MRI (fixed image) and Proton Density MRI (moving image)
  //  provided as input to the registration method.}
  //  \label{fig:FixedMovingImageRegistration2}
  //  \end{figure}
  // 
  //  The second image is the result of intentionally translating the image
  //  \code{BrainProtonDensitySliceBorder20.png} by $(13,17)$ millimeters. Both
  //  images have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration2}. The registration has been
  //  stopped at 200 iterations and produce as result the
  //  parameters:
  //
  //  \begin{verbatim}
  //  Translation X = 13.55
  //  Translation Y = 17.3635
  //  \end{verbatim}
  // 
  //  These values are approximatedly within half pixel of 
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
  // \includegraphics[width=5cm]{ImageRegistration2Output.eps}
  // \includegraphics[width=5cm]{ImageRegistration2CheckerboardBefore.eps}
  // \includegraphics[width=5cm]{ImageRegistration2CheckerboardAfter.eps}
  // \caption{Mapped moving image (left) and composition of fixed and moving
  // images before (center) and after (right) registration.}
  // \label{fig:ImageRegistration2Output}
  // \end{figure}
  //
  //  The result of the resampling the moving image is presented in the left
  //  side of Figure \ref{fig:ImageRegistration2Output}. The center and right
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
  //  oscillates within one or two pixels of the true solution. 
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
  // iterations form 0 to 200. Right, iterations from 100 to 200.}
  // \label{fig:ImageRegistration2TraceMetric}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration2TraceMetric} shows the sequence of
  //  metric values computed as the optimizer searched the parameter space.
  //  The left plot shows values when iterations are extended from $0$ to $200$
  //  while the right figure zooms into iterations $100$ to $200$. 
  //  The fluctuations in the measure value is due to the stochastic
  //  nature in which the measure is computed. At each call of 
  //  \code{GetValue()}, two new sets of intensity samples is randomly 
  //  taken from the image to compute the density and entropy estimates.
  //  Even with the fluctuations, overall the measure initially increases
  //  with the number of iterations.
  //  After about 150 iterations the metric value oscillates
  //  without further noticeable convergence. 
  //  The trace plots in Figure \ref{fig:ImageRegistration2TraceMetric}
  //  highlights one of the difficulties with using this particular metric:
  //  the stochastic oscillations makes it difficult to determine
  //  convergence and limits the use of more sophisticated optimizations
  //  methods. As explained above,
  //  the reduction of the learning rate as the registration progresses
  //  is very important in order to get precise results.
  //
  //  This example shows the importance of tracking the evolution of the
  //  registration method in order to get some insight on the characteristics
  //  of the particular problem at hand and the components being used. 
  //  The behavior revealed by these plots
  //  usually helps to identify possible improvements in the setup of the
  //  registration parameters. 
  //
  //  Software Guide : EndLatex 

  return 0;

}

