/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration2.cxx
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
// Some of the most challenging cases of image registration arise when images
// of different modalities are involved. In such cases, metrics based on
// direct comparison of gray levels are not applicable. It has been
// extensively shown that metrics based on the evaluation of mutual
// information is the best way to overcome the difficulties of multi-modality
// registration.
//
// \index{itk::Image\-Registration\-Method!Multi-Modality}
//
// The following simple example illustrates how multiple imaging modalities
// can be registered using the ITK registration framework. The first
// difference is the use of the \doxygen{MutualInformationImageToImageMetric} as
// the cost-function to be optimized and the second difference is the use of
// the \doxygen{GradientDescentOptimizer}. Due to the stochastic nature of the
// metric computation, the values are too noisy to work
// successfully with the \doxygen{RegularStepGradientDescentOptimizer}.
// Therefore, we will use the simpler GradientDescentOptimizer with
// a user defined learning rate.  The following headers declare the basic
// components of this registration method.
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
//  One way to simplify the computation of the mutual information is
//  to normalize the statistical distribution of the two input images. The
//  \doxygen{NormalizeImageFilter} is the perfect tool for this task.
//  It rescales the intensities of the input images in order to produce an
//  output image with zero mean and unit variance. This filter has been
//  discussed in Section \ref{sec:CastingImageFilters}.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkNormalizeImageFilter.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//  
//  Additionally, low-pass filtering of the images to be registered will also
//  increase robustness against noise. In this example, we will use the
//  \doxygen{DiscreteGaussianImageFilter} for the purpose. The
//  characteristics of this filter have been discussed in Section
//  \ref{sec:BlurringFilters}.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkDiscreteGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"


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
  // The moving and fixed images should be instantiated first. 
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
  //  It is convenient to work with an internal image type because mutual
  //  information will perform better on images with a normalized statistical
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
  //  illustrated in Section \ref{sec:IntroductionImageRegistration} with
  //  the use of the \code{InternalImageType}.
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
  //  The metric requires a number of parameters to be selected, including
  //  the standard deviation of the Gaussian kernel for the fixed image
  //  density estimate, the standard deviation of the kernel for the moving
  //  image density and the number of samples use to compute the densities
  //  and entropy values. Details on the concepts behind the computation of
  //  the metric can be found in Section
  //  \ref{sec:MutualInformationMetric}.  Experience has
  //  shown that a kernel standard deviation of $0.4$ works well for images
  //  which have been normalized to a mean of zero and unit variance.  We
  //  will follow this empirical rule in this example.
  //
  //  \index{itk::Mutual\-Information\-Image\-To\-Image\-Metric!SetFixedImageStandardDeviation()}
  //  \index{itk::Mutual\-Information\-Image\-To\-Image\-Metric!SetMovingImageStandardDeviation()}
  //  \index{itk::Mutual\-Information\-Image\-To\-Image\-Metric!SetNumberOfSpatialSamples()}
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
  //  The normalization filters are instantiated using the fixed and moving
  //  image types as input and the internal image type as output.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::NormalizeImageFilter< 
                                FixedImageType, 
                                InternalImageType 
                                        > FixedNormalizeFilterType;

  typedef itk::NormalizeImageFilter< 
                                MovingImageType, 
                                InternalImageType 
                                              > MovingNormalizeFilterType;

  FixedNormalizeFilterType::Pointer fixedNormalizer = 
                                            FixedNormalizeFilterType::New();

  MovingNormalizeFilterType::Pointer movingNormalizer =
                                            MovingNormalizeFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  The blurring filters are declared using the internal image type as both
  //  the input and output types. In this example, we will set the variance
  //  for both blurring filters to 2.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::DiscreteGaussianImageFilter<
                                      InternalImageType, 
                                      InternalImageType
                                                    > GaussianFilterType;
  
  GaussianFilterType::Pointer fixedSmoother  = GaussianFilterType::New();
  GaussianFilterType::Pointer movingSmoother = GaussianFilterType::New();

  fixedSmoother->SetVariance( 2.0 );
  movingSmoother->SetVariance( 2.0 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  The output of the readers becomes the input to the normalization
  //  filters. The outputs of the normalization filters is connected as
  //  input to the blurring filters. The input to the registration method
  //  is taken from the blurring filters.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fixedNormalizer->SetInput(  fixedImageReader->GetOutput() );
  movingNormalizer->SetInput( movingImageReader->GetOutput() );

  fixedSmoother->SetInput( fixedNormalizer->GetOutput() );
  movingSmoother->SetInput( movingNormalizer->GetOutput() );

  registration->SetFixedImage(    fixedSmoother->GetOutput()    );
  registration->SetMovingImage(   movingSmoother->GetOutput()   );
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
  //  Since larger values of mutual information indicate better matches than
  //  smaller values, we need to maximize the cost function in this example.
  //  By default the GradientDescentOptimizer class is set to minimize the
  //  value of the cost-function. It is therefore necessary to modify its
  //  default behavior by invoking the \code{MaximizeOn()} method.
  //  Additionally, we need to define the optimizer's step size using the
  //  \code{SetLearningRate()} method.
  //
  //  \index{itk::Gradient\-Descent\-Optimizer!MaximizeOn()}
  //  \index{itk::Image\-Registration\-Method!Maximize vs Minimize}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  optimizer->SetLearningRate( 20.0 );
  optimizer->SetNumberOfIterations( 200 );
  optimizer->MaximizeOn();
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
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    } 

  ParametersType finalParameters = registration->GetLastTransformParameters();
  
  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];
  
  unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  
  double bestValue = optimizer->GetValue();


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
  //  \item \code{BrainT1SliceBorder20.png} 
  //  \item \code{BrainProtonDensitySliceShifted13x17y.png}
  //  \end{itemize}
  //
  //  \begin{figure}
  //  \center
  //  \includegraphics[width=0.44\textwidth]{BrainT1SliceBorder20.eps}
  //  \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceShifted13x17y.eps}
  //  \itkcaption[Multi-Modality Registration Inputs]{A T1 MRI (fixed image) and a proton
  //  density MRI (moving image) are provided as input to the registration method.}
  //  \label{fig:FixedMovingImageRegistration2}
  //  \end{figure}
  // 
  //  The second image is the result of intentionally translating the image
  //  \code{BrainProtonDensitySliceBorder20.png} by $(13,17)$ millimeters. Both
  //  images have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration2}. The registration is
  //  stopped at 200 iterations and produces as result the
  //  parameters:
  //
  //  \begin{verbatim}
  //  Translation X = 12.8804
  //  Translation Y = 16.7718
  //  \end{verbatim}
  //  These values are approximately within half a pixel of 
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
  // \includegraphics[width=0.32\textwidth]{ImageRegistration2Output.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration2CheckerboardBefore.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration2CheckerboardAfter.eps}
  // \itkcaption[Multi-Modality Registration outputs]{Mapped moving image (left)
  // and composition of fixed and moving images before (center) and after
  // (right) registration.}
  // \label{fig:ImageRegistration2Output}
  // \end{figure}
  //
  //  The moving image after resampling is presented on the left
  //  side of Figure \ref{fig:ImageRegistration2Output}. The center and right
  //  figures present a checkerboard composite of the fixed and
  //  moving images before and after registration.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{ImageRegistration2TraceTranslations.eps}
  // \includegraphics[width=0.44\textwidth]{ImageRegistration2TraceTranslations2.eps}
  // \itkcaption[Multi-Modality Registration plot of translations]{Sequence of
  // translations during the registration process. On the left are iterations 0 to
  // 200. On the right are iterations 150 to 200.}
  // \label{fig:ImageRegistration2TraceTranslations}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration2TraceTranslations} shows the sequence
  //  of translations followed by the optimizer as it searched the parameter
  //  space. The left plot shows iterations $0$ to $200$ while the right
  //  figure zooms into iterations $150$ to $200$. The area covered by the
  //  right figure has been highlighted by a rectangle in the left image.  It
  //  can be seen that after a certain number of iterations the optimizer
  //  oscillates within one or two pixels of the true solution.  At this
  //  point it is clear that more iterations will not help. Instead it is
  //  time to modify some of the parameters of the registration process, for
  //  example, reducing the learning rate of the optimizer and continuing the
  //  registration so that smaller steps are taken.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{ImageRegistration2TraceMetric.eps}
  // \includegraphics[width=0.44\textwidth]{ImageRegistration2TraceMetric2.eps}
  // \itkcaption[Multi-Modality Registration plot of metrics]{The sequence of metric
  // values produced during the registration process. On the left are iterations 0 to 200.
  // On the right are iterations 100 to 200.}
  // \label{fig:ImageRegistration2TraceMetric}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration2TraceMetric} shows the sequence of
  //  metric values computed as the optimizer searched the parameter space.
  //  The left plot shows values when iterations are extended from $0$ to
  //  $200$ while the right figure zooms into iterations $100$ to $200$.  The
  //  fluctuations in the metric value are due to the stochastic nature in
  //  which the measure is computed. At each call of \code{GetValue()}, two
  //  new sets of intensity samples are randomly taken from the image to
  //  compute the density and entropy estimates.  Even with the fluctuations,
  //  the measure initially increases overall with the number of iterations.
  //  After about 150 iterations, the metric value merely oscillates without further
  //  noticeable convergence.  The trace plots in Figure
  //  \ref{fig:ImageRegistration2TraceMetric} highlight one of the
  //  difficulties associated with this particular metric: the stochastic
  //  oscillations make it difficult to determine convergence and limit the
  //  use of more sophisticated optimization methods. As explained above,
  //  the reduction of the learning rate as the registration progresses is
  //  very important in order to get precise results.
  //
  //  This example shows the importance of tracking the evolution of the
  //  registration method in order to obtain insight into the characteristics
  //  of the particular problem at hand and the components being used.  The
  //  behavior revealed by these plots usually helps to identify possible
  //  improvements in the setup of the registration parameters.
  //
  //  Software Guide : EndLatex 

  return 0;
}

