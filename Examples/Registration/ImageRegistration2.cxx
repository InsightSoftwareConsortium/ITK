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
// One of the most challenging cases of image registration is when images of
// different modalities are involved. In such cases, metrics based on direct
// comparison of gray levels are not applicable. It has been extensively shown
// that metrics based on the evaluation of \code{Mutual Information} provide
// the best mechanisms to overcome the difficulties of multi-modality
// registration.
//
// \index{itk::ImageRegistrationMethod!Multi-Modality|textbf}
//
// The following example illustrates in a minimal program how multiple imaging
// modalities can be registered using Insight components. The first remarkable
// difference is the use of \code{MutualInformationImageToImageMetric} as the
// cost-function to be optimized. The following headers declare the basic
// components of the registration method.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//  
//  One way of simplifying the computation of the Mutual Information Metric is
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
  //  illustrated in section \ref{sec:ImageRegistration1} but considering the
  //  new \code{InternalImageType}.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction< 
                                    InternalImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod< 
                                    InternalImageType, 
                                    InternalImageType >  RegistrationType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The type of the Mutual Information metric is instantiated using the image
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
  //  The metric is created using the \code{New()} method and its smart pointer
  //  is assigned to the registration method object.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  MetricType::Pointer         metric        = MetricType::New();
  
  registration->SetMetric( metric  );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The metric requires a number of parameters to be selected. Among them,
  //  the standard deviation of the fixed and moving image and the number of
  //  samples to be taken in order to estimate the joint probabilities. Details
  //  on the concept behind the computation of the metric can be found in
  //  section \ref{sec:MutualInformationMetric}. In our current case, since the
  //  images have already been passed through the \code{NormalizeImageFilter}
  //  we are confident that their standard deviations are $1.0$.
  //
  //  \index{itk::MutualInformationImageToImageMetric!SetFixedImageStandardDeviation()}
  //  \index{itk::MutualInformationImageToImageMetric!SetMovingImageStandardDeviation()}
  //  \index{itk::MutualInformationImageToImageMetric!SetNumberOfSpatialSamples()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  metric->SetFixedImageStandardDeviation(  1.0 );
  metric->SetMovingImageStandardDeviation( 1.0 );

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
  //  The optimal value of Mutual Information is $1.0$. Sub-optimal values
  //  will be in the interval $[0,1)$. In this regard the optimization methods
  //  becomes a maximization problem. By default the
  //  \code{RegularStepGradientDescentOptimizer} is set to minimize the value
  //  of the cost-function. It is henceforth necessary to modify its default
  //  behavior by invoking the \code{MaximizeOn()} method.
  //
  //  \index{itk::RegularStepGradientDescentOptimizer!MaximizeOn()}
  //  \index{itk::ImageRegistrationMethod!Maximize vs Minimize}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( 4.00 );  
  optimizer->SetMinimumStepLength( 0.01 );
  optimizer->SetNumberOfIterations( 200 );

  optimizer->MaximizeOn();
  // Software Guide : EndCodeSnippet


  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cout << "Caught expected ExceptionObject" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    } 

  ParametersType finalParameters = registration->GetLastTransformParameters();
  
  const double TranslationAlongX = finalParameters[0];
  const double TranslationAlongY = finalParameters[1];
  
  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  
  const double bestValue = optimizer->GetValue();


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
  //  \label{fig:FixedMovingImageRegistration1}
  //  \end{figure}
  // 
  //  The second image is the result of intentionally translating the image
  //  \code{BrainProtonDensitySliceBorder20.png} by $(13,17)$ millimeters. Both
  //  images having unit-spacing. These images are shown in Figure
  //  \ref{fig:FixedMovingImageRegistration2}. The registration takes ??
  //  iterations and produce as result the parameters:
  //
  //  \begin{verbatim}
  //  Translation X = ??
  //  Translation Y = ??
  //  \end{verbatim}
  // 
  //  As expected, these values match pretty well the initial miss-registration
  //  intentionally introduced in the moving image.
  //
  //
  //  Software Guide : EndLatex 



  //  Software Guide : BeginLatex
  //  
  //  Finally we can use the resulting transform to resample the moving image
  //  and map it onto the fixed image space. This is done with the help of the
  //  \code{ResampleImageFilter}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );
  // Software Guide : EndCodeSnippet
  



  //  Software Guide : BeginLatex
  //  
  //  The geometric parameters of the resampling filters are set from the fixed
  //  image. A distictly visible gray level is selected to fill the spaces
  //  non-covered by the moving image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetDefaultPixelValue( 100 );
  // Software Guide : EndCodeSnippet



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
  //  The result of the resampling the moving image is presented in Figure
  //  \ref{fig:ImageRegistration2Output.eps}
  //
  //  Software Guide : EndLatex 





  return 0;

}

