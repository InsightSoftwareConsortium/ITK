/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GradientMagnitudeRecursiveGaussianImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  Differentiation is an ill-defined operation over digital data. In practice
//  it results convenient to define a scale over which the differentiation
//  should be performed. This is usually done by preprocessing the data with a
//  smoothing filter. It has been shown that a Gaussian kernel is the most
//  convinient choice for performing such smoothing. By chossing a particular
//  value for the sigma in the Gaussian, an associated scale is selected. This
//  allows to neglect details with high frequency content that are commonly
//  considered image noise.
//
//  The \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} computes the
//  magnitude of the image gradient at each pixel location.  The computational
//  process is equivalent to first smoothing the image by convolving it with a
//  Gaussian kernel and then applying a differential operator.  The user
//  selects the value of sigma.
//
//  Internally this is done by applying a IIR \footnote{Infinite Impulsional
//  Response} filter that approximates a convolution with the derivative of the
//  Gaussian kernel. The advantage of this approach is that the performance is
//  largely superior, in particular when large sigmas are selected for the
//  Gaussian kernel \cite{Deriche1990,Deriche1993}.
//
//  This filter will work on images of any dimension by taking advantage of the
//  natural separability of the Gaussian kernel and its derivatives. 
//
//  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter|textbf}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char * argv[] )
{


  if( argc < 4 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputImageFile   sigma" << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be choosen for the pixels of the input and output images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  With them, the input and output image types can be instantiated.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet



  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types.
  //
  //  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet



  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!New()}
  //  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  It is now time for selecting the sigma of the Gaussian to use for
  //  smoothing the data prior to te differentiation.
  //
  //  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!SetSigma()}
  //  \index{SetSigma()!itk::GradientMagnitudeRecursiveGaussianImageFilter}
  //
  //  Software Guide : EndLatex 
  const double sigma = atof( argv[3] );


  // Software Guide : BeginCodeSnippet
  filter->SetSigma( sigma );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Finally the filter is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!Update()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  If the output of this filter has been connected to other filters down the
  //  pipeline, updating any of the downstream filters would have triggered the
  //  execution of this one. For example, a rescale filter followed by a writer
  //  could have been used after the gradient magnitude.
  //
  //  Software Guide : EndLatex 


  typedef  unsigned char  WritePixelType;
  typedef itk::Image< WritePixelType, 2 >    WriteImageType;

  typedef itk::RescaleIntensityImageFilter< 
                   OutputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
 

  // Software Guide : BeginCodeSnippet
  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet
  


  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=6cm]{GradientMagnitudeRecursiveGaussianImageFilterOutput3.eps}
  // \includegraphics[width=6cm]{GradientMagnitudeRecursiveGaussianImageFilterOutput5.eps}
  // \caption[GradientMagnitudeRecursiveGaussianImageFilter output]{Effect of
  // the GradientMagnitudeRecursiveGaussianImageFilter on a slice from a MRI
  // Proton Density image  of the brain.}
  // \label{fig:GradientMagnitudeRecursiveGaussianImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:GradientMagnitudeRecursiveGaussianImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain using a sigma value of $3$ (left) and a value of $5$ (right).
  //  The figure shows how the sensitivity to noise can be regulated by
  //  selecting an apropriate sigma.  This type of scale-tunable filter is
  //  suitable for performing scale space analysis.
  //
  //  Attention should be paid to the image type choosen for representing the
  //  input and output images since the dynamic range of gradient magnitudes is
  //  usually quite smaller than the dynamic range of the input image
  //  intensities. 
  //
  //  Software Guide : EndLatex 




  return 0;

}

