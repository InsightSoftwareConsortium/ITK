/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DiscreteGaussianImageFilter.cxx
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
//  \piccaption[DiscreteGaussianImageFilter gaussian diagram]{Discretized
//  Gaussian.\label{fig:DiscretizedGaussian}}
//  \parpic(7cm,4cm)[r]{\includegraphics[width=6cm]{DiscreteGaussian.eps}}
//
//  The \doxygen{DiscreteGaussianImageFilter} computes the convolution
//  of the input image with a Gaussian kernel by taking advantage of
//  its separability.  A one-dimensional Gaussian function is
//  discretized on a convolution kernel.  The size of the kernel is
//  extended until there are enough discrete points in the Gaussian to
//  ensure that a user-provided maximum error is not exceeded.  Since
//  the size of the kernel is unkown a priori it is necesary to impose
//  a limit to its growth. The user can thus provide a value to be the
//  maximum admissible size of the kernel. The discretization errror
//  is defined as the difference between the area under the discrete
//  Gaussian curve (which has finite support) and the area under the
//  continuous Gaussian.
//
//  \index{itk::DiscreteGaussianImageFilter|textbf}
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
//  \index{itk::DiscreteGaussianImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkDiscreteGaussianImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char * argv[] )
{


  if( argc < 5 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  variance  maxKernelWidth " << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be choosen for the pixels of the input and output images.
  //  Image types can be instantiated using the pixel type and dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet




  typedef itk::ImageFileReader< InputImageType >  ReaderType;




  //  Software Guide : BeginLatex
  //
  //  The discrete Gaussian filter type is now instantiated using both the
  //  input image and the output image types. Then a filter object is created.
  //
  //  \index{itk::DiscreteGaussianImageFilter!instantiation}
  //  \index{itk::DiscreteGaussianImageFilter!New()}
  //  \index{itk::DiscreteGaussianImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::DiscreteGaussianImageFilter<
                 InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet



  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );




  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  const double gaussianVariance = atof( argv[3] );

  const unsigned int maxKernelWidth = atoi( argv[4] );


  //  Software Guide : BeginLatex
  //  
  //  The filters requires the user to provide a value for the variance
  //  associated with the Gaussian kernel. The method \code{SetVariance()} is
  //  used for this purpose. The Gaussian is dicretized over the pixels of a
  //  convolution kernel. The maximum value of the kernel size can be set by
  //  the user. Note that the combination of variance and kernel-size values
  //  may result in truncating the borders of the discretized Gaussian kernel.
  //
  //  \index{itk::DiscreteGaussianImageFilter!SetVariance()}
  //  \index{itk::DiscreteGaussianImageFilter!SetMaximumKernelWidth()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetVariance( gaussianVariance );

  filter->SetMaximumKernelWidth( maxKernelWidth );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Finally the filter is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::DiscreteGaussianImageFilter!Update()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  If the output of this filter has been connected to other filters down the
  //  pipeline, updating any of the downstream filters would have triggered the
  //  execution of this one. For example, a writer could have been used after
  //  the filter.
  //
  //  Software Guide : EndLatex 

  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

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
  // \includegraphics[width=6cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=6cm]{DiscreteGaussianImageFilterOutput.eps}
  // \caption[DiscreteGaussianImageFilter output]{Effect of the
  // DiscreteGaussianImageFilter on a slice from a MRI Proton Density image  of
  // the brain.}
  // \label{fig:DiscreteGaussianImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:DiscreteGaussianImageFilterInputOutput} illustrates the
  //  effect of this filter on a MRI proton density image of the brain. 
  //  
  //  Since this filter actually computes the convolution with the Gaussian
  //  kernel, it is not desirable to use large values of the variance since the
  //  appropriate approximation will require large kernels and incurr in
  //  prohibitive computational times.
  //
  //  Software Guide : EndLatex 




  return 0;

}

