/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SmoothingRecursiveGaussianImageFilter.cxx
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
//  The classical method of smoothing an image by convolution with a Gaussian
//  kernel has the drawback of providing low performance as the sigma of the
//  Gaussian increases. This is due to the larger size of the kernel which
//  results in a higher number of computations per pixel.
//
//  The \doxygen{RecursiveGaussianImageFilter} implements an approximation of
//  convolution with the Gaussian and its derivatives by using
//  IIR\footnote{Infinte Impulsional Response} filters. In practice this filter
//  requires a constant number of operations for approximating the convolution.
//  This is independent of the sigma value \cite{Deriche1990,Deriche1993}.
//
//  \index{itk::RecursiveGaussianImageFilter|textbf}
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
//  \index{itk::RecursiveGaussianImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char ** argv )
{


  if( argc < 4 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  sigma " << std::endl;
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
  //  \index{itk::RecursiveGaussianImageFilter!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::RecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet



  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  This filters applies the approximation of the convolution along a single
  //  dimension. It is then necessary to concatenate several of these filters
  //  in order to produce smoothing in all directions. We create then a pair of
  //  filters since we have a $2D$ image to process here. The filters are
  //  created by invoking the \code{New()} method and assigning the result to a
  //  \doxygen{SmartPointer}.
  //
  //  \index{itk::RecursiveGaussianImageFilter!New()}
  //  \index{itk::RecursiveGaussianImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filterX = FilterType::New();
  FilterType::Pointer filterY = FilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Since each one of the newly created filters has the potential to perform
  //  filtering along any dimension, we have to restric each one to a
  //  particular direction. This is done with the \code{SetDirection} method.
  //
  //  \index{RecursiveGaussianImageFilter!SetDirection()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filterX->SetDirection( 0 );   // 0 --> X direction
  filterY->SetDirection( 1 );   // 1 --> Y direction
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The \doxygen{RecursiveGaussianImageFilter} can approximate the convolution
  //  with the gaussian or with its first and second derivatives. We should
  //  hence select here one of these options by  using the \code{SetOrder()}
  //  method. Note that the argument is an \code{enum} whose values can be
  //  \code{ZeroOrder}, \code{FirstOrder} and \code{SecondOrder}. For example,
  //  for computing the derivative along $X$ we should select \code{FirstOrder}
  //  along $X$ and \code{ZeroOrder} along $Y$. Here we want to smooth in $X$
  //  and $Y$ so we select \code{ZeroOrder} in both directions.
  //
  //  \index{RecursiveGaussianImageFilter!SetOrder()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filterX->SetOrder( FilterType::ZeroOrder );
  filterY->SetOrder( FilterType::ZeroOrder );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  There are two typical ways of normalizing Gaussians depending on their
  //  application. For Scale-Space analysis it is desirable to use a
  //  normalization that will preserve the maximum value of the input. This
  //  normalization is represented by the following equation.
  //
  //  \begin{equation}
  //          \frac{ 1 }{ \sigma  \sqrt{ 2 \pi } }
  //  \end{equation}
  //
  //  In applications that use the Gaussian as a solution of the diffusion
  //  equation it is desirable to use a normalization that preserve the
  //  integral of the signal. This last approach can be seen as a conservation
  //  of mass principle. This is represented by the following equation.
  //
  //  \begin{equation}
  //          \frac{ 1 }{ \sigma^2  \sqrt{ 2 \pi } }
  //  \end{equation}
  //
  //  The \doxygen{RecursiveGaussianImageFilter} has a \code{boolean} flag that
  //  allows to select between both normalization options. This is done with
  //  the method \code{SetNormalizeAcrossScale()}. For analyzing an image
  //  across Scale-Space you want to enable this flag. In current example it
  //  doesn't have any impact since we are actually renormalizing the output to
  //  the dynamic range of the reader, so we simply put the flag off here.
  //
  //  \index{RecursiveGaussianImageFilter!SetNormalizeAcrossScale()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filterX->SetNormalizeAcrossScale( false );
  filterY->SetNormalizeAcrossScale( false );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source. The image is passed to the $X$ filter
  //  and then to the $Y$ filter. The reason for keeping these two filters
  //  separated is that it is usual in Scale-Space applications to compute not
  //  only the smoothing but also combinations of derivatives at different
  //  orders and smoothing. Some factorization is possible when separate
  //  filters are used to generate the intermediate results. Here this
  //  capabilites are less interesting though since we only want to smooth the
  //  image in all directions.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filterX->SetInput( reader->GetOutput() );
  filterY->SetInput( filterX->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  It is now time for selecting the sigma of the Gaussian to use for
  //  smoothing the data. Note that sigma has to be passed to both filters.
  //  Note that sigma is considered to be in millimeters. That is, at the
  //  moment of applying the smoothing process, the filter will take into
  //  account the spacing values defined in the image.
  //
  //  \index{itk::RecursiveGaussianImageFilter!SetSigma()}
  //  \index{SetSigma()!itk::RecursiveGaussianImageFilter}
  //
  //  Software Guide : EndLatex 

  const double sigma = atof( argv[3] );

  // Software Guide : BeginCodeSnippet
  filterX->SetSigma( sigma );
  filterY->SetSigma( sigma );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Finally the pipeline is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::RecursiveGaussianImageFilter!Update()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  filterY->Update();
  // Software Guide : EndCodeSnippet



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
 

  rescaler->SetInput( filterY->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();
  


  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=6cm]{SmoothingRecursiveGaussianImageFilterOutput3.eps}
  // \includegraphics[width=6cm]{SmoothingRecursiveGaussianImageFilterOutput5.eps}
  // \caption[RecursiveGaussianImageFilter output]{Effect of the
  // RecursiveGaussianImageFilter on a slice from a MRI Proton Density image
  // of the brain.}
  // \label{fig:RecursiveGaussianImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:RecursiveGaussianImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain using a sigma value of $3$ (left) and a value of $5$ (right).
  //  The figure shows how the attenuation of noise can be regulated by
  //  selecting an apropriate sigma.  This type of scale-tunable filter is
  //  suitable for performing Scale-Space analysis.
  //
  //  Software Guide : EndLatex 




  return 0;

}

