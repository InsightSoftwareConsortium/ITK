/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ThresholdImageFilter.cxx
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
// \begin{figure}
// \center
// \includegraphics[height=5cm]{ThresholdTransferFunctionBelow.eps}
// \includegraphics[height=5cm]{ThresholdImageFilterOutputBelow.eps}
// \caption{Effect of using the threshold-below mode.}
// \label{fig:ThresholdTransferFunctionBelow}
// \end{figure}
//
// \begin{figure}
// \center
// \includegraphics[height=5cm]{ThresholdTransferFunctionAbove.eps}
// \includegraphics[height=5cm]{ThresholdImageFilterOutputAbove.eps}
// \caption{Effect of using the threshold-above mode.}
// \label{fig:ThresholdTransferFunctionAbove}
// \end{figure}
//
// \begin{figure}
// \center
// \includegraphics[height=5cm]{ThresholdTransferFunctionOutside.eps}
// \includegraphics[height=5cm]{ThresholdImageFilterOutputOutside.eps}
// \caption{Effect of using the threshold-outside mode.}
// \label{fig:ThresholdTransferFunctionOutside}
// \end{figure}
//
// This example illustrates the use of the \code{ThresholdImageFilter}.  This
// filter can be used to transform the intensity levels of an image in three
// different ways. 
//
// \begin{itemize}
//
// \item First, the user can define a particular threshold , all the pixels
// with values below this threshold will be replaced by a user defined value
// called here the \code{OutsideValue}.  Pixels with values above the threshold
// remain unchanged. This is illustrated in figure
// \ref{fig:ThresholdTransferFunctionBelow}.
//
// \item Second, the user can define a particular threshold , all the pixels
// with values above this threshold will be replaced by the
// \code{OutsideValue}.  Pixels with values below the threshold remain
// unchanged. This is illustrated in figure
// \ref{fig:ThresholdTransferFunctionAbove}.
//
// \item Third, the user can provide two thresholds. All the pixels with
// intensity values inside the range defined by the two thresholds will remain
// unchanged. Pixels with values outside this range will be assigned to the
// \code{OutsideValue}. This is illustrated in figure
// \ref{fig:ThresholdTransferFunctionOutside}.
//
// \end{itemize}
//
// The mode of operation of the filter is the defined by calling one of the
// following methods
//
// \begin{itemize}
// \item \texttt{ThresholdBelow()}
// \item \texttt{ThresholdAbove()}
// \item \texttt{ThresholdOutside()}
// \end{itemize}
//
// \index{itk::ThresholdImageFilter!Instantiation|textbf}
// \index{itk::ThresholdImageFilter!Header|textbf}
// \index{itk::ThresholdImageFilter!ThresholdAbove()}
// \index{itk::ThresholdImageFilter!ThresholdBelow()}
// \index{itk::ThresholdImageFilter!ThresholdOutside()}
//
// The first step required to use this filter is to include its header file. 
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkThresholdImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPNGImageIO.h"

int main( int argc, char ** argv )
{

  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0] << " inputImageFile ";
    std::cerr << " outputImageFile1 outputImageFile2 outputImageFile3" << std::endl;  
    return 1;
    }
  
  //  Software Guide : BeginLatex
  //
  //  Then we must decide what pixel type to use for the image. This filter is
  //  templated over a single image type since the output image must be able to
  //  represent the unchanged pixels.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char  PixelType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The image type is now defined using the pixel type and the dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< PixelType,  3 >   ImageType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The filter type can be instantiated using the image type defined above.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ThresholdImageFilter< ImageType >  FilterType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  An \code{ImageFileReader} class is also instantiated in order to read
  //  image data from a file. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  // Software Guide : EndCodeSnippet


  // An ImageFileWriter is instantiated in order to write the output image to a
  // file.
  typedef itk::ImageFileWriter< ImageType >  WriterType;



  //  Software Guide : BeginLatex
  //
  //  Both the filter and the reader are created by invoking their \code{New()}
  //  methods and assigning the result to SmartPointers.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet

  WriterType::Pointer writer = WriterType::New();

  itk::PNGImageIO::Pointer pngIO  = itk::PNGImageIO::New();

  writer->SetImageIO( pngIO );
  writer->SetInput( filter->GetOutput() );

  reader->SetFileName( argv[1] );



  //  Software Guide : BeginLatex
  //  
  //  The image obtained with the reader is passed as input to the
  //  \code{ThresholdImageFilter}.
  //
  //  \index{itk::ThresholdImageFilter!SetInput()}
  //  \index{itk::FileImageReader!GetOutput()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The method \code{SetOutsideValue()} defines the intensity value to be
  //  assigned to those pixels whose intensities are outside the range defined
  //  by the lower and upper thresholds. 
  //  
  //  \index{itk::ThresholdImageFilter!SetOutsideValue()}
  //  \index{SetOutsideValue()!itk::ThresholdImageFilter}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetOutsideValue( 0 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The method \code{ThresholdBelow()} define the intensity value below which
  //  pixels  of the input image will be transformed into the \code{OutsideValue}.
  //  
  //  \index{itk::ThresholdImageFilter!ThresholdBelow()|textbf}
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->ThresholdBelow( 180 );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The execution of the filter is triggered by invoking the \code{Update()}
  //  method. If the filter's output has been passed as input to subsequent
  //  filters, the \code{Update()} call on any of the posterior filters in the
  //  pipeline will indirectly trigger the update of this filter too.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet



  writer->SetFileName( argv[2] );
  writer->Update();


  //  Software Guide : BeginLatex
  //  
  //  The output of this process is shown on figure
  //  \ref{fig:ThresholdTransferFunctionBelow}.  The second mode of operation
  //  of the filter is enabled by invoking the method \code{ThresholdAbove()}
  //  and then triggering the update of the filter.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->ThresholdAbove( 180 );
  filter->Update();
  // Software Guide : EndCodeSnippet


  writer->SetFileName( argv[3] );
  writer->Update();

  //  Software Guide : BeginLatex
  //  
  //  The output of this process is shown on figure
  //  \ref{fig:ThresholdTransferFunctionAbove}.  The third mode of operation
  //  of the filter is enabled by invoking the method \code{ThresholdOutside()}
  //  and then triggering the update of the filter.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->ThresholdOutside( 170,190 );
  filter->Update();
  // Software Guide : EndCodeSnippet


  writer->SetFileName( argv[4] );
  writer->Update();


  //  Software Guide : BeginLatex
  //  
  //  The output of this process is shown on figure
  //  \ref{fig:ThresholdTransferFunctionAbove}.  The examples in this section
  //  also illustrate the limitations of this filter for performing
  //  segmentation by itself. These limitations are particularly noticeable in
  //  noisy images and in images lacking spatial uniformity as is the case of
  //  MRI due to field bias.
  //
  //  Software Guide : EndLatex 


  

  return 0;

}

