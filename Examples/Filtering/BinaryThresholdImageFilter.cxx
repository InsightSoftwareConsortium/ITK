/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    BinaryThresholdImageFilter.cxx
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
// \piccaption[2]{Transfer function of the BinaryThreshold image filter.
//                \label{fig:BinaryThresholdTransferFunction}}
//  \parpic(8cm,5cm)[r]{\includegraphics[width=7cm]{BinaryThresholdTransferFunction.eps}}
//
// This example illustrates the use of the \code{BinaryThresholdImageFilter}.
// This filter is used to transform an image into a binary image by changing
// the pixel values according to the rule illustrated in figure
// \ref{fig:BinaryThresholdTransferFunction}. The user defines two thresholds
// ---Upper and Lower--- and two intensity values --- Inside and Outside. For
// each pixel in the input image, the value of the pixel is compared with the
// lower and upper thresholds. If the pixel value is inside the range defined
// by $[Lower,Upper]$ the output pixel is assigned the InsideValue. Otherwise
// the output pixels is assigned to the OutsideValue. Thresholding is commonly
// applied as the last operation of a segmentation pipeline.
//
// \index{itk::BinaryThresholdImageFilter!Instantiation|textbf}
// \index{itk::BinaryThresholdImageFilter!Header|textbf}
//
// The first step required to use this filter is to include its header file. 
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkBinaryThresholdImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char ** argv )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile" << std::endl;  
    return 1;
    }
  
  //  Software Guide : BeginLatex
  //
  //  Then we must decide what pixel types to use for the input and output
  //  images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char  InputPixelType;
  typedef  unsigned char  OutputPixelType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The input and output image types are now defined using their respective
  //  pixel type and dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  3 >   InputImageType;
  typedef itk::Image< OutputPixelType, 3 >   OutputImageType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The filter type can be instantiated using the input and output image
  //  types defined above.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryThresholdImageFilter<
               InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  An \code{ImageFileReader} class is also instantiated in order to read
  //  image data from a file. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  // Software Guide : EndCodeSnippet


  // An ImageFileWriter is instantiated in order to write the output image to a
  // file.
  typedef itk::ImageFileWriter< InputImageType >  WriterType;



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

  writer->SetInput( filter->GetOutput() );

  reader->SetFileName( argv[1] );



  //  Software Guide : BeginLatex
  //  
  //  The image obtained with the reader is passed as input to the
  //  BinaryThresholdImageFilter.
  //
  //  \index{itk::BinaryThresholdImageFilter!SetInput()}
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
  //  by the lower and upper thresholds. The method \code{SetInsideValue()}
  //  define the intensity value to be assigned to pixels with intensities
  //  falling inside the threshold range.
  //  
  //  \index{itk::BinaryThresholdImageFilter!SetOutsideValue()}
  //  \index{itk::BinaryThresholdImageFilter!SetInsideValue()}
  //  \index{SetOutsideValue()!itk::BinaryThresholdImageFilter}
  //  \index{SetInsideValue()!itk::BinaryThresholdImageFilter}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetOutsideValue( 0 );
  filter->SetInsideValue( 255 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The methods \code{SetLowerThreshold()} and \code{SetUpperThreshold()}
  //  define the range of the input image intensities that will be transformed
  //  into the \code{InsideValue}. Note that the lower and upper thresholds are
  //  values of the type of the input image pixels, while the inside and
  //  outside values are of the type of the output image pixels.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetLowerThreshold( 150 );
  filter->SetUpperThreshold( 180 );
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

  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=6cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=6cm]{BinaryThresholdImageFilterOutput.eps}
  // \caption{Effect of the BinaryThresholdImageFilter on a slice from a MRI
  // Proton Density image  of the brain.}
  // \label{fig:BinaryThresholdImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:BinaryThresholdImageFilterInputOutput} illustrate the
  //  effect of this filter on a MRI proton density image of the brain. This
  //  figure shows the limitations of this filter for performing segmentation
  //  by itself. These limitations are particularly noticeable in noisy images
  //  and in images lacking spatial uniformity as is the case of MRI due to
  //  field bias.
  //
  //  Software Guide : EndLatex 

  writer->SetFileName( argv[2] );

  writer->Update();


  return 0;

}

