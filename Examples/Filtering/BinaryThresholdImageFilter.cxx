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


int main( int argc, char ** argv )
{

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
  //  The filter type is now instantiated.
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


  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );

  filter->SetOutsideValue( 0 );
  filter->SetInsideValue( 255 );

  filter->SetLowerThreshold(  85 );
  filter->SetUpperThreshold( 194 );

  filter->Update();

  return 0;

}

