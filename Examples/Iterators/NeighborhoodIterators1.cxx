/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    NeighborhoodIterators1.cxx
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
// This example uses the \doxygen{itk::NeighborhoodIterator} to implement a simple
// Sobel edge detection algorithm \cite{Gonzalez1993}.  We will read an input
// image, create an output buffer of matching size, then iterate through both
// images simultaneously, calculating derivatives on the input and writing the
// results to the output.
//
// In general, neighborhood-based image processing cannot operate in-place on a
// single image buffer and must use the read-only input strategy just
// described.  This is because modifying a value at any pixel would affect a
// later calculation at its neighboring pixel.  Most of the examples in this
// section and all of the neighborhood-based image processing filters in the
// Insight toolkit follow this model.
//
// Let's begin as always by including the proper header files.  The
// \doxygen{ImageRegionIterator} will be used to write the results of
// computations to the output image.  A const version of the neighborhood
// iterator is used because the input image is read-only.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

// Software Guide : BeginCodeSnippet
#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
  if ( argc < 3 )
    {
      std::cerr << "Missing parameters. " << std::endl;
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0]
                << " inputImageFile outputImageFile"
                << std::endl;
      return -1;
    }

// Software Guide : BeginLatex
//
// Now declare the image and pixel types.  The finite difference calculations
// in this algorithm require a floating point data type.. The file reader will
// automatically cast fixed-point data to \code{float}.
//
// The second template parameter, which specifies the boundary condition, has
// been omitted from the neighborhood iterator definition because the default
// neighborhood iterator boundary condition is appropriate for this algorithm.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef float PixelType;
  typedef itk::Image< PixelType, 2 >  ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;
  
  typedef itk::ConstNeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  typedef itk::ImageRegionIterator< ImageType>        IteratorType;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The following code creates and executes ITK image reader. The \code{Update}
// call on the reader object is surrounded by the standard \code{try / catch}
// blocks to handle any exceptions that may be thrown by the reader.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
      reader->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
      std::cout << "ExceptionObject caught !" << std::endl; 
      std::cout << err << std::endl; 
      return -1;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
//  We can now create a neighborhood iterator to range over the output of the
//  reader. For Sobel edge-detection in 2D, we need a square iterator that
//  extends one pixel away from the neighborhood center in every dimension.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType it( radius, reader->GetOutput(),
                               reader->GetOutput()->GetRequestedRegion() );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The following code creates an output image and iterator.
//   
// Software Guide : EndLatex
  
// Software Guide : BeginCodeSnippet
  ImageType::Pointer output = ImageType::New();
  output->SetRegions(reader->GetOutput()->GetRequestedRegion());
  output->Allocate();

  IteratorType out(output, reader->GetOutput()->GetRequestedRegion());
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Sobel edge detection uses weighted finite difference calculations to
// construct an edge magnitude image.  For simplicity, this example only
// calculates the $x$ component of the edge magnitude to produce a derivative
// image biased towards maximally vertical edges.
//
// There are many ways to take derivatives using the neighborhood operators.
// The example in section~\ref{sec:NeighborhoodIterators2}  illustrates traditional
// convolution filtering with convolution kernels.  In this example, we instead
// use the
// neighborhood iterator \code{Set / Get} API to perform the necessary finite
// difference calculations.
//
// There are six positions in the neighborhood used for the Sobel derivatives.
// These size positions are recorded in \code{offset1}--\code{offset6}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::OffsetType offset1 = {{-1,-1}};
  NeighborhoodIteratorType::OffsetType offset2 = {{1,-1}};
  NeighborhoodIteratorType::OffsetType offset3 = {{-1,0 }};
  NeighborhoodIteratorType::OffsetType offset4 = {{1,0}};
  NeighborhoodIteratorType::OffsetType offset5 = {{-1,1}};
  NeighborhoodIteratorType::OffsetType offset6 = {{1,1}};
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Note that it is equivalent to supply the corresponding integer array
// indicies.  For example, the offsets \code{(-1,-1)} and \code{(1, -1)} are
// equivalent to the integer indicies \code{0} and \code{2}, respectively.
//
// The calculations are now done in a \code{for} loop that moves the input and
// output iterators synchronously across their respective images.  The
// \code{sum} variable is used to sum the results of the finite differences.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  for (it.GoToBegin(), out.GoToBegin(); !it.IsAtEnd(); ++it, ++out)
    {
    float sum;
    sum = it.GetPixel(offset2) - it.GetPixel(offset1);
    sum += 2.0 * it.GetPixel(offset4) - 2.0 * it.GetPixel(offset3);
    sum += it.GetPixel(offset6) - it.GetPixel(offset5);
    out.Set(sum);
    }
// Software Guide : EndCodeSnippet
  
// Software Guide : BeginLatex
//
// The last step is to write the output buffer to an image file.  This is done
// in the standard way with a \code{try / catch} block to handle any
// exceptions.  For the purpose of visualizing the output as a \code{png}
// image, it is rescaled to intensity range $[0, 255]$ and cast to unsigned
// char.
//
// Software Guide : EndLatex  
  
// Software Guide : BeginCodeSnippet
  typedef unsigned char WritePixelType;
  typedef itk::Image< WritePixelType, 2 > WriteImageType;
  typedef itk::ImageFileWriter< WriteImageType > WriterType;
  
  typedef itk::RescaleIntensityImageFilter< 
               ImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput(output);
  
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(rescaler->GetOutput());
  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;   
    }
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
// The center image of Figure~\ref{fig:NeighborhoodExamples1} shows the output of the Sobel
// algorithm run on the image \code{Insight/Examples/Data/BrainT1Slice.png}.
//
// \begin{figure} \centering
// \includegraphics[width=0.3\textwidth]{BrainT1Slice.eps}
// \includegraphics[width=0.3\textwidth]{NeighborhoodIterators1a.eps}
// \includegraphics[width=0.3\textwidth]{NeighborhoodIterators1b.eps}
// \itkcaption[Sobel edge detection results]{Applying the Sobel operator in
// different orientations to an MRI image (left) produces $x$ (center) and $y$
// (right) derivative images.}
// \protect\label{fig:NeighborhoodExamples1}
// \end{figure}
//
// Software Guide : EndLatex

  
  return 0;
}

