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
// This example uses the \code{itk::NeighborhoodIterator} to implement a simple
// Sobel edge detection algorithm ([reference here?].  Start by including
// the proper header files.  The \code{ImageRegionIterator} is used to write the
// results of computations to the output image.  A const version of the
// neighborhood iterator is used because the input image to the algorithm is
// not modified.
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

// Software Guide : BeginCodeSnippet
#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
  // Verify the number of parameters on the command line.
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
// Next declare image and pixel types.  The floating point pixel type is used
// because the algorithm performs finite difference calculations.  The file
// readers and writers will automatically cast non floating point data to the
// correct type.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef float PixelType;
  typedef itk::Image< PixelType, 2 >  ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  typedef itk::ConstNeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  typedef itk::ImageRegionIterator< ImageType>        IteratorType;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
// The first step is to read the input image.  The following code creates and
// executes ITK image reader.  Refer to section ??? for more information.  The
// \code{Update} call on the reader object is surrounded by the standard
// \code{try / catch} blocks to handle any thrown exceptions.
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
// Once the file reader has updated,  we can set up a neighborhood
// iterator over the reader's output to perform the edge detection calculations.
// The radius of the neighborhood must be supplied when the iterator is
// created.  For Sobel edge-detection in 2D, we need a square iterator that
// extends one pixel away from the neighborhood center in every dimension.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType it( radius, reader->GetOutput(), reader->GetOutput()->GetRequestedRegion() );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// NOTE HERE ABOUT BOUNDARY CONDITIONS.  USING THE DEFAULT BOUNDARY CONDITIONS
// blah blah 
//
//
// In general, neighborhood-based image processing cannot operate in-place on a
// single image buffer because modifying a value at any pixel would affect a later
// calculation at its neighboring pixel.  For our Sobel algorithm, we allocate
// an output image of the same size as the input.
//
// Software Guide : EndLatex
  
// Software Guide : BeginCodeSnippet
  ImageType::Pointer output = ImageType::New();
  output->SetRegions(reader->GetOutput()->GetRequestedRegion());
  output->Allocate();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// Now we set up an iterator to write to the output image.
// Software Guide : EndLatex

// Software Guide :BeginCodeSnippet
  IteratorType out(output, reader->GetOutput()->GetRequestedRegion());
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Sobel edge detection involves weighted finite difference operations in the
// neighborhood of the each pixel.  Figure~\ref show the standard Sobel
// convolution kernel for edge-detection in the $X$ direction.
//
// There are many ways to implement Sobel edge detection using the neighborhood
// operators.  Example~[NEXT EXAMPLE] illustrates traditional convolution
// filtering with convolution kernels.  In this example, we use the
// neighborhood iterator \code{Set / Get} API to perform the necessary finite
// difference calculations.
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
// Note that it is equivalent to supply the integer indicies that correspond to
// these offsets in the neighborhood.  The offsets \code{(-1,-1)} and \code{(1,
// -1)}, for example, are equivalent to the integer indicies \code{0} and
// \code{2}, respectively.
//
// The calculations are done in a \code{for} loop that iterates the input and
// output iterators across their respective images.  The \code{sum} variable is
// used to sum the results of the finite differences.
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
// The final step is to write the output buffer to an image file.  This is done
// in the standard way with a \code{try / catch} block to handle any
// exceptions.  The file name is read from the command line.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet









  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(output);
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

  return 0;
}

