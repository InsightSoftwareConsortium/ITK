/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegionIteratorWithIndex.cxx
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
// The following example illustrates the use of
// \code{itk::ImageRegionIteratorWithIndex}.  The algorithm shown here
// mirrors a 2D image across its x axis (see \code{itk::FlipImageAxis} for an
// ND version).  The algorithm makes extensive use of the \code{GetIndex()}
// method, calculating the mirrored index at each pixel to copy from the
// original image.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkRGBPixel.h"
// Software Guide : BeginCodeSnippet
#include "itkImageRegionIteratorWithIndex.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

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
//
// Image and pixel types are defined as in the previous example
// (section~\ref{sec:itkImageRegionIterator}). The
// \code{itk::ImageRegionIteratorWithIndex} class has a single template
// parameter, the image type.
//
// Software Guide : EndLatex


  const unsigned int Dimension = 2;
  
  typedef itk::RGBPixel< unsigned char > RGBPixelType;
  typedef itk::Image< RGBPixelType, Dimension >  ImageType;
  
  // Software Guide : BeginCodeSnippet
  typedef itk::ImageRegionIteratorWithIndex< ImageType >       IteratorType;
  // Software Guide : EndCodeSnippet
  
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  ImageType::ConstPointer inputImage;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
      reader->Update();
      inputImage = reader->GetOutput();
    }
  catch ( itk::ExceptionObject &err)
    {
      std::cout << "ExceptionObject caught !" << std::endl; 
      std::cout << err << std::endl; 
      return -1;
    }

// Software Guide : BeginLatex
//
// After reading the input image, we allocate an output image that of the same
// size.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::Pointer outputImage = ImageType::New();
  outputImage->SetRegions( inputImage->GetRequestedRegion() );
  outputImage->Allocate();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we create the iterator to walk the output image.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  IteratorType outputIt( outputImage, outputImage->GetRequestedRegion() );
// Software Guide : EndCodeSnippet

// Software Guide: BeginLatex
//
// This axis flipping algorithm works by iterating through the output image, querying
// the iterator for its index, and copying the value from the input at an index
// mirrored across the x axis.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  for ( outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt)
    {
      ImageType::IndexType idx = outputIt.GetIndex();

      idx[0] = outputImage->GetRequestedRegion().GetIndex()[0]
        + outputImage->GetRequestedRegion().GetSize()[0] - idx[0];
      
      outputIt.Set( inputImage->GetPixel(idx) );
    }
// Software Guide : EndCodeSnippet

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(outputImage);
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

  return 0;
}
