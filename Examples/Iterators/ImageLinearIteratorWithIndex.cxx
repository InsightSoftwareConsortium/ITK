/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageLinearIteratorWithIndex.cxx
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
// The next code example shows how to use the
// \code{itk::ImageLinearIteratorWithIndex}.  It implements the same
// algorithm as in the previous example, flipping an image across its x axis.
// Two line iterators are used, moving in opposite directions on the x axis,
// and iterating line by line down the y axis.
//
// We include headers for both the const and non-const versions.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkRGBPixel.h"
// Software Guide : BeginCodeSnippet
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
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
// Image and pixel types are defined as in the previous examples
// (section~\ref{sec:itkImageLinearIterator}). The
// \code{itk::ImageLinearIteratorWithIndex} class and its const version have
// single template parameters, the image type.
//
// Software Guide : EndLatex

  const unsigned int Dimension = 2;
  
  typedef itk::RGBPixel< unsigned char > RGBPixelType;
  typedef itk::Image< RGBPixelType, Dimension >  ImageType;
  
  // Software Guide : BeginCodeSnippet
  typedef itk::ImageLinearIteratorWithIndex< ImageType >       IteratorType;
  typedef itk::ImageLinearConstIteratorWithIndex< ImageType >  ConstIteratorType;
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
// Next we create the two iterators.  The const iterator walks the input image,
// and the non-const iterator walks the output image.  The iterators are
// initialized over the same region.  The direction of iteration is set to 0,
// the x dimension.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ConstIteratorType inputIt(  inputImage,  inputImage->GetRequestedRegion() );
  IteratorType outputIt( outputImage, inputImage->GetRequestedRegion() );

  inputIt.SetDirection(0);
  outputIt.SetDirection(0);
// Software Guide : EndCodeSnippet

// Software Guide: BeginLatex
//
// This version of the axis flipping algorithm works by moving the two
// iterators line by line through the image, where a line spans the columns of
// the image.  The input iterator moves forward across columns while the output
// iterator moves backwards.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  for ( inputIt.GoToBegin(),  outputIt.GoToBegin(); ! inputIt.IsAtEnd();
        outputIt.NextLine(),  inputIt.NextLine())
    {
      inputIt.GoToBeginOfLine();
      outputIt.GoToEndOfLine();
      --outputIt;
      while ( ! inputIt.IsAtEndOfLine() )
        {
          outputIt.Set( inputIt.Get() );
          ++inputIt;
          --outputIt;
        }
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
