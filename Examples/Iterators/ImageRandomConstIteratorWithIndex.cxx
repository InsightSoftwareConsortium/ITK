/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRandomConstIteratorWithIndex.cxx
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
// Sometimes an algorithm needs to find a random sample of pixel values in an
// image. \code{itk::ImageRandomConstIteratorWithIndex} was developed for that
// purpose. When incremented or decremented, the iterator simply jumps to a
// random location in its image region.
//
// The user must specify a sample size when creating this iterator. The sample
// size defines the end position for the iterator.  \code{IsAtEnd()} returns
// \code{true} when the current sample number equals the sample size.
// \code{IsAtBegin()} returns \code{true} when the current sample number equals
// zero.  Another important difference from other iterators is that
// \code{itk::ImageRandomConstIteratorWithIndex} may visit the same pixel
// location more than once.
//
// Let's use the random iterator to do some simple image statistics. The next
// example calculates an estimate of the arithmetic mean of pixel values.
//
// First, include the appropriate header and declare pixel and image types.
//
// Software Guide : EndLatex

#include "itkImage.h"
// Software Guide : BeginCodeSnippet
#include "itkImageRandomConstIteratorWithIndex.h"
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
                << " inputImageFile numberOfSamples"
                << std::endl;
      return -1;
    }

// Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;

  typedef unsigned short  PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;  
  typedef itk::ImageRandomConstIteratorWithIndex< ImageType >  ConstIteratorType;
// Software Guide : EndCodeSnippet
  
  typedef itk::ImageFileReader< ImageType > ReaderType;

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
// The input image has been read as \code{inputImage}.  We now create an
// iterator with a number of samples set by command line argument. The call to
// \code{ReinitializeSeed} seeds the random number generator.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ConstIteratorType inputIt(  inputImage,  inputImage->GetRequestedRegion() );
  inputIt.SetNumberOfSamples( ::atoi( argv[2]) );
  inputIt.ReinitializeSeed();
// Software Guide : EndCodeSnippet

// Software Guide: BeginLatex
//
// Now take the specified number of samples and calculate their average value.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  float mean = 0.0f;
  for ( inputIt.GoToBegin(); ! inputIt.IsAtEnd(); ++inputIt)
    {
      mean += static_cast<float>( inputIt.Get() );
    }
  mean = mean / ::atof( argv[2] );
  
// Software Guide : EndCodeSnippet
  std::cout << "Mean estimate with " << argv[2] << " samples is " << mean << std::endl;

  return 0;
}
