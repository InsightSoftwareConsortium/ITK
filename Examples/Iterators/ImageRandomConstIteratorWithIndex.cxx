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
// This code example does some simple image statistics using the
// \code{itk::ImageRandomConstIteratorWithIndex}.  We will calculate an
// estimate of the arithmetic mean of the image values.
//
// First, We include the appropriate header and declare pixel and image types.
// For this example, we will work with a scalar-valued image.
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

  const unsigned int Dimension = 2;
  
  // Software Guide : BeginCodeSnippet
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
// The \code{IsAtBegin} and \code{IsAtEnd} methods take on a different meaning
// in the context of this iterator.  Beginning and ending ``positions'' of the
// iterator are defined based on the number of samples.  As the random iterator
// is incremented, it keeps a sample count.  \code{IsAtEnd()} returns
// \code{true} when the sample count is equal to the user-defined
// \code{NumberOfSamples} parameter. \code{IsAtBegin()} returns \code{true}
// when the sample count is zero.
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
// Now take the specified number of samples and calculate its average.
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
