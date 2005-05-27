/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FFTImageFilterFourierDomainFiltering.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


//  Software Guide : BeginLatex
//
//  One of the most common image processing operations performed in the Fourier
//  Domain is the masking of the spectrum in order to eliminate a rango of
//  spatial frequencies from the input image. This operation is typically
//  performed by taking the input image, computing its Fourier transform using
//  a FFT filter, multiplying the resulting image in the Fourier domain with a
//  mask, and finally taking the result of the masking and computing its
//  inverse Fourier transform.
//  
//  This typical processing is what it is illustrated in the example below.
//  
//  \index{itk::FFTRealToComplexConjugateImageFilter}
//  \index{itk::VnlFFTRealToComplexConjugateImageFilter}
//  \index{itk::FFTWRealToComplexConjugateImageFilter}
//  \index{itk::MaskImageFilter}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv [] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0] << " inputScalarImage  outputRealPartOfComplexImage outputRealImaginaryPartOfComplexImage" << std::endl;
    }


// Software Guide : BeginLatex
//
// The first decision to make is related to the pixel type and dimension of the
// images on which we want to compute the Fourier transform. 
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef float  PixelType;
  const unsigned int Dimension = 2;

  typedef itk::Image< PixelType, Dimension > ImageType;
// Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< ImageType >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );




// Software Guide : BeginLatex
//
// We use the same image type in order to instantiate the FFT filter. In this
// case the \doxygen{VnlFFTRealToComplexConjugateImageFilter}. Note that
// contrary to most ITK filters, the FFT filter is instantiated using the Pixel
// type and the image dimension explicitly. Once the filter type is
// instantiated, we can use it for creating one object by invoking the
// \code{New()} method and assigning the result to a SmartPointer.
// 
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::VnlFFTRealToComplexConjugateImageFilter< PixelType, Dimension >  FFTFilterType;

  FFTFilterType::Pointer filter = FFTFilterType::New();

  filter->SetInput( reader->GetOutput() );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The execution of the filter can be triggered by invoking the \code{Update()}
// method.  Since this invokation can eventually throw and exception, the call
// must be placed inside a try/catch block.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
// Software Guide : EndCodeSnippet




  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );

//  writer->SetInput( intensityRescaler->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error writing the real image: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;
}

