/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FFTImageFilter.cxx
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
//  In this section we assume that you are familiar with Spectral Analysis, in
//  particular with the concepts of the Fourier Transform and the numerical
//  implementation of the Fast Fourier transform. If you are not familiar with
//  these concepts you may want to consult first any of the many available
//  introductory books to spectral analysis~\cite{Bracewell1999,Bracewell2004}.
//
//  This example illustrates how to use the Fast Fourier Transform filter (FFT)
//  for processing an image in the spectral domain. Given that FFT computation
//  can be CPU intensive, there are multiple hardware specific impelmentations
//  of FFT. IT is convenient in many cases to delegate the actual computation
//  of the transform to local available libraries. Particular examples of those
//  libraries are fftw\footnote{http://www.fftw.org} and the VXL implementation
//  of FFT. For this reason ITK provides a base abstract class that factorizes
//  the interface to multiple specific implementations of FFT. This base class
//  is the \doxygen{FFTRealToComplexConjugateImageFilter}, and two of its
//  derived classes are \doxygen{VnlFFTRealToComplexConjugateImageFilter} and
//  \doxygen{FFTWRealToComplexConjugateImageFilter}.
//  
//  
//  \index{itk::FFTRealToComplexConjugateImageFilter}
//  \index{itk::VnlFFTRealToComplexConjugateImageFilter}
//  \index{itk::FFTWRealToComplexConjugateImageFilter}
//
//  Software Guide : EndLatex 

// Software Guide : BeginLatex
//
// A typical application that uses FFT will need to include the following
// header files.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"


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
  typedef itk::VnlFFTRealToComplexConjugateImageFilter< 
                                      PixelType, Dimension >  FFTFilterType;

  FFTFilterType::Pointer fftFilter = FFTFilterType::New();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The input to this filter can be taken from a reader, for example.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  fftFilter->SetInput( reader->GetOutput() );
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
    fftFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// In general the output of the FFT filter will be a complex image. We can
// proceed to save this image in a file for further analysis. This can be done
// by simply instantiating an \doxygen{ImageFileWriter} using the trait of the
// output image from the FFT filter. We construct one instance of the writer
// and pass the output of the FFT filter as the input of the writer.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef FFTFilterType::OutputImageType    ComplexImageType;

  typedef itk::ImageFileWriter< ComplexImageType > ComplexWriterType;

  ComplexWriterType::Pointer complexWriter = ComplexWriterType::New();
  complexWriter->SetFileName("complexImage.mhd");

  complexWriter->SetInput( fftFilter->GetOutput() );
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// Finally we invoke the \code{Update()} method placing inside a try/catch
// block.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  try
    {
    complexWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
// Software Guide : EndCodeSnippet






// Software Guide : BeginLatex
//
// In addition to saving the complex image into a file, we could also extract
// its real and imaginary parts for further analysis. This can be done with the
// \doxygen{ComplexToRealImageFilter} and the
// \doxygen{ComplexToImaginaryImageFilter}.
//
// We instantiate first the ImageFilter that will help us to extract the real
// part from the complex image.  The \code{ComplexToRealImageFilter} takes as
// first template parameter the type of the complex image and as second
// template parameter it takes the type of the output image pixel. We create
// one instance of this filter and connect as its input the output of the FFT
// filter.
//
// \index{itk::ComplexToRealImageFilter}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ComplexToRealImageFilter< 
                 ComplexImageType, ImageType > RealFilterType;

  RealFilterType::Pointer realFilter = RealFilterType::New();

  realFilter->SetInput( fftFilter->GetOutput() );
// Software Guide : EndCodeSnippet


  typedef unsigned char WritePixelType;
  typedef itk::Image< WritePixelType, Dimension > WriteImageType;


  
// Software Guide : BeginLatex
//
// Since the range of intensities in the Fourier domain can be quite
// concentrated, it result convenient to rescale the image in order to
// visualize it. For this purpose we instantiate here a
// \doxygen{RescaleIntensityImageFilter} that will rescale the intensities of
// the \code{real} image into a range suitable for writing in a file. We also
// set the minimum and maximum values of the output to the range of the pixel
// type used for writing.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::RescaleIntensityImageFilter< 
                                ImageType, 
                                WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer intensityRescaler = RescaleFilterType::New();

  intensityRescaler->SetInput( realFilter->GetOutput() );

  intensityRescaler->SetOutputMinimum(  0  );
  intensityRescaler->SetOutputMaximum( 255 );
// Software Guide : EndCodeSnippet






  typedef itk::ImageFileWriter< WriteImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );

  writer->SetInput( intensityRescaler->GetOutput() );

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



// Software Guide : BeginLatex
//
// We can now instantiate the ImageFilter that will help us to extract the
// imaginary part from the complex image.  The filter that we use here is the
// \doxygen{ComplexToImaginaryImageFilter}. It takes as first template
// parameter the type of the complex image and as second template parameter it
// takes the type of the output image pixel. An instance of the filter is
// created, and its input is connected to the output of the FFT filter.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef FFTFilterType::OutputImageType    ComplexImageType;

  typedef itk::ComplexToImaginaryImageFilter< 
                       ComplexImageType, ImageType > ImaginaryFilterType;

  ImaginaryFilterType::Pointer imaginaryFilter = ImaginaryFilterType::New();

  imaginaryFilter->SetInput( fftFilter->GetOutput() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The Imaginary image can then be rescaled and saved into a file, just as we
// did with the Real part.
//
// Software Guide : EndLatex 



  intensityRescaler->SetInput( imaginaryFilter->GetOutput() );
  writer->SetFileName( argv[3] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error writing the imaginary image: " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


// Software Guide : BeginLatex
//
// For the sake of illustrating the use of a \doxygen{ImageFileReader} on
// Complex images, here we instantiate a reader that will load the Complex
// image that we just saved. Note that nothing special is required in this
// case. The instantiation is done just the same as for any other type of
// image. Which once again illustrates the power of Generic Programming.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ComplexImageType > ComplexReaderType;
  
  ComplexReaderType::Pointer complexReader = ComplexReaderType::New();

  complexReader->SetFileName("complexImage.mhd");
  complexReader->Update();
// Software Guide : EndCodeSnippet

  
  // A way of testing the pixel type of an image in file is to
  // invoke the ImageIO object from the reader and then call
  // \code{GetPixelTypeAsString()}
  complexReader->GetImageIO()->GetPixelTypeAsString(
                    complexReader->GetImageIO()->GetPixelType() );


  return EXIT_SUCCESS;
}

