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
//  Domain is the masking of the spectrum in order to eliminate a range of
//  spatial frequencies from the input image. This operation is typically
//  performed by taking the input image, computing its Fourier transform using
//  a FFT filter, masking the resulting image in the Fourier domain with a
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


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"



// Software Guide : BeginLatex
//
// We start by including the headers of the FFT filters and the Mask image
// filter. Note that we use two different types of FFT filters here. The first
// one expects as input an image of real pixel type (real in the sense of
// complex numbers) and produces as ouput a complex image. The second FFT
// filter expects as in put a complex image and produces a real image as
// output.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include "itkVnlFFTComplexConjugateToRealImageFilter.h"
#include "itkMaskImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv [] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0] << " inputScalarImage  inputMaskImage";
    std::cerr << " outputFilteredImage" << std::endl;
    }


// Software Guide : BeginLatex
//
// The first decision to make is related to the pixel type and dimension of the
// images on which we want to compute the Fourier transform. 
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef float  InputPixelType;
  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType, Dimension > InputImageType;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Then we select the pixel type to use for the mask image and instantiate the
// image type of the mask.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef unsigned char  MaskPixelType;

  typedef itk::Image< MaskPixelType, Dimension > MaskImageType;
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// Both the input image and the mask image can be read from files or could be
// obtained as the output of a preprocesing pipeline. We omit here the details
// of reading the image since the process is quite standard.
//
// Software Guide : EndLatex 



  typedef itk::ImageFileReader< InputImageType >    InputReaderType;
  typedef itk::ImageFileReader< MaskImageType  >    MaskReaderType;

  InputReaderType::Pointer inputReader = InputReaderType::New();
  MaskReaderType::Pointer  maskReader  = MaskReaderType::New();

  inputReader->SetFileName( argv[1] );
  maskReader->SetFileName( argv[2] );



// Software Guide : BeginLatex
//
// Now the \doxygen{VnlFFTRealToComplexConjugateImageFilter} can be instantiated.
// Note that contrary to most ITK filters, the FFT filter is instantiated using
// the Pixel type and the image dimension explicitly. Using the type we
// construct one instance of the filter.
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::VnlFFTRealToComplexConjugateImageFilter< 
                                  InputPixelType, Dimension >  FFTFilterType;

  FFTFilterType::Pointer fftFilter = FFTFilterType::New();

  fftFilter->SetInput( inputReader->GetOutput() );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Since our purpose is to perform filtering in the frequency domain by
// altering the weights of the image spectrum, we need here a filter that will
// mask the Fourier transform of the input image with a binary image. Note that the
// type of the spectral image is taken here from the traits of the FFT filter. 
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef FFTFilterType::OutputImageType    SpectralImageType;

  typedef itk::MaskImageFilter< SpectralImageType,
                                    MaskImageType, 
                                    SpectralImageType >  MaskFilterType;

  MaskFilterType::Pointer maskFilter = MaskFilterType::New();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We connect the inputs to the mask filter by taking the outputs from the
// first FFT filter and from the reader of the Mask image.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
maskFilter->SetInput1( fftFilter->GetOutput() );
maskFilter->SetInput2( maskReader->GetOutput() ); 
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// For the purpose of verifying the aspect of the spectrum after being filtered
// witht the mask, we can write out the output of the Mask filter to a file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
typedef itk::ImageFileWriter< SpectralImageType > SpectralWriterType;
SpectralWriterType::Pointer spectralWriter = SpectralWriterType::New();
spectralWriter->SetFileName("filteredSpectrum.mhd");
spectralWriter->SetInput( maskFilter->GetOutput() );
spectralWriter->Update();
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// The output of the mask filter will contain the \emph{filtered} spectrum
// of the input image. We must then apply an inverse Fourier transform on it in
// order to obtain the filtered version of the input image. For that purpose we 
// create another instance of the FFT filter.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::VnlFFTComplexConjugateToRealImageFilter< 
                                  InputPixelType, Dimension >  IFFTFilterType;

  IFFTFilterType::Pointer fftInverseFilter = IFFTFilterType::New();

  fftInverseFilter->SetInput( maskFilter->GetOutput() );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// The execution of the pipeline can be triggered by invoking the
// \code{Update()} method in this last filter.  Since this invokation can
// eventually throw and exception, the call must be placed inside a try/catch
// block.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  try
    {
    fftInverseFilter->Update();
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
// The result of the filtering can now be saved into an image file, or be
// passed to a subsequent processsing pipeline. Here we simply write it out to
// an image file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< InputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( fftInverseFilter->GetOutput() );
// Software Guide : EndCodeSnippet


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
// Note that this example is just a minimal illustration of the multiple types
// of processing that are possible in the Fourier domain.
//
// Software Guide : EndLatex 


  return EXIT_SUCCESS;
}

