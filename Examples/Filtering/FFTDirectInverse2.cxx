/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FFTDirectInverse2.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include "itkConfigure.h"

//
// This example is based on the on that was contributed by Stephan in the users list
//
//     http://public.kitware.com/pipermail/insight-users/2005-June/013482.html
//
//

// Software Guide : BeginLatex
//
// This example illustrates how to compute the direct Fourier transform
// followed by the inverse Fourier transform using the FFTW library.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkFFTWRealToComplexConjugateImageFilter.h"
#include "itkFFTWComplexConjugateToRealImageFilter.h"
#include "itkFlipImageFilter.h"

#if !defined(USE_FFTWF)
//#error "This example only works when single precision FFTW is used
//Changing WorkPixeltype to double and changing this conditional to USE_FFTWD
//will also work.
#endif

int main( int argc, char * argv[] )
{
  if( argc != 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output" << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginLatex
//
// First we set up the types of the input and output images.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const unsigned int      Dimension = 2;
  typedef unsigned char   OutputPixelType;
  typedef float           WorkPixelType;

  typedef itk::Image< WorkPixelType,  Dimension > InputImageType;
  typedef itk::Image< WorkPixelType,  Dimension > WorkImageType;
  typedef itk::Image< OutputPixelType,Dimension > OutputImageType;
// Software Guide : EndCodeSnippet

// File handling
  typedef itk::ImageFileReader< InputImageType  > ReaderType;
  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  ReaderType::Pointer inputreader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  inputreader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

// Read the image and get its size
  inputreader->Update();

// Forward FFT filter
  typedef itk::FFTWRealToComplexConjugateImageFilter <
                                              WorkPixelType,
                                              Dimension
                                                      > FFTFilterType;

  FFTFilterType::Pointer fftinput = FFTFilterType::New();
  fftinput->SetInput( inputreader->GetOutput() );
  fftinput->Update();

// This is the output type from the FFT filters
  typedef FFTFilterType::OutputImageType ComplexImageType;

// Do the inverse transform = forward transform + flip all axes
  typedef itk::FFTWComplexConjugateToRealImageFilter <
                                              WorkPixelType,
                                              Dimension > invFFTFilterType;

  invFFTFilterType::Pointer fftoutput = invFFTFilterType::New();
  fftoutput->SetInput(fftinput->GetOutput()); // try to recover the input image
  fftoutput->Update();

// Rescale the output to suit the output image type
  typedef itk::RescaleIntensityImageFilter<
                                      WorkImageType,
                                      OutputImageType > RescaleFilterType;

  RescaleFilterType::Pointer intensityrescaler = RescaleFilterType::New();

  std::cout << fftoutput->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  intensityrescaler->SetInput( fftoutput->GetOutput() );

  intensityrescaler->SetOutputMinimum(  0  );
  intensityrescaler->SetOutputMaximum( 255 );

// Write the output
  writer->SetInput(intensityrescaler->GetOutput());
  writer->Update();

  //DEBUG: std::cout << "inputreader "<<inputreader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  //DEBUG: std::cout << "fftinput " <<fftinput->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  //DEBUG: std::cout << "fftoutput " <<fftoutput->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  //DEBUG: std::cout << "intensityrescaller " <<intensityrescaler->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  return EXIT_SUCCESS;

}

