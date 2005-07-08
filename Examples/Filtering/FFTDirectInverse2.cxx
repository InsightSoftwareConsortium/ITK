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
  typedef unsigned char   IOPixelType;
  typedef float           WorkPixelType; 
  
  typedef itk::Image< IOPixelType,  Dimension > IOImageType;
  typedef itk::Image< WorkPixelType, Dimension > WorkImageType;
// Software Guide : EndCodeSnippet

  
// File handling
  typedef itk::ImageFileReader< IOImageType > ReaderType;
  typedef itk::ImageFileWriter< IOImageType > WriterType;
  
  ReaderType::Pointer inputreader = ReaderType::New();
  ReaderType::Pointer kernelreader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  inputreader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );



// Handle padding of the image with resampling
  typedef itk::ResampleImageFilter< 
                              IOImageType, 
                              WorkImageType >  ResamplerType;

  ResamplerType::Pointer inputresampler = ResamplerType::New();

  inputresampler->SetDefaultPixelValue(0);



// Read the image and get its size
  inputreader->Update();

  IOImageType::SizeType     inputsize;
  IOImageType::SizeType     worksize;

  inputsize = inputreader->GetOutput()->GetLargestPossibleRegion().GetSize();

// worksize is the nearest multiple of 2 larger than the input 
  for( unsigned int i=0; i < 2 ; i++ ) 
    {
    int n=0;
    worksize[i] = inputsize[i];
    while( worksize[i] >>= 1 ) 
      { 
      n++; 
      }
    worksize[i] = 1 << (n+1);

    std::cout << "worksize[" << i << "]=" << worksize[i] << std::endl;
    }

  inputresampler->SetSize( worksize );
  inputresampler->SetInput( inputreader->GetOutput() );


// Forward FFT filter
  typedef itk::FFTWRealToComplexConjugateImageFilter < 
                                              WorkPixelType, 
                                              Dimension 
                                                      > FFTFilterType;

  FFTFilterType::Pointer fftinput = FFTFilterType::New();

  fftinput->SetInput( inputresampler->GetOutput() );

// This is the output type from the FFT filters
  typedef FFTFilterType::OutputImageType ComplexImageType;



// Do the inverse transform = forward transform + flip all axes
  typedef itk::FFTWComplexConjugateToRealImageFilter < 
                                              WorkPixelType, 
                                              Dimension > invFFTFilterType;

  invFFTFilterType::Pointer fftoutput = invFFTFilterType::New();

  fftoutput->SetInput(fftinput->GetOutput()); // try to recover the input image

// Flip all axes to complete the inverse transform
  typedef itk::FlipImageFilter< WorkImageType > FlipperType;

  FlipperType::Pointer flipper = FlipperType::New();



  bool fliparray[Dimension] = {true,true};

  FlipperType::FlipAxesArrayType flipAxes( fliparray );

  flipper->SetFlipAxes(flipAxes);

  flipper->SetInput(fftoutput->GetOutput());

// Rescale the output to suit the output image type
  typedef itk::RescaleIntensityImageFilter< 
                                      WorkImageType, 
                                      IOImageType > RescaleFilterType;

  RescaleFilterType::Pointer intensityrescaler = RescaleFilterType::New();

  intensityrescaler->SetInput( flipper->GetOutput() );

  intensityrescaler->SetOutputMinimum(  0  );
  intensityrescaler->SetOutputMaximum( 255 );

// Write the output
  writer->SetInput(intensityrescaler->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;


}

