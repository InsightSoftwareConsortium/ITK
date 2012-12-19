/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

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
#include "itkFFTWForwardFFTImageFilter.h"
#include "itkFFTWInverseFFTImageFilter.h"
#include "itkFlipImageFilter.h"

#if !defined(ITK_USE_FFTWF)
//#error "This example only works when single precision FFTW is used
//Changing WorkPixeltype to double and changing this conditional to ITK_USE_FFTWD
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
  //  typedef unsigned char   OutputPixelType;
  typedef unsigned short  OutputPixelType;
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
  typedef itk::FFTWForwardFFTImageFilter < InputImageType > FFTFilterType;

  FFTFilterType::Pointer fftinput = FFTFilterType::New();
  fftinput->SetInput( inputreader->GetOutput() );
  fftinput->Update();

// This is the output type from the FFT filters
  typedef FFTFilterType::OutputImageType ComplexImageType;

// Do the inverse transform = forward transform + flip all axes
  typedef itk::FFTWInverseFFTImageFilter < ComplexImageType > invFFTFilterType;

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
  intensityrescaler->SetOutputMaximum( 65535 );

  // Write the output
  writer->SetInput(intensityrescaler->GetOutput());
  writer->Update();

  //DEBUG: std::cout << "inputreader "<<inputreader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  //DEBUG: std::cout << "fftinput " <<fftinput->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  //DEBUG: std::cout << "fftoutput " <<fftoutput->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  //DEBUG: std::cout << "intensityrescaller " <<intensityrescaler->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  return EXIT_SUCCESS;

}
