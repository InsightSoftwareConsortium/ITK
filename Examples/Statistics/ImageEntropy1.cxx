/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageEntropy1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// This example shows how to compute the Entropy of an image. 
//
// More formally this should be said : The reduction in uncertainty gained when
// we actually measure a randomly selected pixel in this image, given that we
// already know the intensity distribution of the image.
//
// Software Guide : EndLatex 


#include "itkScalarImageToHistogramGenerator.h"
#include "itkImage.h"

#include "itkImageFileReader.h"

int main( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageHistogram1  inputImageFileName " << std::endl;
    return -1;
    }


  typedef unsigned char       PixelType;
  const unsigned int          Dimension = 2;

  typedef itk::Image<PixelType, Dimension > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return -1;
    }

  typedef itk::Statistics::ScalarImageToHistogramGenerator< 
                                                    ImageType 
                                                          >   HistogramGeneratorType;

  HistogramGeneratorType::Pointer histogramGenerator = HistogramGeneratorType::New();

  histogramGenerator->SetInput(  reader->GetOutput() );

  histogramGenerator->SetNumberOfBins( 255 );
  histogramGenerator->SetMarginalScale( 10.0 );
  histogramGenerator->Compute();

  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();

  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, 0 ) << std::endl;
    }

// Software Guide : BeginLatex
//
// Since the computation of the image histogram itself has been demostrated in
// previous examples, we focus here on the \emph{estimation} of Entropy given
// the histogram. The first conceptual jump to be done here is that we assume
// that the histogram, which is the simple count of frequency of occurrence for
// the grayscale values of the image pixels, can be normalized in order to
// estimate the probability density function \textbf{PDF} of the actual
// estatistical distribution of pixel values. 
//
//  Note that the log(2) is used in order to convert the base of the natural
//  logarithm in to the a logarithm of base 2, and make possible to report the
//  Entropy in its natural unit: the bit.
//  
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  HistogramType::ConstIterator itr = histogram->Begin();
  HistogramType::ConstIterator end = histogram->End();
 
  double Sum = histogram->GetTotalFrequency();

  double Entropy = 0.0;

  while( itr != end )
    {
    const double probability = itr.GetFrequency() / Sum;

    if( probability > 1e-16 )
      {
      Entropy += - probability * log( probability ) / log( 2.0 );
      }
    ++itr;
    }

  std::cout << "Image entropy = " << Entropy << " bits " << std::endl;

// Software Guide : EndCodeSnippet
  return 0;
  
}


