/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageHistogram2.cxx
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
// This example shows how to compute the histogram of a scalar image.
// using the helper class \doxygen{ScalarImageToHistogramGenerator}.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkScalarImageToHistogramGenerator.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet

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

  unsigned int bin;
  for( bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, 0 ) << std::endl;
    }

// Software Guide : BeginLatex
//
// It is also possible to use iterators in order to visit the values
// of the histogram bins.
//
// Software Guide : EndLatex 


  HistogramType::ConstIterator itr = histogram->Begin();
  HistogramType::ConstIterator end = histogram->End();
 
  bin = 0;
  while( itr != end )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << itr.GetFrequency() << std::endl;     
    ++itr;
    ++bin;
    }


  return 0;
  
}


