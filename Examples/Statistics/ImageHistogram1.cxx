/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageHistogram1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// This example shows how to compute the histogram of a scalar image.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkScalarImageToListAdaptor.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"
#include "itkListSampleToHistogramGenerator.h"

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


  typedef itk::Statistics::ScalarImageToListAdaptor< ImageType >   AdaptorType;

  AdaptorType::Pointer adaptor = AdaptorType::New();

  adaptor->SetImage(  reader->GetOutput() );

  typedef PixelType        HistogramMeasurementType;

  typedef itk::Statistics::ListSampleToHistogramGenerator< 
                                                AdaptorType, 
                                                HistogramMeasurementType 
                                                                > GeneratorType;

  GeneratorType::Pointer generator = GeneratorType::New();

  typedef GeneratorType::HistogramType  HistogramType;

  HistogramType::SizeType size;
  size.Fill( 255 );

  generator->SetListSample( adaptor );
  generator->SetNumberOfBins( size );
  generator->SetMarginalScale( 10.0 );
  generator->Update();

  HistogramType::ConstPointer histogram = generator->GetOutput();

  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, 0 ) <<std::endl;
    }

  return 0;
  
}


