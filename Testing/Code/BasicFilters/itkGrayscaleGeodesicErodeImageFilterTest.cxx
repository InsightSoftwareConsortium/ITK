/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleGeodesicErodeImageFilterTest.cxx
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

#include "itkGrayscaleGeodesicErodeImageFilter.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int itkGrayscaleGeodesicErodeImageFilterTest(int argc, char* argv [] ) 
{ 
  if ( argc < 3 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " Inputimage OutputImage" << std::endl;
    return EXIT_FAILURE;
  } 
  const int Dimension = 3;
  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, Dimension >   InputImageType;
  typedef itk::Image< PixelType, Dimension >   OutputImageType;
  
  typedef itk::ImageFileReader< InputImageType >   ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  
  typedef itk::GrayscaleGeodesicErodeImageFilter < InputImageType, 
                                                   OutputImageType > 
                               MorphologicalFilterType;
  
  ReaderType::Pointer           reader = ReaderType::New();
  WriterType::Pointer           writer = WriterType::New();

  // Create the reader and writer
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Create the filter
  MorphologicalFilterType::Pointer   filter = MorphologicalFilterType::New();
  
  // Connect the pipelines
  filter->SetInput ( reader-> GetOutput() );
  writer->SetInput ( filter-> GetOutput() );


  // Execute print
  filter->Print( std::cout );

  // Execute the filter
  try
  {
    writer->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Exception caught:" << excp << std::endl;
    return  EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}




