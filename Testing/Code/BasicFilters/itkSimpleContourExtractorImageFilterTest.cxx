/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleContourExtractorImageFilterTest.cxx
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


#include <itkImage.h>
#include <itkSimpleContourExtractorImageFilter.h>
#include "itkFilterWatcher.h"
#include <itkExceptionObject.h>
#include <itkImageFileReader.h>
#include <itkImageFileWriter.h>

int itkSimpleContourExtractorImageFilterTest(int argc, char* argv [] ) 
{
  if( argc < 3 )
    {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
    }
  
  // Define the dimension of the images
  const unsigned int Dimension = 2;

  // Define the pixel type
  typedef unsigned char PixelType;
  
  // Declare the types of the images
  typedef itk::Image<PixelType, Dimension>  ImageType;

  // Declare the reader and writer
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;
  
 
  // Declare the type for the morphology Filter
  typedef itk::SimpleContourExtractorImageFilter<
                                           ImageType, ImageType > FilterType;

  // Create the reader and writer
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );
  
  // Create the filter
  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter, "filter");

  // Connect the pipeline
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  
  filter->SetInputForegroundValue( 255 );
  filter->SetInputBackgroundValue(  0  );

  FilterType::InputSizeType radius;

  radius.Fill( 1 );
  
  filter->SetRadius( radius );
  
  // Exercise Print()
  filter->Print( std::cout );

  // Execute the filter
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception caught during pipeline Update\n"  << e;
    return EXIT_FAILURE;
    }

  // All objects should be automatically destroyed at this point

  return EXIT_SUCCESS;

}




