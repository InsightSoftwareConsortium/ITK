/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDifferenceImageFilterTest.cxx
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
#include <iostream>
#include "itkImage.h"
#include "itkDifferenceImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

int itkDifferenceImageFilterTest(int argc, char *argv [] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0];
    std::cerr << "  inputImageFile1 inputImageFile2 outputImage threshold radius" << std::endl;
    return EXIT_FAILURE;
    }


  // Test using an unsigned integral pixel type and generate a signed
  // integral pixel type
  typedef   signed   short  InputPixelType;
  typedef   unsigned short  OutputPixelType;

  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );

  // Define the filter
  typedef itk::DifferenceImageFilter<
                             InputImageType, 
                             OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  // setup the filter
  filter->SetDifferenceThreshold( atoi( argv[4] ) );
  filter->SetToleranceRadius(     atoi( argv[5] ) );

  itk::SimpleFilterWatcher watcher( filter, "Difference");
  
  // wire the pipeline
  filter->SetValidInput( reader1->GetOutput() );
  filter->SetTestInput(  reader2->GetOutput() );

  // Write the output
  typedef itk::ImageFileWriter< OutputImageType >       WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( filter->GetOutput() );

  writer->SetFileName( argv[3] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception : " << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
