/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkComposeRGBAImageFilterTest.cxx
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
//Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
#pragma warning( disable : 4786 )
#endif

// General includes
#include <iostream>

// ITK includes
#include "itkNumericTraits.h"
#include "itkRGBAPixel.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkComposeRGBImageFilter.h"
#include "itkComposeRGBAImageFilter.h"

int itkComposeRGBAImageFilterTest(int argc, char* argv[])
{

  if( argc < 6 )
    {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " outputFile inputFileR inputFileG inputFileB inputFileA" << std::endl;
    }

  try
    {
    // ARGUMENTS:
    //argv[0] = Executable name
    //argv[1] = Output file name and path
    //argv[2] = Input 1 file name and path
    //argv[2] = Input 2 file name and path
    //argv[2] = Input 3 file name and path
    //argv[2] = Input 4 file name and path

    // Get arguments
    char* OutputFilename  = argv[1];
    char* Input1Filename  = argv[2];
    char* Input2Filename  = argv[3];
    char* Input3Filename  = argv[4];
    char* Input4Filename  = argv[5];

    // Typedefs
    typedef unsigned char                                 ScalarPixelType;
    const unsigned int                                    Dimension = 2;
    typedef itk::RGBAPixel< ScalarPixelType >             RGBAPixelType;
    typedef itk::Image< ScalarPixelType, Dimension >      ScalarImageType;
    typedef itk::Image< RGBAPixelType, Dimension >        RGBAImageType;
    typedef itk::ImageFileReader< ScalarImageType >       ReaderType;
    typedef itk::ImageFileWriter< RGBAImageType >         WriterType;
    typedef itk::ComposeRGBAImageFilter< 
      ScalarImageType, RGBAImageType >                    ComposeFilterType;

    // Read input1
    ReaderType::Pointer reader1 = ReaderType::New();
    reader1->SetFileName( Input1Filename );
    reader1->Update();
    
    // Read input2
    ReaderType::Pointer reader2 = ReaderType::New();
    reader2->SetFileName( Input2Filename );
    reader2->Update();

    // Read input3
    ReaderType::Pointer reader3 = ReaderType::New();
    reader3->SetFileName( Input3Filename );
    reader3->Update();

    // Read input4
    ReaderType::Pointer reader4 = ReaderType::New();
    reader4->SetFileName( Input4Filename );
    reader4->Update();

    // Test ComposeRGBA filter
    ComposeFilterType::Pointer filterCompose = ComposeFilterType::New();
    filterCompose->SetInput( 0, reader1->GetOutput() );
    filterCompose->SetInput( 1, reader2->GetOutput() );
    filterCompose->SetInput( 2, reader3->GetOutput() );
    filterCompose->SetInput( 3, reader4->GetOutput() );
    filterCompose->Update();

        // Write output
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( OutputFilename );
    writer->SetInput( filterCompose->GetOutput() );
    writer->Update();
    }
  catch (itk::ExceptionObject & err) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return EXIT_FAILURE;
    } 

  //Return
  return EXIT_SUCCESS;
}
