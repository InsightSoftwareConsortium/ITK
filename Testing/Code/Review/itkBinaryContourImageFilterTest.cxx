/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryContourImageFilterTest.cxx
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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkSimpleFilterWatcher.h"
#include <itkBinaryContourImageFilter.h>

int itkBinaryContourImageFilterTest(int argc, char * argv[])
{

  if( argc != 6 )
    {
    std::cerr << "usage: " << argv[0] << " intput output fullyConnected fg bg" << std::endl;
    std::cerr << " input: the input image" << std::endl;
    std::cerr << " output: the output image" << std::endl;
    std::cerr << " fullyConnected: 0 or 1" << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 3;
  
  typedef unsigned char            PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::BinaryContourImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  // test default values
  if ( filter->GetFullyConnected( ) != false )
    {
    std::cerr << "Wrong default FullyConnected." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetForegroundValue( ) != 255 )
    {
    std::cerr << "Wrong default foreground value." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetBackgroundValue( ) != 0 )
    {
    std::cerr << "Wrong default background value." << std::endl;
    return EXIT_FAILURE;
    }

  // 
  // Tests for raising code coverage
  //
  try
    {
    filter->Update();
    std::cerr << "Failed to throw expected exception" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
    std::cout << "catched EXPECTED exception for emtpy image as input" << std::endl;
    // TODO: should ResetPipeline() be required?
    filter->ResetPipeline();
    }

  filter->FullyConnectedOn();
  if( !filter->GetFullyConnected() )
    {
    std::cerr << "Set/GetFullyConnected() error" << std::endl;
    return EXIT_FAILURE;
    }


  // set the inputs

  filter->SetInput( reader->GetOutput() );

  filter->FullyConnectedOff();
  if( filter->GetFullyConnected() )
    {
    std::cerr << "Set/GetFullyConnected() error" << std::endl;
    return EXIT_FAILURE;
    }


  filter->SetFullyConnected( atoi(argv[3]) );
  if ( filter->GetFullyConnected( ) != (bool)atoi(argv[3]) )
    {
    std::cerr << "Set/Get FullyConnected problem." << std::endl;
    return EXIT_FAILURE;
    }
  
  filter->SetForegroundValue( atoi(argv[4]) );
  if ( filter->GetForegroundValue( ) != atoi(argv[4]) )
    {
    std::cerr << "Set/Get ForegroundValue problem." << std::endl;
    return EXIT_FAILURE;
    }
  
  filter->SetBackgroundValue( atoi(argv[5]) );
  if ( filter->GetBackgroundValue( ) != atoi(argv[5]) )
    {
    std::cerr << "Set/Get BackgroundValue problem." << std::endl;
    return EXIT_FAILURE;
    }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
