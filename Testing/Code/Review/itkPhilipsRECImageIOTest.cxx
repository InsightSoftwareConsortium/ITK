/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhilipsRECImageIOTest.cxx
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
#include "itkImage.h"

#include "itkPhilipsRECImageIOFactory.h"
#include "itkPhilipsRECImageIO.h"

int itkPhilipsRECImageIOTest( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage" << std::endl;
    return EXIT_FAILURE;
    }
 
  typedef unsigned short                      PixelType;
  typedef itk::Image< PixelType, 2 >          ImageType;

  typedef itk::ImageFileReader< ImageType >   ReaderType;
  typedef itk::ImageFileWriter< ImageType >   WriterType;

  itk::PhilipsRECImageIOFactory::RegisterOneFactory();

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );
 
  writer->SetInput( reader->GetOutput() );

  try
    {
    writer->Update(); 
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
