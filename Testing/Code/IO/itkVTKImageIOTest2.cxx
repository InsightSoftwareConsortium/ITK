/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageIOTest2.cxx
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

// some compilers have trouble with the size of this test
#define ITK_LEAN_AND_MEAN

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkVTKImageIO.h"

int itkVTKImageIOTest2(int argc, char* argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  output1 output2 " << std::endl;
    return EXIT_FAILURE;
    }


  const unsigned int Dimension = 3;

  // Testing to write images of vectors as VTK images.

  typedef float                                            PixelComponentType;
  typedef itk::Vector< PixelComponentType, Dimension >     PixelType;
  typedef itk::Image< PixelType, Dimension >               ImageType;
  typedef itk::ImageFileReader< ImageType >                ReaderType;
  typedef itk::ImageFileWriter< ImageType >                WriterType;
  
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
