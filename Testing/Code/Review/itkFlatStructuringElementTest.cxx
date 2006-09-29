/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFlatStructuringElementTest.cxx
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
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"
#include "itkFlatStructuringElement.h"

int itkFlatStructuringElementTest(int argn, char * argv[])
{

  if( argn < 4 )
    {
    std::cerr << "usage: kernelShape fileName radius type [lines|img]" << std::endl;
    std::cerr << "  type: 0 -> Box" << std::endl;
    std::cerr << "        1 -> Ball" << std::endl;
    std::cerr << "        2 -> FromImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;
  
  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::FlatStructuringElement< ImageType, Dimension > 
                                             StructuringElementType;

  StructuringElementType::RadiusType Rad;

  Rad.Fill( atoi( argv[2] ) );
  
  int type = atoi( argv[3] );
  
  StructuringElementType K;
  
  if( type == 0 )
    {
    K = StructuringElementType::Box( Rad );
    }
  else if( type == 1 )
    {
    K = StructuringElementType::Ball( Rad );
    }
  else if( type == 2 )
    {
    typedef itk::ImageFileReader< ImageType > ReaderType;
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( argv[4] );
    reader->Update();
    K = itk::FlatStructuringElement<ImageType,Dimension>::FromImage( 
                                             reader->GetOutput() );
    }
  else
    {
    return EXIT_FAILURE;
    }

  ImageType::Pointer kernelImage = K.GetImage();

  kernelImage->Print( std::cout );

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( kernelImage );
  writer->SetFileName( argv[1] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught ! " << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

