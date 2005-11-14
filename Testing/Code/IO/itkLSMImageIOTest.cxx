/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLSMImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkLSMImageIO.h"
#include "itkImage.h"

int itkLSMImageIOTest(int argc, char* argv[])
{
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " LSM.lsm OutputImage.lsm\n";
    return EXIT_FAILURE;
    }

  typedef unsigned char InputPixelType;
  typedef itk::Image< InputPixelType, 2 > InputImageType;
  typedef itk::ImageFileReader< InputImageType > ReaderType;
  typedef itk::LSMImageIO ImageIOType;

  const char *filename = argv[1];
  const char *outfilename = argv[2];

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( filename );

  ImageIOType::Pointer lsmImageIO = ImageIOType::New();
  reader->SetImageIO( lsmImageIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

  //
  typedef itk::ImageFileWriter< InputImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outfilename );
  writer->SetInput( reader->GetOutput() );
  writer->SetImageIO( lsmImageIO );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

  lsmImageIO->Print(std::cout);

  return EXIT_SUCCESS;
}

