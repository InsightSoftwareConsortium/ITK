/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPNGImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "itkPNGImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"

int itkPNGImageIOTest(int argc, char * argv[])
{
  // This test is usually run with the data file
  // Insight/Testing/Data/Input/cthead1.png
  if( argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " filename\n";
    return 1;
    }

  // We are converting read data into RGB pixel image
  typedef itk::RGBPixel<unsigned char> RGBPixelType;
  typedef itk::Image<RGBPixelType,2> RGBImageType;

  // Read in the image
  itk::PNGImageIO::Pointer io;
  io = itk::PNGImageIO::New();

  itk::ImageFileReader<RGBImageType>::Pointer reader;
  reader = itk::ImageFileReader<RGBImageType>::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);
  reader->Update();

  itk::ImageFileWriter<RGBImageType>::Pointer writer;
  writer = itk::ImageFileWriter<RGBImageType>::New();
  writer->SetInput(reader->GetOutput());
  writer->SetFileName("junk.png");
  writer->SetImageIO(io);
  writer->Write();

  return EXIT_SUCCESS;
}
