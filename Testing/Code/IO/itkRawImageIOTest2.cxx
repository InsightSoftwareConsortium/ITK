/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIOTest2.cxx
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
#include "itkTextOutput.h"
#include "itkRawImageIO.h"
#include "itkImageFileReader.h"
#include "itkRGBPixel.h"
#include "itkImage.h"


int itkRawImageIOTest2(int argc, char * argv [])
{

  if ( argc < 2 )
    {
    itkGenericOutputMacro(<<"Need a file to process");
    return EXIT_FAILURE;
    }

  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());
  // Uncomment the following if you want to see each message independently
  // itk::OutputWindow::GetInstance()->PromptUserOn();

  // We are reading a RGB pixel
  typedef itk::RGBPixel<unsigned char> RGBPixelType;

  // Create a source object (in this case a reader)
  itk::RawImageIO<RGBPixelType>::Pointer io;
  io = itk::RawImageIO<RGBPixelType>::New();
  io->SetFileName(argv[1]);
  io->SetFileDimensionality(3);
  io->SetNumberOfDimensions(3);
  unsigned int dim[3] = {50,50,10};
  double spacing[3] = {1.0, 1.0, 1.0};
  double origin[3] = {0.0,0.0,0.0};
  for(unsigned int i=0; i<3; i++)
    {
    io->SetDimensions(i,dim[i]);
    io->SetSpacing(i,spacing[i]);
    io->SetOrigin(i,origin[i]);
    }
  io->SetHeaderSize(0);
  io->SetImageMask(0x7fff);
  io->SetByteOrderToLittleEndian();
  io->SetPixelType(itk::ImageIOBase::RGB);
  io->SetComponentType(itk::ImageIOBase::UCHAR);
  io->SetNumberOfComponents(3);

  std::cout << "IO: " << io << std::endl;

  typedef itk::Image<RGBPixelType,3> RGBImage3DType;
  itk::ImageFileReader<RGBImage3DType>::Pointer reader;
  reader = itk::ImageFileReader<RGBImage3DType>::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);
  reader->Update();

  reader->GetOutput()->Print( std::cout );

  return EXIT_SUCCESS;
}



