/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIOTest2.cxx
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
#include "itkOutputWindow.h"
//#include "itkRawImageIO.h"
#include "itkFileIOToImageFilter.h"
#include "itkRGBPixel.h"
#include "itkImage.h"

// this class is used to send output to stdout and not the itk window
class TextOutput : public itk::OutputWindow
{
public:
  typedef itk::SmartPointer<TextOutput> Pointer;
  itkNewMacro(TextOutput);
  virtual void DisplayText(const char* s)
    {
      std::cout << s << std::endl;
    }
};

int main(int argc, char *argv[])
{
  if ( argc < 2 )
    {
    itkGenericOutputMacro(<<"Need a file to process");
    return 1;
    }
#if 0

  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(TextOutput::New());
  // Uncomment the following if you want to see each message independently
  // itk::OutputWindow::GetInstance()->PromptUserOn();

  // We are reading a RGB pixel
  typedef itk::RGBPixel<unsigned char> RGBPixelType;

  // Create a source object (in this case a reader)
  itk::RawImageIO<RGBPixelType>::Pointer io;
  io = itk::RawImageIO<RGBPixelType>::New();
  io->SetFilePrefix(argv[1]);
  unsigned int dim[3] = {570,670,1};
  io->SetDimensions(dim);
  double spacing[3] = {0.8, 0.8, 1.5};
  io->SetSpacing(spacing);
  double origin[3] = {0.0,0.0,0.0};
  io->SetOrigin(origin);
  io->SetHeaderSize(0);
  io->SetImageMask(0x7fff);
  io->SetImageByteOrderToLittleEndian();
  io->SetPixelType(itk::ITK_UCHAR);
  io->SetNumberOfComponents(3);
  std::cout << "IO: " << io << std::endl;
  typedef itk::Image<RGBPixelType,2> RGBImage2DType;
  itk::FileIOToImageFilter<RGBImage2DType>::Pointer reader;
  reader = itk::FileIOToImageFilter<RGBImage2DType>::New();
  reader->SetFileName(argv[1]);
  reader->SetIO(io);
  reader->Update();
#endif

  return EXIT_SUCCESS;
}



