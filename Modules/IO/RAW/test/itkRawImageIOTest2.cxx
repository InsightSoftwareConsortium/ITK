/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkTextOutput.h"
#include "itkRawImageIO.h"
#include "itkImageFileReader.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST


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
