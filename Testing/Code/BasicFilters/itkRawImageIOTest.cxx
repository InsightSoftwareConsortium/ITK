/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkRawImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include <iostream>
#include "itkOutputWindow.h"
#include "itkRawImageIO.h"
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

  typedef itk::Image<RGBPixelType,2> RGBImage2DType;
  itk::FileIOToImageFilter<RGBImage2DType>::Pointer reader;
  reader = itk::FileIOToImageFilter<RGBImage2DType>::New();
  reader->SetFileName(argv[1]);
  reader->SetIO(io);
  reader->Update();

  return EXIT_SUCCESS;
}



