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
#include "itkPNGImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkPNGImageIOTest2(int argc, char * argv[])
{
  if( argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " input output\n";
    return EXIT_FAILURE;
    }
  const unsigned int                      Dimension = 2;
  typedef unsigned char                   PixelType;
  typedef itk::Image<PixelType,Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typedef itk::ImageFileWriter<ImageType> WriterType;
  ImageType::Pointer                      readResult;
  ImageType::Pointer                      baseline;
  try
    {
    //
    // actually reading a RGBA image;
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(argv[1]);

    // Read in the image
    itk::PNGImageIO::Pointer io;
    io = itk::PNGImageIO::New();
    reader->SetImageIO(io);

    reader->Update();
    readResult = reader->GetOutput();
    //
    // writing grayscale image
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(readResult);
    writer->SetImageIO(io);
    writer->SetFileName(argv[2]);
    writer->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
