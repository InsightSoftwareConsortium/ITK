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

#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"
#include "itkFileListVideoIOFactory.h"

int itkVideoFileReaderWriterTest( int argc, char *argv[] )
{
  // Check parameters
  if (argc != 7)
    {
    std::cerr << "Usage: [Video Input] [Image Output]" << std::endl;
    return EXIT_FAILURE;
    }

  // Instantiate a new reader
  typedef itk::RGBPixel<uint8_t> PixelType;
  const unsigned int NumberOfDimensions =             2;
  typedef itk::Image< PixelType, NumberOfDimensions > FrameType;
  typedef itk::VideoStream< FrameType >               VideoType;
  typedef itk::VideoFileReader< VideoType >           VideoReaderType;
  typedef itk::VideoFileWriter< VideoType >           VideoWriterType;

  std::string inFile = "";
  for( int i = 1; i <= 5; ++i )
    {
    inFile = inFile + std::string(argv[i]);
    if( i != 5 )
      {
      inFile = inFile + std::string(",");
      }
    }
  VideoReaderType::Pointer reader = VideoReaderType::New();
  reader->SetFileName(inFile.c_str() );

  // I'm still not sure how to handle this right, but for now, just manually
  // register an FileListVideoIO
  itk::ObjectFactoryBase::RegisterFactory( itk::FileListVideoIOFactory::New() );

  // Set up the writer
  VideoWriterType::Pointer writer = VideoWriterType::New();
  writer->SetInput(reader->GetOutput() );
  writer->SetFileName(argv[6]);

  // Call Update on the writer to process the entire video
  writer->Update();

  return EXIT_SUCCESS;
}
