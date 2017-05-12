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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScancoImageIO.h"
#include "itkTestingMacros.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int
itkScancoImageIOTest2(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0] << " Input Output \n";
    return EXIT_FAILURE;
  }
  const std::string inputFileName = argv[1];
  const std::string outputFileName = argv[2];

  // ATTENTION THIS IS THE PIXEL TYPE FOR
  // THE RESULTING IMAGE
  const unsigned int                       Dimension = 3;
  typedef short                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer                     reader = ReaderType::New();

  // force use of ScancoIO
  typedef itk::ScancoImageIO IOType;
  IOType::Pointer            scancoIO = IOType::New();
  reader->SetImageIO(scancoIO);

  // read the file
  reader->SetFileName(inputFileName);
  TRY_EXPECT_NO_EXCEPTION(reader->Update());
  ImageType::Pointer image = reader->GetOutput();
  image->Print(std::cout);

  TEST_EXPECT_TRUE(scancoIO->CanWriteFile(outputFileName.c_str()));

  TEST_EXPECT_TRUE(!scancoIO->CanWriteFile((outputFileName + ".exe").c_str()));

  // Generate test image
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     writer = WriterType::New();
  // IOType::Pointer metaOut = IOType::New();
  // writer->SetImageIO(metaOut);
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(outputFileName);
  // TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
