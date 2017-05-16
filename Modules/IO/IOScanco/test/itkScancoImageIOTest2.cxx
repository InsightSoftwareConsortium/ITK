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

  std::cout << "Version: \t\t" << scancoIO->GetVersion() << std::endl;
  TEST_EXPECT_EQUAL(scancoIO->GetVersion(), std::string("CTDATA-HEADER_V1"));
  std::cout << "PatientIndex: \t" << scancoIO->GetPatientIndex() << std::endl;
  TEST_EXPECT_EQUAL(scancoIO->GetPatientIndex(), 78);
  std::cout << "ScannerID: \t\t" << scancoIO->GetScannerID() << std::endl;
  TEST_EXPECT_EQUAL(scancoIO->GetScannerID(), 2135);
  std::cout << "SliceThickness: \t" << scancoIO->GetSliceThickness() << std::endl;
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetSliceThickness(), 0.036, 6, 1e-3));
  std::cout << "SliceIncrement: \t" << scancoIO->GetSliceIncrement() << std::endl;
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetSliceIncrement(), 0.036, 6, 1e-3));
  std::cout << "StartPosition: \t" << scancoIO->GetStartPosition() << std::endl;
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetStartPosition(), 75.0, 6, 1e-3));
  std::cout << "DataRange[0]: \t" << scancoIO->GetDataRange()[0] << std::endl;
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetDataRange()[0], -2715.0, 6, 1e-3));
  std::cout << "DataRange[1]: \t" << scancoIO->GetDataRange()[1] << std::endl;
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(scancoIO->GetDataRange()[1], 32767.0, 6, 1e-3));

  TEST_EXPECT_TRUE(scancoIO->CanWriteFile(outputFileName.c_str()));

  TEST_EXPECT_TRUE(!scancoIO->CanWriteFile((outputFileName + ".exe").c_str()));

  scancoIO->SetFileName(outputFileName);
  scancoIO->WriteImageInformation();

  //// Generate test image
  // typedef itk::ImageFileWriter< ImageType > WriterType;
  // WriterType::Pointer writer = WriterType::New();
  ////IOType::Pointer metaOut = IOType::New();
  ////writer->SetImageIO(metaOut);
  // writer->SetInput( reader->GetOutput() );
  // writer->SetFileName( outputFileName );
  ////TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
