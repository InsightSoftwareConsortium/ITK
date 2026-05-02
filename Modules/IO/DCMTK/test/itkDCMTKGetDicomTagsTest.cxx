/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkDCMTKFileReader.h"
#include "itkIntTypes.h"
#include "itkTestingMacros.h"

#include <fstream>
#include <iostream>


// Exercises every well-formed VR-typed accessor on itk::DCMTKFileReader against
// a known fixture (Input/DicomSeries/Image0075.dcm).  Expected values were
// verified out-of-band with pydicom; see PR description for the discovery
// procedure.  Correctness is asserted in-process via ITK_TEST_EXPECT_EQUAL;
// argv[2] receives one tag value per line for diagnostic inspection only.
int
itkDCMTKGetDicomTagsTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <DicomImage> <outTagDataFileName>" << std::endl;
    return EXIT_FAILURE;
  }

  itk::DCMTKFileReader fileReader;
  fileReader.SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(fileReader.LoadFile());

  std::ofstream outputFile(argv[2], std::ios::out);
  if (!outputFile.is_open())
  {
    std::cerr << "Unable to open output file: " << argv[2] << std::endl;
    return EXIT_FAILURE;
  }

  // Per-call: throwException=false so a missing tag returns EXIT_FAILURE
  // and we surface a test failure instead of an exception.
  constexpr bool throwException = false;

  // (0008,0021) DA  StudyDate
  {
    std::string actual;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementDA(0x0008, 0x0021, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, std::string("20030625"));
  }

  // (0008,0030) TM  StudyTime
  {
    std::string actual;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementTM(0x0008, 0x0030, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, std::string("152734"));
  }

  // (0010,0010) PN  PatientName
  {
    std::string actual;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementPN(0x0010, 0x0010, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, std::string("Wes Turner"));
  }

  // (0010,0040) CS  PatientSex
  {
    std::string actual;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementCS(0x0010, 0x0040, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, std::string("O"));
  }

  // (0010,1030) DS  PatientWeight (single-element decimal-string)
  {
    std::string actual;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementDS(0x0010, 0x1030, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, std::string("68.039"));
  }

  // (0018,0086) IS  EchoNumber
  {
    itk::int32_t actual = 0;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementIS(0x0018, 0x0086, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, itk::int32_t{ 1 });
  }

  // (0019,105a) FL  AcquisitionDuration (private tag, in GE private dictionary)
  {
    float actual = 0.0F;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementFL(0x0019, 0x105A, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, 414273984.0F);
  }

  // (0021,1057) SL  LocsPer3DSlab (private tag)
  {
    itk::int32_t actual = 0;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementSL(0x0021, 0x1057, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, itk::int32_t{ 124 });
  }

  // (0028,0002) US  SamplesPerPixel
  {
    unsigned short actual = 0;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementUS(0x0028, 0x0002, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, static_cast<unsigned short>(1));
  }

  // (0028,0004) CS  PhotometricInterpretation
  {
    std::string actual;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementCS(0x0028, 0x0004, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, std::string("MONOCHROME2"));
  }

  // (0028,0010) US  Rows
  {
    unsigned short actual = 0;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementUS(0x0028, 0x0010, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, static_cast<unsigned short>(256));
  }

  // (0028,0100) US  BitsAllocated
  {
    unsigned short actual = 0;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementUS(0x0028, 0x0100, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, static_cast<unsigned short>(16));
  }

  // (0043,1061) UI  ScannerStudyEntityUID (GE private; in the DCMTK private
  // dictionary so VR resolves to UI)
  {
    std::string actual;
    ITK_TEST_EXPECT_EQUAL(fileReader.GetElementUI(0x0043, 0x1061, actual, throwException), EXIT_SUCCESS);
    outputFile << actual << std::endl;
    ITK_TEST_EXPECT_EQUAL(actual, std::string("1.2.840.113619.2.133.1762890640.1886.1055165015.961"));
  }

  outputFile.close();
  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
