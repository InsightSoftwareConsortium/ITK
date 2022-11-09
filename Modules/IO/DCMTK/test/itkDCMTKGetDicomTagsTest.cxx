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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkDCMTKFileReader.h"
#include "itkTestingMacros.h"

#include <fstream>


int
itkDCMTKGetDicomTagsTest(int argc, char * argv[])
{

  if (argc != 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " DicomImage outTagDataFileName" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;
  using InputPixelType = unsigned short;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  itk::DCMTKFileReader fileReader;
  fileReader.SetFileName(argv[1]);
  fileReader.LoadFile();

  std::string   outTagDataFileName = argv[2];
  std::ofstream outputFile(outTagDataFileName, std::ios::out);
  if (!outputFile.is_open())
  {
    std::cerr << "Unable to open output file: " << outTagDataFileName << std::endl;
    return EXIT_FAILURE;
  }

  bool throwException = false;

  std::string strTarget;

  // Study Date
  unsigned short group = 0x0008;
  unsigned short element = 0x0021;
  if (fileReader.GetElementDA(group, element, strTarget, throwException) == EXIT_SUCCESS)
  {
    // YYYYMMDD
    outputFile << strTarget << std::endl;
    std::string expectedVal = "20030625";
    ITK_TRY_EXPECT_EQUAL(strTarget, expectedVal);
  }

  // Study Time
  group = 0x0008;
  element = 0x0030;
  // seq.GetElementTM(group, element, tag)
  if (fileReader.GetElementTM(group, element, strTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << strTarget << std::endl;
    std::string expectedVal = "152734";
    ITK_TRY_EXPECT_EQUAL(strTarget, expectedVal);
  }

  // Patient Name
  group = 0x0010;
  element = 0x0010;
  if (fileReader.GetElementPN(group, element, strTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << strTarget << std::endl;
    std::string expectedVal = "Wes Turner";
    ITK_TRY_EXPECT_EQUAL(strTarget, expectedVal);
  }

  // Patient Sex
  group = 0x0010;
  element = 0x0040;
  if (fileReader.GetElementCS(group, element, strTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << strTarget << std::endl;
    std::string expectedVal = "O";
    ITK_TRY_EXPECT_EQUAL(strTarget, expectedVal);
  }

  // Patient Weight
  group = 0x0010;
  element = 0x1030;
  if (fileReader.GetElementDS(group, element, 1, strTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << strTarget << std::endl;
    std::string expectedVal = "68.039";
    ITK_TRY_EXPECT_EQUAL(strTarget, expectedVal);
  }

  // Echo Number
  group = 0x0018;
  element = 0x0086;
  itk::int32_t int32Target;
  if (fileReader.GetElementIS(group, element, int32Target, throwException) == EXIT_SUCCESS)
  {
    outputFile << int32Target << std::endl;
    itk::int32_t expectedVal = 12576; // Expect (12576)"1"
    ITK_TRY_EXPECT_EQUAL(int32Target, expectedVal);
  }

  // Acquisition Duration
  group = 0x0019;
  element = 0x105a;
  float floatTarget;
  if (fileReader.GetElementFL(group, element, floatTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << floatTarget << std::endl;
    float expectedVal = 1304791694;
    ITK_TRY_EXPECT_EQUAL(floatTarget, expectedVal);
  }

  // Locs per 3D slab
  group = 0x0021;
  element = 0x1057;
  if (fileReader.GetElementSL(group, element, int32Target, throwException) == EXIT_SUCCESS)
  {
    outputFile << int32Target << std::endl;
    itk::int32_t expectedVal = 124; // Expect (124)"|"
    ITK_TRY_EXPECT_EQUAL(int32Target, expectedVal);
  }

  // Samples per Pixel
  group = 0x0028;
  element = 0x0002;
  if (fileReader.GetElementUS(group, element, usTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << usTarget << std::endl;
    unsigned short expectedVal = 1; // for scalar; 3 for RGB
    ITK_TRY_EXPECT_EQUAL(usTarget, expectedVal);
  }

  // Photometric Interpretation
  group = 0x0028;
  element = 0x0004;
  if (fileReader.GetElementCS(group, element, strTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << strTarget << std::endl;
    unsigned short expectedVal = "????";
    ITK_TRY_EXPECT_EQUAL(strTarget, expectedVal);
  }

  // Rows
  group = 0x0028;
  element = 0x0010;
  unsigned short usTarget;
  if (fileReader.GetElementUS(group, element, usTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << usTarget << std::endl;
    unsigned short expectedVal = 256; // Expect(256)""
    ITK_TRY_EXPECT_EQUAL(usTarget, expectedVal);
  }

  // Bits Allocated
  group = 0x0028;
  element = 0x0100;
  if (fileReader.GetElementUS(group, element, usTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << usTarget << std::endl;
    unsigned short expectedVal = 8;
    ITK_TRY_EXPECT_EQUAL(usTarget, expectedVal);
  }

  // Unique Image Identifier
  group = 0x0043;
  element = 0x1028;
  if (fileReader.GetElementOB(group, element, strTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << strTarget << std::endl;
    std::string expectedVal = "MR Recon <0>";
    ITK_TRY_EXPECT_EQUAL(strTarget, expectedVal);
  }

  // Scanner Study Entity UID
  group = 0x0043;
  element = 0x1061;
  if (fileReader.GetElementUI(group, element, strTarget, throwException) == EXIT_SUCCESS)
  {
    outputFile << strTarget << std::endl;
    std::string expectedVal = "1.2.840.113619.2.133.1762890640.1886.1055165015.961";
    ITK_TRY_EXPECT_EQUAL(strTarget, expectedVal);
  }

  // Image Pixel Type
  IOPixelEnum pixelType = fileReader.GetImagePixelType();
  outputFile << pixelType << std::endl;
  unsigned short expectedVal = IOPixelEnum::RGB;
  ITK_TRY_EXPECT_EQUAL(pixelType, expectedVal);

  outputFile.close();


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
