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

#include "itkDCMTKImageIO.h"
#include "itkMetaDataObject.h"
#include "itkTestingMacros.h"

#include <iostream>

// Tests that DCMTKImageIO::ReadImageInformation() populates the metadata
// dictionary with DICOM tag values using "GGGG|EEEE" keys (lowercase
// matching GDCMImageIO behavior). Expected values are verified against
// Input/DicomSeries/Image0075.dcm using the same fixture as
// itkDCMTKGetDicomTagsTest.
int
itkDCMTKImageIOMetadataTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <DicomImage>" << std::endl;
    return EXIT_FAILURE;
  }

  auto dcmtkIO = itk::DCMTKImageIO::New();

  ITK_TEST_EXPECT_TRUE(dcmtkIO->CanReadFile(argv[1]));
  dcmtkIO->SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(dcmtkIO->ReadImageInformation());

  const itk::MetaDataDictionary & dict = dcmtkIO->GetMetaDataDictionary();

  // Dictionary must be populated
  ITK_TEST_EXPECT_TRUE(!dict.GetKeys().empty());

  // Pixel data (7FE0,0010) must NOT appear in the dictionary
  ITK_TEST_EXPECT_TRUE(!dict.HasKey("7fe0|0010"));

  // Verify string-valued DICOM tags using the "GGGG|EEEE" key format.
  // Values are specific to the test file Input/DicomSeries/Image0075.dcm
  std::string value;

  // (0008,0021) DA  StudyDate
  ITK_TEST_EXPECT_TRUE(itk::ExposeMetaData<std::string>(dict, "0008|0021", value));
  ITK_TEST_EXPECT_EQUAL(value, std::string("20030625"));

  // (0010,0010) PN  PatientName
  ITK_TEST_EXPECT_TRUE(itk::ExposeMetaData<std::string>(dict, "0010|0010", value));
  ITK_TEST_EXPECT_EQUAL(value, std::string("Wes Turner"));

  // (0010,0040) CS  PatientSex
  ITK_TEST_EXPECT_TRUE(itk::ExposeMetaData<std::string>(dict, "0010|0040", value));
  ITK_TEST_EXPECT_EQUAL(value, std::string("O"));

  // (0028,0004) CS  PhotometricInterpretation
  ITK_TEST_EXPECT_TRUE(itk::ExposeMetaData<std::string>(dict, "0028|0004", value));
  ITK_TEST_EXPECT_EQUAL(value, std::string("MONOCHROME2"));

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
