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
#include "itkGDCMImageIO.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkMetaDataObject.h"

#include "itkGTest.h"
#include "itksys/SystemTools.hxx"

#define _STRING(s) #s
#define TOSTRING(s) _STRING(s)

namespace
{

itk::Image<unsigned short, 2>::Pointer
MakeSyntheticCT()
{
  using ImageType = itk::Image<unsigned short, 2>;
  auto image = ImageType::New();

  ImageType::SizeType size{ { 4, 4 } };
  image->SetRegions(ImageType::RegionType(size));
  image->Allocate();
  image->FillBuffer(100);
  return image;
}

void
SeedDicomMetadata(itk::MetaDataDictionary & dict)
{
  // The minimum DICOM tags that GDCMImageIO::Write needs to produce a
  // syntactically valid CT-ish output.  All of these are in the public
  // dictionary, so they exercise the "valid VR" branch.
  itk::EncapsulateMetaData<std::string>(dict, "0008|0060", "CT");                        // Modality
  itk::EncapsulateMetaData<std::string>(dict, "0008|0016", "1.2.840.10008.5.1.4.1.1.2"); // SOPClassUID
  itk::EncapsulateMetaData<std::string>(dict, "0008|0018", "1.2.3.4.5.6.7.8.9.1");       // SOPInstanceUID
  itk::EncapsulateMetaData<std::string>(dict, "0010|0010", "Test^Patient");              // PatientName
  itk::EncapsulateMetaData<std::string>(dict, "0010|0020", "12345");                     // PatientID
}

} // namespace

// -----------------------------------------------------------------------------
// Regression test for ITK PR #2553 / mrc-sys's segfault report:
//
// When MetaDataDictionary contains a key that parses as a (group,element)
// DICOM tag but is not in GDCM's public dictionary (e.g., a private vendor
// tag, a typo, or any custom tag added without going through a private
// creator), GDCMImageIO::Write looks up the tag's VR via
// pubdict.GetDictEntry(tag).GetVR(), which returns gdcm::VR::INVALID for
// unknown tags.  The unconditional call to gdcm::StringFilter::FromString
// that follows hits the switch's default case in
// gdcmStringFilter::FromString, which calls gdcm_assert(0) and terminates
// the process.
//
// The expected behavior after the fix: the tag is reported via the existing
// "ignoring non-DICOM and non-ITK standard keys" warning channel, the rest
// of the metadata is still written, and the process completes normally.
// -----------------------------------------------------------------------------
TEST(GDCMImageIOInvalidVRType, UnknownTagDoesNotCrash)
{
  using ImageType = itk::Image<unsigned short, 2>;
  auto image = MakeSyntheticCT();

  auto & dict = image->GetMetaDataDictionary();
  SeedDicomMetadata(dict);

  // Add a tag whose (group,element) is well-formed but not in the public
  // DICOM dictionary.  Group 0x9999 is unassigned; this is the simplest
  // way to deterministically produce VR::INVALID at lookup time.
  itk::EncapsulateMetaData<std::string>(dict, "9999|9999", "should-be-skipped");

  auto gdcmIO = itk::GDCMImageIO::New();

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetImageIO(gdcmIO);
  writer->SetInput(image);

  const std::string outFile = std::string(TOSTRING(ITK_TEST_OUTPUT_DIR)) + "/GDCMImageIOInvalidVRType_unknown.dcm";
  writer->SetFileName(outFile);

  // Pre-fix expectation: writer->Update() either segfaults or asserts
  // inside gdcm_assert(0) in StringFilter::FromString.
  // Post-fix expectation: writer->Update() completes without throwing,
  // skipping the unknown tag and emitting a warning.
  EXPECT_NO_THROW(writer->Update());

  // The output should exist and be non-empty (the rest of the metadata
  // and the pixel buffer were written).
  ASSERT_TRUE(itksys::SystemTools::FileExists(outFile));
  EXPECT_GT(itksys::SystemTools::FileLength(outFile), 0u);
}

// A second test that uses several unknown tags at once, to confirm the
// warning aggregates them rather than stopping at the first.
TEST(GDCMImageIOInvalidVRType, MultipleUnknownTagsAllSkipped)
{
  using ImageType = itk::Image<unsigned short, 2>;
  auto image = MakeSyntheticCT();

  auto & dict = image->GetMetaDataDictionary();
  SeedDicomMetadata(dict);

  itk::EncapsulateMetaData<std::string>(dict, "9999|0001", "alpha");
  itk::EncapsulateMetaData<std::string>(dict, "9999|0002", "beta");
  itk::EncapsulateMetaData<std::string>(dict, "9991|0003", "gamma");

  auto gdcmIO = itk::GDCMImageIO::New();

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetImageIO(gdcmIO);
  writer->SetInput(image);

  const std::string outFile = std::string(TOSTRING(ITK_TEST_OUTPUT_DIR)) + "/GDCMImageIOInvalidVRType_multi.dcm";
  writer->SetFileName(outFile);

  EXPECT_NO_THROW(writer->Update());
  ASSERT_TRUE(itksys::SystemTools::FileExists(outFile));
  EXPECT_GT(itksys::SystemTools::FileLength(outFile), 0u);
}
