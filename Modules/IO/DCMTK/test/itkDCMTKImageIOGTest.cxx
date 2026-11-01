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
#include "gtest/gtest.h"
#include "itkDCMTKImageIO.h"
#include "itkImageFileReader.h"

#include "dcmtk/config/osconfig.h"
#include "dcmtk/dcmdata/dctk.h"

#include <string>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{

std::string
OutputPath(const std::string & name)
{
  return TOSTRING(ITK_TEST_OUTPUT_DIR) + "/" + name;
}

constexpr unsigned short ROWS = 4;
constexpr unsigned short COLUMNS = 8;
constexpr double         ROW_SPACING = 1.0;
constexpr double         COLUMN_SPACING = 3.0;

// Reports the first tag that could not be inserted, so a setup failure is not read as a spacing failure.
::testing::AssertionResult
WriteSingleFrameImage(const std::string & fileName)
{
  DcmFileFormat      fileFormat;
  DcmDataset * const dataset = fileFormat.getDataset();

  char uid[100];
  const struct
  {
    const DcmTagKey & key;
    const char *      value;
  } strings[] = {
    { DCM_SOPClassUID, UID_CTImageStorage },
    { DCM_SOPInstanceUID, dcmGenerateUniqueIdentifier(uid, SITE_INSTANCE_UID_ROOT) },
    { DCM_Modality, "CT" },
    { DCM_PatientName, "Anisotropic^Spacing" },
    { DCM_PatientID, "0" },
    { DCM_PhotometricInterpretation, "MONOCHROME2" },
    { DCM_PixelSpacing, "1\\3" },
    { DCM_ImageOrientationPatient, "1\\0\\0\\0\\1\\0" },
    { DCM_ImagePositionPatient, "0\\0\\0" },
    { DCM_SliceThickness, "2" },
  };
  for (const auto & tag : strings)
  {
    const OFCondition status = dataset->putAndInsertString(tag.key, tag.value);
    if (status.bad())
    {
      return ::testing::AssertionFailure() << tag.key.toString().c_str() << ": " << status.text();
    }
  }

  const struct
  {
    const DcmTagKey & key;
    Uint16            value;
  } uint16s[] = { { DCM_Rows, ROWS },        { DCM_Columns, COLUMNS }, { DCM_BitsAllocated, 16 },
                  { DCM_BitsStored, 16 },    { DCM_HighBit, 15 },      { DCM_PixelRepresentation, 1 },
                  { DCM_SamplesPerPixel, 1 } };
  for (const auto & tag : uint16s)
  {
    const OFCondition status = dataset->putAndInsertUint16(tag.key, tag.value);
    if (status.bad())
    {
      return ::testing::AssertionFailure() << tag.key.toString().c_str() << ": " << status.text();
    }
  }

  Sint16 pixels[ROWS * COLUMNS];
  for (unsigned int i = 0; i < ROWS * COLUMNS; ++i)
  {
    pixels[i] = static_cast<Sint16>(i);
  }
  const OFCondition pixelStatus =
    dataset->putAndInsertUint16Array(DCM_PixelData, reinterpret_cast<Uint16 *>(pixels), ROWS * COLUMNS);
  if (pixelStatus.bad())
  {
    return ::testing::AssertionFailure() << DCM_PixelData.toString().c_str() << ": " << pixelStatus.text();
  }

  const OFCondition saveStatus = fileFormat.saveFile(fileName.c_str(), EXS_LittleEndianExplicit);
  if (saveStatus.bad())
  {
    return ::testing::AssertionFailure() << fileName << ": " << saveStatus.text();
  }
  return ::testing::AssertionSuccess();
}

} // namespace

// Unequal in-plane spacing keeps its column/row order when Spacing Between Slices is absent.
TEST(DCMTKImageIO, InPlaneSpacingOrderWithoutSpacingBetweenSlices)
{
  const std::string fileName = OutputPath("itkDCMTKImageIOAnisotropicSpacing.dcm");
  ASSERT_TRUE(WriteSingleFrameImage(fileName));

  using ImageType = itk::Image<short, 3>;
  const auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetFileName(fileName);
  reader->SetImageIO(itk::DCMTKImageIO::New());
  ASSERT_NO_THROW(reader->Update());

  const ImageType * const image = reader->GetOutput();
  EXPECT_EQ(image->GetLargestPossibleRegion().GetSize()[0], COLUMNS);
  EXPECT_EQ(image->GetLargestPossibleRegion().GetSize()[1], ROWS);
  EXPECT_DOUBLE_EQ(image->GetSpacing()[0], COLUMN_SPACING);
  EXPECT_DOUBLE_EQ(image->GetSpacing()[1], ROW_SPACING);
}
