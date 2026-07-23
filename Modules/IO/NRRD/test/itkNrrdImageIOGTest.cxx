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

#include "itkNrrdImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMetaDataObject.h"
#include "itkGTest.h"
#include <fstream>

namespace
{
using ImageType = itk::Image<unsigned char, 3>;

std::string
WriteFile(const std::string & path, const std::string & content)
{
  std::ofstream file(path, std::ios::binary);
  file << content;
  return path;
}

ImageType::Pointer
ReadNrrd(const std::string & path)
{
  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetFileName(path);
  reader->SetImageIO(itk::NrrdImageIO::New());
  reader->Update();
  return reader->GetOutput();
}

std::string
WriteRaw(const std::string & path, const std::size_t numberOfBytes)
{
  std::ofstream     file(path, std::ios::binary);
  const std::string zeros(numberOfBytes, '\0');
  file << zeros;
  return path;
}
} // namespace

// A 16-dimensional header whose sizes line holds a 17th token previously wrote one
// element past a 16-element stack array in the vendored parser (issue #6575, B67).
TEST(NrrdImageIO, RejectsExcessSizesAtMaximumDimension)
{
  const std::string dir = ::testing::TempDir();
  const std::string header = WriteFile(dir + "/b67.nhdr",
                                       "NRRD0004\n"
                                       "type: uchar\n"
                                       "dimension: 16\n"
                                       "sizes: 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1\n"
                                       "encoding: raw\n"
                                       "data file: b67.raw\n");
  WriteRaw(dir + "/b67.raw", 1);

  auto io = itk::NrrdImageIO::New();
  io->SetFileName(header);
  EXPECT_THROW(io->ReadImageInformation(), itk::ExceptionObject);
}

// nrrdOriginCalculate tested axis 0's min for every axis, so a usable first-axis min
// with NaN mins elsewhere produced a NaN origin (issue #6575, B68).
TEST(NrrdImageIO, PartialAxisMinsYieldFiniteOrigin)
{
  const std::string dir = ::testing::TempDir();
  const std::string header = WriteFile(dir + "/b68.nhdr",
                                       "NRRD0004\n"
                                       "type: uchar\n"
                                       "dimension: 3\n"
                                       "sizes: 2 2 2\n"
                                       "spacings: 1 1 1\n"
                                       "axis mins: 0 nan nan\n"
                                       "encoding: raw\n"
                                       "data file: b68.raw\n");
  WriteRaw(dir + "/b68.raw", 8);

  const auto image = ReadNrrd(header);
  for (unsigned int i = 0; i < 3; ++i)
  {
    EXPECT_TRUE(std::isfinite(image->GetOrigin()[i])) << " axis " << i;
  }
}

// The `none` arm of the kinds parser assigned the axis's centering instead of its kind,
// clobbering a previously parsed centers line (issue #6575, B69).
TEST(NrrdImageIO, KindsNoneDoesNotClobberCentering)
{
  const std::string dir = ::testing::TempDir();
  const std::string header = WriteFile(dir + "/b69.nhdr",
                                       "NRRD0004\n"
                                       "type: uchar\n"
                                       "dimension: 3\n"
                                       "sizes: 2 2 2\n"
                                       "centerings: cell cell cell\n"
                                       "kinds: none domain domain\n"
                                       "encoding: raw\n"
                                       "data file: b69.raw\n");
  WriteRaw(dir + "/b69.raw", 8);

  const auto  image = ReadNrrd(header);
  std::string centering;
  ASSERT_TRUE(itk::ExposeMetaData(image->GetMetaDataDictionary(), "NRRD_centerings[0]", centering));
  EXPECT_EQ(centering, "cell");
}

// Reserved-key dispatch matched by prefix and ignored its result: an unmatched NRRD_
// key was silently dropped on a read-write round trip (issue #6575, B63).
TEST(NrrdImageIO, UnmatchedReservedKeySurvivesRoundTrip)
{
  const std::string dir = ::testing::TempDir();
  auto              image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(2) });
  image->Allocate();
  image->FillBuffer(0);
  itk::EncapsulateMetaData<int>(image->GetMetaDataDictionary(), "NRRD_pixel_original_axis", 2);

  const std::string file = dir + "/b63.nrrd";
  auto              writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetFileName(file);
  writer->SetInput(image);
  writer->SetImageIO(itk::NrrdImageIO::New());
  writer->Update();
  const auto restored = ReadNrrd(file);

  std::string value;
  EXPECT_TRUE(itk::ExposeMetaData(restored->GetMetaDataDictionary(), "NRRD_pixel_original_axis", value));
}

// A custom key whose prefix is a per-axis reserved field followed by a valid "[axis]"
// suffix plus trailing text (e.g. NRRD_kinds[0]_vendor) was consumed as reserved and
// silently dropped; exact-suffix dispatch must let it survive the round trip (B63).
TEST(NrrdImageIO, PerAxisPrefixedCustomKeySurvivesRoundTrip)
{
  const std::string dir = ::testing::TempDir();
  auto              image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, ImageType::SizeType::Filled(2) });
  image->Allocate();
  image->FillBuffer(0);
  itk::EncapsulateMetaData<std::string>(image->GetMetaDataDictionary(), "NRRD_kinds[0]_vendor", "acme");

  const std::string file = dir + "/b63-peraxis.nrrd";
  auto              writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetFileName(file);
  writer->SetInput(image);
  writer->SetImageIO(itk::NrrdImageIO::New());
  writer->Update();
  const auto restored = ReadNrrd(file);

  std::string value;
  EXPECT_TRUE(itk::ExposeMetaData(restored->GetMetaDataDictionary(), "NRRD_kinds[0]_vendor", value));
}
