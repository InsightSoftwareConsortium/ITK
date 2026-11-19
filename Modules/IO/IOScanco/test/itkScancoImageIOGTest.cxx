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
#include "itkGTest.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMetaDataObject.h"
#include "itkScancoDataManipulation.h"
#include "itkScancoImageIO.h"

#include <cstdint>
#include <cstring>
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>
#include <vector>

#define SCANCO_STRINGIFY(s) #s
#define SCANCO_TOSTRING(s) std::string(SCANCO_STRINGIFY(s))

namespace
{
using ImageType = itk::Image<short, 3>;
using Bytes = std::vector<char>;

constexpr size_t BlockSize = ScancoHeaderBlockSize;

// AIM v020: a pre-header of five 4-byte lengths, then the image structure
constexpr size_t AIMV020PreHeaderSize = 20;
constexpr size_t AIMV020StructSize = 140;
constexpr size_t AIMV020StructLengthPosition = 4;
constexpr size_t AIMV020LogLengthPosition = 8;
constexpr size_t AIMV020DataLengthPosition = 12;
constexpr size_t AIMV020TypeOffset = 20;
constexpr size_t AIMV020DimensionOffset = 36;
constexpr size_t AIMV020ElementSizeOffset = 108;

// AIM v030: a version string, a pre-header of five 8-byte lengths, then the image structure
constexpr size_t AIMV030VersionSize = 16;
constexpr size_t AIMV030PreHeaderSize = 40;
constexpr size_t AIMV030StructSize = 224;
constexpr size_t AIMV030StructLengthPosition = AIMV030VersionSize + 8;
constexpr size_t AIMV030LogLengthPosition = AIMV030VersionSize + 16;
constexpr size_t AIMV030DataLengthPosition = AIMV030VersionSize + 24;
constexpr size_t AIMV030TypeOffset = 12;
constexpr size_t AIMV030DimensionOffset = 40;
constexpr size_t AIMV030ElementSizeOffset = 184;

// ISQ: the data offset, in blocks, ends the first block; the writer's extended header is a
// MultiHeader block, the calibration table block, and two calibration blocks.
constexpr size_t ISQDataOffsetPosition = BlockSize - 4;
constexpr size_t ISQMultiHeaderNamePosition = BlockSize + 8;
constexpr size_t ISQCalibrationNamePosition = 2 * BlockSize + 136;
constexpr size_t ISQCalibrationBlocksPosition = ISQCalibrationNamePosition + 16;

std::string
OutputPath(const std::string & name)
{
  return SCANCO_TOSTRING(ITK_TEST_OUTPUT_DIR) + "/" + name;
}

void
PutInt32(Bytes & bytes, size_t offset, int32_t value)
{
  const auto u = static_cast<uint32_t>(value);
  for (size_t i = 0; i < 4; ++i)
  {
    bytes[offset + i] = static_cast<char>((u >> (8 * i)) & 0xff);
  }
}

void
PutInt64(Bytes & bytes, size_t offset, int64_t value)
{
  const auto u = static_cast<uint64_t>(value);
  for (size_t i = 0; i < 8; ++i)
  {
    bytes[offset + i] = static_cast<char>((u >> (8 * i)) & 0xff);
  }
}

Bytes
ReadBytes(const std::string & path)
{
  std::ifstream in(path, std::ios::binary);
  if (!in)
  {
    throw std::runtime_error("Cannot open " + path);
  }
  return Bytes(std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
}

void
WriteBytes(const std::string & path, const Bytes & bytes)
{
  std::ofstream out(path, std::ios::binary);
  out.write(bytes.data(), static_cast<std::streamsize>(bytes.size()));
  if (!out)
  {
    throw std::runtime_error("Cannot write " + path);
  }
}

constexpr const char * AIMV020Version = "AIMDATA_V020   ";
constexpr const char * AIMV030Version = "AIMDATA_V030   ";

ImageType::Pointer
MakeRampImage(const char * version)
{
  auto                      image = ImageType::New();
  const ImageType::SizeType size{ { 4, 3, 2 } };
  image->SetRegions(ImageType::RegionType(size));
  image->Allocate();
  short value = 0;
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
  {
    it.Set(value++);
  }
  itk::EncapsulateMetaData<std::string>(image->GetMetaDataDictionary(), "CreationDate", "19-NOV-2026 12:00:00.000");
  itk::EncapsulateMetaData<std::string>(image->GetMetaDataDictionary(), "ModificationDate", "19-NOV-2026 12:00:00.000");
  if (version != nullptr)
  {
    itk::EncapsulateMetaData<std::string>(image->GetMetaDataDictionary(), "Version", version);
  }
  return image;
}

// Writes a well-formed file with ITK's own Scanco writer and returns its bytes.
Bytes
WriteReference(const std::string & name, const char * version = nullptr)
{
  const std::string path = OutputPath(name);
  auto              writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetImageIO(itk::ScancoImageIO::New());
  writer->SetInput(MakeRampImage(version));
  writer->SetFileName(path);
  writer->Update();
  return ReadBytes(path);
}

int32_t
GetInt32(const Bytes & bytes, size_t offset)
{
  uint32_t u = 0;
  for (size_t i = 0; i < 4; ++i)
  {
    u |= static_cast<uint32_t>(static_cast<unsigned char>(bytes[offset + i])) << (8 * i);
  }
  return static_cast<int32_t>(u);
}

// Legacy v020 files have no magic string and start with the pre-header length.
Bytes
WriteReferenceV020(const std::string & name)
{
  Bytes bytes = WriteReference(name, AIMV020Version);
  EXPECT_EQ(GetInt32(bytes, 0), static_cast<int32_t>(AIMV020PreHeaderSize));
  EXPECT_EQ(GetInt32(bytes, AIMV020StructLengthPosition), static_cast<int32_t>(AIMV020StructSize));
  return bytes;
}

Bytes
WriteReferenceV030(const std::string & name)
{
  Bytes bytes = WriteReference(name, AIMV030Version);
  EXPECT_EQ(std::string(bytes.data(), 15), AIMV030Version);
  EXPECT_EQ(GetInt32(bytes, AIMV030StructLengthPosition), static_cast<int32_t>(AIMV030StructSize));
  return bytes;
}

ImageType::Pointer
ReadScanco(const std::string & path)
{
  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(itk::ScancoImageIO::New());
  reader->SetFileName(path);
  reader->Update();
  return reader->GetOutput();
}

void
ExpectReadThrows(const std::string & name, const Bytes & bytes)
{
  const std::string path = OutputPath(name);
  ASSERT_NO_THROW(WriteBytes(path, bytes));
  EXPECT_THROW(ReadScanco(path), itk::ExceptionObject) << name;
}

// AIM v020 file with an uncompressed or compressed data section.
Bytes
MakeAIMV020(int32_t dataType, const int32_t (&dims)[3], const Bytes & data)
{
  Bytes bytes(AIMV020PreHeaderSize + AIMV020StructSize, 0);
  PutInt32(bytes, 0, static_cast<int32_t>(AIMV020PreHeaderSize));
  PutInt32(bytes, AIMV020StructLengthPosition, static_cast<int32_t>(AIMV020StructSize));
  PutInt32(bytes, AIMV020DataLengthPosition, static_cast<int32_t>(data.size()));
  const size_t structStart = AIMV020PreHeaderSize;
  PutInt32(bytes, structStart + AIMV020TypeOffset, dataType);
  for (size_t i = 0; i < 3; ++i)
  {
    PutInt32(bytes, structStart + AIMV020DimensionOffset + 4 * i, dims[i]);
    const float spacing = 1.0f;
    std::memcpy(bytes.data() + structStart + AIMV020ElementSizeOffset + 4 * i, &spacing, sizeof(spacing));
  }
  bytes.insert(bytes.end(), data.begin(), data.end());
  return bytes;
}

Bytes
MakeAIMV030(int32_t dataType, const int32_t (&dims)[3], const Bytes & data)
{
  Bytes bytes(AIMV030VersionSize + AIMV030PreHeaderSize + AIMV030StructSize, 0);
  std::memcpy(bytes.data(), AIMV030Version, 15);
  PutInt64(bytes, AIMV030VersionSize, static_cast<int64_t>(AIMV030PreHeaderSize));
  PutInt64(bytes, AIMV030StructLengthPosition, static_cast<int64_t>(AIMV030StructSize));
  PutInt64(bytes, AIMV030DataLengthPosition, static_cast<int64_t>(data.size()));
  const size_t structStart = AIMV030VersionSize + AIMV030PreHeaderSize;
  PutInt32(bytes, structStart + AIMV030TypeOffset, dataType);
  for (size_t i = 0; i < 3; ++i)
  {
    PutInt64(bytes, structStart + AIMV030DimensionOffset + 8 * i, dims[i]);
    PutInt64(bytes, structStart + AIMV030ElementSizeOffset + 8 * i, 1000000); // micrometers
  }
  bytes.insert(bytes.end(), data.begin(), data.end());
  return bytes;
}

ImageType::Pointer
WriteAndRead(const std::string & name, const Bytes & bytes)
{
  const std::string path = OutputPath(name);
  WriteBytes(path, bytes);
  return ReadScanco(path);
}

void
ExpectRamp(const ImageType * image)
{
  short expected = 0;
  for (itk::ImageRegionConstIterator<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd(); ++it)
  {
    EXPECT_EQ(it.Get(), expected++);
  }
}
} // namespace

TEST(ScancoImageIO, ReferenceFilesRoundTrip)
{
  WriteReferenceV020("ScancoGTestRef020.aim");
  ExpectRamp(ReadScanco(OutputPath("ScancoGTestRef020.aim")));

  WriteReferenceV030("ScancoGTestRef030.aim");
  ExpectRamp(ReadScanco(OutputPath("ScancoGTestRef030.aim")));

  WriteReference("ScancoGTestRef.isq");
  ExpectRamp(ReadScanco(OutputPath("ScancoGTestRef.isq")));
}

TEST(ScancoImageIO, RejectsAIMImageStructLengthMismatch)
{
  const Bytes v020 = WriteReferenceV020("ScancoGTestStruct020.aim");
  for (const int32_t length : { 4096, 141, 139, 0, -1 })
  {
    Bytes bad = v020;
    PutInt32(bad, AIMV020StructLengthPosition, length);
    ExpectReadThrows("ScancoGTestStruct020Bad.aim", bad);
  }

  const Bytes v030 = WriteReferenceV030("ScancoGTestStruct030.aim");
  for (const int64_t length : { int64_t{ 4096 }, int64_t{ 225 }, int64_t{ 223 }, int64_t{ -1 } })
  {
    Bytes bad = v030;
    PutInt64(bad, AIMV030StructLengthPosition, length);
    ExpectReadThrows("ScancoGTestStruct030Bad.aim", bad);
  }
}

TEST(ScancoImageIO, RejectsAIMProcessingLogBeyondFile)
{
  Bytes v020 = WriteReferenceV020("ScancoGTestLog020.aim");
  PutInt32(v020, AIMV020LogLengthPosition, 0x7fffffff);
  ExpectReadThrows("ScancoGTestLog020Bad.aim", v020);

  Bytes v030 = WriteReferenceV030("ScancoGTestLog030.aim");
  PutInt64(v030, AIMV030LogLengthPosition, INT64_MAX);
  ExpectReadThrows("ScancoGTestLog030Bad.aim", v030);
  PutInt64(v030, AIMV030LogLengthPosition, -1);
  ExpectReadThrows("ScancoGTestLog030Bad.aim", v030);
}

TEST(ScancoImageIO, RejectsTruncatedAIMHeader)
{
  const Bytes v020 = WriteReferenceV020("ScancoGTestTrunc020.aim");
  for (const size_t length : { size_t{ 0 }, size_t{ 10 }, AIMV020PreHeaderSize + 100 })
  {
    ExpectReadThrows("ScancoGTestTrunc020Bad.aim", Bytes(v020.begin(), v020.begin() + length));
  }
  const Bytes  v030 = WriteReferenceV030("ScancoGTestTrunc030.aim");
  const size_t cut = AIMV030VersionSize + AIMV030PreHeaderSize + 100;
  ExpectReadThrows("ScancoGTestTrunc030Bad.aim", Bytes(v030.begin(), v030.begin() + cut));
}

TEST(ScancoImageIO, ReadsAIMSmallerThanOneBlock)
{
  constexpr int32_t dims[3] = { 2, 2, 2 };
  Bytes             data(2 * 8, 0);
  data[2] = 7;
  const auto image = WriteAndRead("ScancoGTestSmall.aim", MakeAIMV020(0x00020002, dims, data));
  EXPECT_EQ(image->GetPixel({ { 1, 0, 0 } }), 7);
}

TEST(ScancoImageIO, RejectsNegativeAIMDimension)
{
  constexpr int32_t dims[3] = { -4, 4, 4 };
  ExpectReadThrows("ScancoGTestNegDim.aim", MakeAIMV020(0x00020002, dims, Bytes(128, 0)));
}

TEST(ScancoImageIO, RunLengthBitsDecodeAndRejectBadSize)
{
  constexpr int32_t dims[3] = { 4, 4, 4 };
  // size field, the two voxel values, then run lengths 10 and 54
  Bytes good(4, 0);
  PutInt32(good, 0, 8);
  good.insert(good.end(), { 1, 2, 10, 54 });
  const auto image = WriteAndRead("ScancoGTestRLE.aim", MakeAIMV020(0x00150001, dims, good));
  size_t     index = 0;
  for (itk::ImageRegionConstIterator<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd();
       ++it, ++index)
  {
    EXPECT_EQ(it.Get(), index < 10 ? 1 : 2) << index;
  }

  for (const int32_t size : { 3, 4, 5, 1 << 30 })
  {
    Bytes bad(4, 0);
    PutInt32(bad, 0, size);
    bad.push_back(1);
    ExpectReadThrows("ScancoGTestRLEBad.aim", MakeAIMV020(0x00150001, dims, bad));
  }
}

TEST(ScancoImageIO, RunLengthCharDecodeAndRejectBadSize)
{
  constexpr int32_t dims[3] = { 4, 4, 4 };
  // size field, then (run length, value) pairs
  Bytes good(4, 0);
  PutInt32(good, 0, 8);
  good.insert(good.end(), { 10, 3, 54, 4 });
  const auto image = WriteAndRead("ScancoGTestRLEChar.aim", MakeAIMV020(0x00080002, dims, good));
  size_t     index = 0;
  for (itk::ImageRegionConstIterator<ImageType> it(image, image->GetLargestPossibleRegion()); !it.IsAtEnd();
       ++it, ++index)
  {
    EXPECT_EQ(it.Get(), index < 10 ? 3 : 4) << index;
  }

  for (const int32_t size : { 0, 3, 1 << 30 })
  {
    Bytes bad(4, 0);
    PutInt32(bad, 0, size);
    ExpectReadThrows("ScancoGTestRLECharBad.aim", MakeAIMV020(0x00080002, dims, bad));
  }
}

TEST(ScancoImageIO, RunLengthSizeIsEightBytesInAIMV030)
{
  constexpr int32_t dims[3] = { 4, 4, 4 };
  Bytes             good(8, 0);
  PutInt64(good, 0, 12);
  good.insert(good.end(), { 1, 2, 10, 54 });
  const auto image = WriteAndRead("ScancoGTestRLE030.aim", MakeAIMV030(0x00150001, dims, good));
  EXPECT_EQ(image->GetPixel({ { 0, 0, 0 } }), 1);
  EXPECT_EQ(image->GetPixel({ { 3, 3, 3 } }), 2);

  for (const int64_t size : { int64_t{ 7 }, int64_t{ 9 }, int64_t{ 1 } << 32, int64_t{ -1 } })
  {
    Bytes bad(8, 0);
    PutInt64(bad, 0, size);
    bad.push_back(1);
    ExpectReadThrows("ScancoGTestRLE030Bad.aim", MakeAIMV030(0x00150001, dims, bad));
  }
}

TEST(ScancoImageIO, RejectsDimensionsWhoseProductWraps)
{
  // 2^23 * 2^22 * 2^22 voxels and 2^22 * 2^21 * 2^21 + 1 packed bytes both wrap to small values
  constexpr int32_t dims[3] = { 1 << 23, 1 << 22, 1 << 22 };
  ExpectReadThrows("ScancoGTestWrapPacked.aim", MakeAIMV020(0x00060001, dims, Bytes(64, 0)));
  ExpectReadThrows("ScancoGTestWrapShort.aim", MakeAIMV020(0x00020002, dims, Bytes(64, 0)));
}

TEST(ScancoImageIO, RejectsDimensionsTheDataCannotFill)
{
  // Rejected from the header alone, before a reader allocates the image
  auto expectInformationThrows = [](const std::string & name, const Bytes & bytes) {
    const std::string path = OutputPath(name);
    ASSERT_NO_THROW(WriteBytes(path, bytes));
    auto imageIO = itk::ScancoImageIO::New();
    imageIO->SetFileName(path);
    EXPECT_THROW(imageIO->ReadImageInformation(), itk::ExceptionObject) << name;
  };
  constexpr int32_t dims[3] = { 1 << 12, 1 << 12, 1 << 12 };
  expectInformationThrows("ScancoGTestLargeShort.aim", MakeAIMV020(0x00020002, dims, Bytes(64, 0)));
  expectInformationThrows("ScancoGTestLargePacked.aim", MakeAIMV020(0x00060001, dims, Bytes(64, 0)));
  expectInformationThrows("ScancoGTestLargeRunLength.aim", MakeAIMV020(0x00150001, dims, Bytes(64, 0)));
  expectInformationThrows("ScancoGTestLargeShort030.aim", MakeAIMV030(0x00020002, dims, Bytes(64, 0)));

  Bytes isq = WriteReference("ScancoGTestLarge.isq");
  isq.resize(isq.size() - 2);
  expectInformationThrows("ScancoGTestLargeTruncated.isq", isq);
}

TEST(ScancoImageIO, PackedBitsDecodeTwoByTwoByTwoBlocks)
{
  for (const auto & size : { std::vector<int32_t>{ 4, 4, 4 }, std::vector<int32_t>{ 3, 5, 7 } })
  {
    const int32_t dims[3] = { size[0], size[1], size[2] };
    const size_t  xinc = (dims[0] + 1) / 2;
    const size_t  yinc = (dims[1] + 1) / 2;
    const size_t  zinc = (dims[2] + 1) / 2;
    // Each byte holds a 2x2x2 block; bit = x&1 | (y&1)<<1 | (z&1)<<2. A zero trailing byte selects 0x7f.
    Bytes packed(xinc * yinc * zinc + 1, 0);
    auto  isSet = [](int32_t x, int32_t y, int32_t z) { return ((x * 7 + y * 3 + z * 5) % 3) == 0; };
    for (int32_t z = 0; z < dims[2]; ++z)
    {
      for (int32_t y = 0; y < dims[1]; ++y)
      {
        for (int32_t x = 0; x < dims[0]; ++x)
        {
          if (isSet(x, y, z))
          {
            const size_t byte = ((z / 2) * yinc + (y / 2)) * xinc + (x / 2);
            packed[byte] = static_cast<char>(packed[byte] | (1 << ((x & 1) | ((y & 1) << 1) | ((z & 1) << 2))));
          }
        }
      }
    }
    const auto image = WriteAndRead("ScancoGTestPacked.aim", MakeAIMV020(0x00060001, dims, packed));
    ExpectReadThrows("ScancoGTestPackedShort.aim",
                     MakeAIMV020(0x00060001, dims, Bytes(packed.begin(), packed.end() - 1)));
    for (int32_t z = 0; z < dims[2]; ++z)
    {
      for (int32_t y = 0; y < dims[1]; ++y)
      {
        for (int32_t x = 0; x < dims[0]; ++x)
        {
          EXPECT_EQ(image->GetPixel({ { x, y, z } }), isSet(x, y, z) ? 0x7f : 0) << x << ',' << y << ',' << z;
        }
      }
    }
  }
}

TEST(ScancoImageIO, RejectsISQDataOffsetOutsideFile)
{
  const Bytes isq = WriteReference("ScancoGTestOffset.isq");
  const auto  blocks = static_cast<int32_t>(isq.size() / BlockSize);
  for (const int32_t offset : { -2, -1, blocks + 4, 0x7fffffff })
  {
    Bytes bad = isq;
    PutInt32(bad, ISQDataOffsetPosition, offset);
    ExpectReadThrows("ScancoGTestOffsetBad.isq", bad);
  }
  ExpectReadThrows("ScancoGTestOffsetShort.isq", Bytes(isq.begin(), isq.begin() + 100));
}

TEST(ScancoImageIO, IgnoresISQCalibrationBlocksBeyondExtendedHeader)
{
  const Bytes isq = WriteReference("ScancoGTestCal.isq");
  ASSERT_EQ(GetInt32(isq, ISQDataOffsetPosition), 4);
  ASSERT_EQ(std::string(isq.data() + ISQMultiHeaderNamePosition, 11), "MultiHeader");
  ASSERT_EQ(std::string(isq.data() + ISQCalibrationNamePosition, 11), "Calibration");

  // Drop the last calibration block so the table claims more than the header holds.
  Bytes shortened(isq.begin(), isq.begin() + 4 * BlockSize);
  shortened.insert(shortened.end(), isq.begin() + 5 * BlockSize, isq.end());
  PutInt32(shortened, ISQDataOffsetPosition, 3);
  ExpectRamp(WriteAndRead("ScancoGTestCalShort.isq", shortened));

  for (const int32_t blocks : { -1, -2, 0x7fffffff })
  {
    Bytes bad = isq;
    PutInt32(bad, ISQCalibrationBlocksPosition, blocks);
    ExpectRamp(WriteAndRead("ScancoGTestCalBad.isq", bad));
  }
}
