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

#include "itkTIFFImageIO.h"
#include "itkImageIORegion.h"
#include "itksys/SystemTools.hxx"

#include "itkGTest.h"

#include "itk_tiff.h"

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <type_traits>
#include <vector>
#include <string>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{

std::string
TIFFImageIOGTestOutputPath(const std::string & name)
{
  const std::string dir = TOSTRING(ITK_TEST_OUTPUT_DIR);
  itksys::SystemTools::MakeDirectory(dir);
  return dir + "/" + name;
}

void
WriteUniformDirectory(TIFF *        tif,
                      uint32_t      width,
                      uint32_t      height,
                      unsigned char value,
                      bool          hasSubfiletype,
                      uint32_t      subfiletype)
{
  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, height);
  if (hasSubfiletype)
  {
    TIFFSetField(tif, TIFFTAG_SUBFILETYPE, subfiletype);
  }

  const std::vector<unsigned char> row(width, value);
  for (uint32_t r = 0; r < height; ++r)
  {
    TIFFWriteScanline(tif, const_cast<unsigned char *>(row.data()), r, 0);
  }
  TIFFWriteDirectory(tif);
}

void
WriteU16(unsigned char * dest, uint16_t value, bool bigEndian)
{
  if (bigEndian)
  {
    dest[0] = static_cast<unsigned char>(value >> 8);
    dest[1] = static_cast<unsigned char>(value);
  }
  else
  {
    dest[0] = static_cast<unsigned char>(value);
    dest[1] = static_cast<unsigned char>(value >> 8);
  }
}

// libtiff refuses to write a COMPRESSION value with no registered codec, so an
// unregistered-codec fixture must be produced by patching the tag's on-disk value.
bool
PatchCompressionTag(const std::string & fileName, uint16_t newCompression)
{
  std::ifstream              in(fileName, std::ios::binary);
  std::vector<unsigned char> buf((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
  in.close();
  if (buf.size() < 2)
  {
    return false;
  }
  const bool bigEndian = (buf[0] == 'M' && buf[1] == 'M');

  unsigned char pattern[8];
  WriteU16(pattern + 0, TIFFTAG_COMPRESSION, bigEndian);
  WriteU16(pattern + 2, TIFF_SHORT, bigEndian);
  WriteU16(pattern + 4, 1, bigEndian); // field count == 1
  WriteU16(pattern + 6, 0, bigEndian);

  const auto match = std::search(buf.begin(), buf.end(), pattern, pattern + 8);
  if (match == buf.end())
  {
    return false;
  }
  WriteU16(&*match + 8, newCompression, bigEndian);

  std::ofstream out(fileName, std::ios::binary);
  out.write(reinterpret_cast<const char *>(buf.data()), static_cast<std::streamsize>(buf.size()));
  return out.good();
}

} // namespace

// Kept pages must land at sequential slice index, not raw TIFF directory index.
TEST(TIFFImageIOGTest, ReadVolumeSkipsReducedImageAndPacksSlicesSequentially)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 4;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_ReducedImage.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  WriteUniformDirectory(tif, width, height, 200, true, FILETYPE_REDUCEDIMAGE);
  WriteUniformDirectory(tif, width, height, 10, true, 0);
  WriteUniformDirectory(tif, width, height, 20, true, 0);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();

  ASSERT_EQ(tiffImageIO->GetNumberOfDimensions(), 3u);
  ASSERT_EQ(tiffImageIO->GetDimensions(2), 2u);

  itk::ImageIORegion ioRegion(3);
  for (unsigned int d = 0; d < 3; ++d)
  {
    ioRegion.SetIndex(d, 0);
    ioRegion.SetSize(d, tiffImageIO->GetDimensions(d));
  }
  tiffImageIO->SetIORegion(ioRegion);

  const size_t               sliceBytes = static_cast<size_t>(width) * height;
  constexpr unsigned char    sentinel = 111;
  std::vector<unsigned char> buffer(sliceBytes * 3, sentinel); // 1 padding slice past the logical depth

  tiffImageIO->Read(buffer.data());

  EXPECT_EQ(buffer[0 * sliceBytes], 10) << "first SUBFILETYPE==0 page must land at slice 0";
  EXPECT_EQ(buffer[1 * sliceBytes], 20) << "second SUBFILETYPE==0 page must land at slice 1";
  EXPECT_EQ(buffer[2 * sliceBytes], sentinel) << "read must not write past the logical volume depth";
}

// An untagged page must be excluded once SUBFILETYPE tagging is in use.
// TIFF 6.0 defines NewSubfileType to default to 0 (a primary image); an
// untagged page in a file that also has explicitly-tagged pages must still
// count as a kept slice, not be silently dropped.
TEST(TIFFImageIOGTest, ReadVolumeKeepsUntaggedPageAsPrimary)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 4;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_UntaggedPage.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  WriteUniformDirectory(tif, width, height, 200, false, 0); // no SUBFILETYPE tag at all
  WriteUniformDirectory(tif, width, height, 10, true, 0);
  WriteUniformDirectory(tif, width, height, 20, true, 0);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();

  ASSERT_EQ(tiffImageIO->GetNumberOfDimensions(), 3u);
  ASSERT_EQ(tiffImageIO->GetDimensions(2), 3u);

  itk::ImageIORegion ioRegion(3);
  for (unsigned int d = 0; d < 3; ++d)
  {
    ioRegion.SetIndex(d, 0);
    ioRegion.SetSize(d, tiffImageIO->GetDimensions(d));
  }
  tiffImageIO->SetIORegion(ioRegion);

  const size_t               sliceBytes = static_cast<size_t>(width) * height;
  std::vector<unsigned char> buffer(sliceBytes * 3);

  tiffImageIO->Read(buffer.data());

  EXPECT_EQ(buffer[0 * sliceBytes], 200) << "untagged page must be kept as the first primary slice";
  EXPECT_EQ(buffer[1 * sliceBytes], 10) << "first SUBFILETYPE==0 page must land at slice 1";
  EXPECT_EQ(buffer[2 * sliceBytes], 20) << "second SUBFILETYPE==0 page must land at slice 2";
}

// Per-page geometry is pinned from the first page; a mismatched later page must be rejected.
TEST(TIFFImageIOGTest, ReadVolumeRejectsPageWithDifferentGeometry)
{
  constexpr uint32_t firstWidth = 8;
  constexpr uint32_t secondWidth = 4;
  constexpr uint32_t height = 4;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_NarrowerPage.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  WriteUniformDirectory(tif, firstWidth, height, 1, false, 0);
  WriteUniformDirectory(tif, secondWidth, height, 2, false, 0);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();

  ASSERT_EQ(tiffImageIO->GetNumberOfDimensions(), 3u);
  ASSERT_EQ(tiffImageIO->GetDimensions(2), 2u);

  itk::ImageIORegion ioRegion(3);
  for (unsigned int d = 0; d < 3; ++d)
  {
    ioRegion.SetIndex(d, 0);
    ioRegion.SetSize(d, tiffImageIO->GetDimensions(d));
  }
  tiffImageIO->SetIORegion(ioRegion);

  std::vector<unsigned char> buffer(static_cast<size_t>(firstWidth) * height * 2, 0);

  EXPECT_THROW(tiffImageIO->Read(buffer.data()), itk::ExceptionObject);
}

// SAMPLEFORMAT_VOID has no defined component type; the ladder must reject it explicitly.
TEST(TIFFImageIOGTest, ReadImageInformationRejects32BitSampleFormatVoid)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 4;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_32BitVoid.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 32);
  TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_VOID);
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, height);

  const std::vector<uint32_t> row(width, 0);
  for (uint32_t r = 0; r < height; ++r)
  {
    TIFFWriteScanline(tif, const_cast<uint32_t *>(row.data()), r, 0);
  }
  TIFFWriteDirectory(tif);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);

  EXPECT_THROW(tiffImageIO->ReadImageInformation(), itk::ExceptionObject);
}

// A MINISWHITE page with IEEEFP samples has no fixed maximum to invert
// against, so PutGrayscale's integral-only inversion would silently pass it
// through photo-negative. CanRead() excludes the combination; the read must
// fail loudly rather than return un-inverted float data.
TEST(TIFFImageIOGTest, ReadImageInformationRejectsMinIsWhiteFloat)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 4;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_MinIsWhiteFloat.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 32);
  TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_IEEEFP);
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISWHITE);
  TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, height);

  const std::vector<float> row(width, 0.25f);
  for (uint32_t r = 0; r < height; ++r)
  {
    TIFFWriteScanline(tif, const_cast<float *>(row.data()), r, 0);
  }
  TIFFWriteDirectory(tif);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);

  EXPECT_THROW(tiffImageIO->ReadImageInformation(), itk::ExceptionObject);
}

// MINISWHITE samples must be inverted to min-is-black, not copied verbatim.
TEST(TIFFImageIOGTest, ReadInvertsMinIsWhitePhotometric)
{
  constexpr uint32_t      width = 4;
  constexpr uint32_t      height = 4;
  constexpr unsigned char storedValue = 70;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_MinIsWhite.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISWHITE);
  TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, height);

  const std::vector<unsigned char> row(width, storedValue);
  for (uint32_t r = 0; r < height; ++r)
  {
    TIFFWriteScanline(tif, const_cast<unsigned char *>(row.data()), r, 0);
  }
  TIFFWriteDirectory(tif);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();

  std::vector<unsigned char> buffer(static_cast<size_t>(width) * height, 0);
  tiffImageIO->Read(buffer.data());

  constexpr unsigned char expected = static_cast<unsigned char>(~storedValue);
  for (const unsigned char sample : buffer)
  {
    EXPECT_EQ(sample, expected) << "MINISWHITE samples must be inverted, not copied verbatim";
  }
}

// GRAYSCALE component count must follow SamplesPerPixel, not assume 1.
TEST(TIFFImageIOGTest, ReadGrayscaleWithExtraSamplePreservesAllComponents)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 2;
  constexpr uint16_t samplesPerPixel = 2;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_GrayAlpha.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, samplesPerPixel);
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, height);
  const uint16_t extraSampleTypes[1] = { EXTRASAMPLE_UNSPECIFIED };
  TIFFSetField(tif, TIFFTAG_EXTRASAMPLES, 1, extraSampleTypes);

  std::vector<unsigned char> row(static_cast<size_t>(width) * samplesPerPixel);
  for (uint32_t x = 0; x < width; ++x)
  {
    row[x * samplesPerPixel + 0] = 100;
    row[x * samplesPerPixel + 1] = 200;
  }
  for (uint32_t r = 0; r < height; ++r)
  {
    TIFFWriteScanline(tif, row.data(), r, 0);
  }
  TIFFWriteDirectory(tif);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();

  ASSERT_EQ(tiffImageIO->GetNumberOfComponents(), samplesPerPixel);

  std::vector<unsigned char> buffer(static_cast<size_t>(width) * height * samplesPerPixel, 0);
  tiffImageIO->Read(buffer.data());

  for (uint32_t i = 0; i < width * height; ++i)
  {
    EXPECT_EQ(buffer[i * samplesPerPixel + 0], 100) << "grayscale sample at pixel " << i;
    EXPECT_EQ(buffer[i * samplesPerPixel + 1], 200) << "extra sample at pixel " << i;
  }
}

// 2-component images must get MINISBLACK + 1 EXTRASAMPLES entry, not incomplete RGB.
TEST(TIFFImageIOGTest, WriteTwoComponentImageDeclaresMinIsBlackWithExtraSample)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 2;
  constexpr uint16_t samplesPerPixel = 2;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_WriteGrayAlpha.tif");

  std::vector<unsigned char> buffer(static_cast<size_t>(width) * height * samplesPerPixel);
  for (size_t i = 0; i < buffer.size(); i += samplesPerPixel)
  {
    buffer[i + 0] = 100;
    buffer[i + 1] = 200;
  }

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->SetNumberOfDimensions(2);
  tiffImageIO->SetDimensions(0, width);
  tiffImageIO->SetDimensions(1, height);
  tiffImageIO->SetPixelType(itk::IOPixelEnum::VECTOR);
  tiffImageIO->SetNumberOfComponents(samplesPerPixel);
  tiffImageIO->SetComponentType(itk::IOComponentEnum::UCHAR);
  tiffImageIO->Write(buffer.data());

  TIFF * tif = TIFFOpen(fileName.c_str(), "r");
  ASSERT_NE(tif, nullptr);

  uint16_t photometric = 0;
  ASSERT_TRUE(TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photometric));
  EXPECT_EQ(photometric, PHOTOMETRIC_MINISBLACK) << "2-component image must not be tagged incomplete RGB";

  uint16_t writtenSamplesPerPixel = 0;
  ASSERT_TRUE(TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &writtenSamplesPerPixel));
  EXPECT_EQ(writtenSamplesPerPixel, samplesPerPixel);

  uint16_t   extraSamplesCount = 0;
  uint16_t * extraSamplesTypes = nullptr;
  ASSERT_TRUE(TIFFGetField(tif, TIFFTAG_EXTRASAMPLES, &extraSamplesCount, &extraSamplesTypes));
  EXPECT_EQ(extraSamplesCount, 1) << "the non-color sample must be declared as EXTRASAMPLES";

  TIFFClose(tif);
}

// ReadGenericImage's dispatch previously had no branch for
// IOComponentEnum::UINT/INT even though CanRead() (and the SampleFormat
// switch in ReadImageInformation) accepted 32-bit UINT/INT pages: the read
// silently no-op'd, leaving the output buffer untouched. Exercise the full
// decode path, not just component-type detection.
TEST(TIFFImageIOGTest, ReadDecodes32BitUnsignedIntPixels)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 4;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_32BitUInt.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 32);
  TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_UINT);
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, height);

  std::vector<uint32_t> row(width);
  for (uint32_t r = 0; r < height; ++r)
  {
    for (uint32_t c = 0; c < width; ++c)
    {
      row[c] = 1000u * r + c;
    }
    TIFFWriteScanline(tif, row.data(), r, 0);
  }
  TIFFWriteDirectory(tif);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();
  ASSERT_EQ(tiffImageIO->GetComponentType(), itk::IOComponentEnum::UINT);

  std::vector<uint32_t> buffer(static_cast<size_t>(width) * height, 0xDEADBEEFu);
  ASSERT_NO_THROW(tiffImageIO->Read(buffer.data()));

  for (uint32_t r = 0; r < height; ++r)
  {
    for (uint32_t c = 0; c < width; ++c)
    {
      EXPECT_EQ(buffer[(r * width) + c], 1000u * r + c) << "row " << r << " col " << c;
    }
  }
}

// ReadCurrentPage advances the per-slice buffer offset using the component
// type; the 32-bit UINT/INT arms must advance by 4 bytes per element, not the
// default 1-byte (unsigned char) stride, or every slice past the first lands
// at the wrong offset. Single-slice reads (offset 0) do not exercise this.
TEST(TIFFImageIOGTest, ReadDecodesMultiSlice32BitIntegerVolume)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 4;
  constexpr uint32_t pages = 3;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_32BitIntVolume.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  for (uint32_t p = 0; p < pages; ++p)
  {
    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 32);
    TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, SAMPLEFORMAT_INT);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 1);
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
    TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, height);
    std::vector<int32_t> row(width);
    for (uint32_t r = 0; r < height; ++r)
    {
      for (uint32_t c = 0; c < width; ++c)
      {
        row[c] = static_cast<int32_t>(100000 * p + 1000 * r + c) - 50000;
      }
      TIFFWriteScanline(tif, row.data(), r, 0);
    }
    TIFFWriteDirectory(tif);
  }
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();
  ASSERT_EQ(tiffImageIO->GetComponentType(), itk::IOComponentEnum::INT);
  ASSERT_EQ(tiffImageIO->GetNumberOfDimensions(), 3u);

  itk::ImageIORegion ioRegion(3);
  for (unsigned int d = 0; d < 3; ++d)
  {
    ioRegion.SetIndex(d, 0);
    ioRegion.SetSize(d, tiffImageIO->GetDimensions(d));
  }
  tiffImageIO->SetIORegion(ioRegion);

  std::vector<int32_t> buffer(static_cast<size_t>(width) * height * pages, static_cast<int32_t>(0xDEADBEEF));
  ASSERT_NO_THROW(tiffImageIO->Read(buffer.data()));

  for (uint32_t p = 0; p < pages; ++p)
  {
    for (uint32_t r = 0; r < height; ++r)
    {
      for (uint32_t c = 0; c < width; ++c)
      {
        const size_t  idx = ((static_cast<size_t>(p) * height) + r) * width + c;
        const int32_t expected = static_cast<int32_t>(100000 * p + 1000 * r + c) - 50000;
        EXPECT_EQ(buffer[idx], expected) << "page " << p << " row " << r << " col " << c;
      }
    }
  }
}

// ReadImageInformation only reports metadata, already fully determined by tags read
// independently of codec support; it must not throw for an unregistered codec (e.g. tag
// 34712, JPEG2000). TIFFFindCODEC returns nullptr for such schemes, so Read(), which
// actually needs to decode, must report the numeric tag, not "unknown".
TEST(TIFFImageIOGTest, ReadImageInformationSucceedsAndReadThrowsForUnregisteredCompression)
{
  constexpr uint32_t width = 4;
  constexpr uint32_t height = 4;
  constexpr uint16_t unregisteredCompression = 34712;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_UnregisteredCodec.tif");

  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  WriteUniformDirectory(tif, width, height, 5, false, 0);
  TIFFClose(tif);

  ASSERT_TRUE(PatchCompressionTag(fileName, unregisteredCompression));

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);

  ASSERT_NO_THROW(tiffImageIO->ReadImageInformation());
  EXPECT_EQ(tiffImageIO->GetNumberOfComponents(), 1u);
  EXPECT_EQ(tiffImageIO->GetComponentType(), itk::IOComponentEnum::UCHAR);
  EXPECT_EQ(tiffImageIO->GetDimensions(0), width);
  EXPECT_EQ(tiffImageIO->GetDimensions(1), height);

  std::vector<unsigned char> buffer(static_cast<size_t>(width) * height, 0);

  try
  {
    tiffImageIO->Read(buffer.data());
    FAIL() << "expected an itk::ExceptionObject for an unregistered TIFF codec";
  }
  catch (const itk::ExceptionObject & e)
  {
    EXPECT_NE(std::string(e.GetDescription()).find(std::to_string(unregisteredCompression)), std::string::npos);
  }
}

namespace
{

struct TiledSpec
{
  uint32_t          width{ 64 };
  uint32_t          height{ 48 };
  uint32_t          tileWidth{ 16 };
  uint32_t          tileHeight{ 16 };
  uint16_t          samplesPerPixel{ 1 };
  uint16_t          photometric{ PHOTOMETRIC_MINISBLACK };
  uint16_t          compression{ COMPRESSION_NONE };
  uint16_t          orientation{ ORIENTATION_TOPLEFT };
  uint16_t          planarConfig{ PLANARCONFIG_CONTIG };
  uint32_t          pages{ 1 };
  std::vector<bool> stripPages{}; // pages written as strips instead of tiles
};

// Deterministic, position-dependent sample value that fits every tested sample type.
template <typename T>
T
TiledPixel(uint32_t x, uint32_t y, uint32_t c, uint32_t page)
{
  const int64_t v = (x * 7u + y * 13u + c * 31u + page * 57u) % 251u;
  if constexpr (std::is_floating_point_v<T>)
  {
    return static_cast<T>(v) * static_cast<T>(0.5);
  }
  else
  {
    constexpr int64_t scale = sizeof(T) == 1 ? 1 : (sizeof(T) == 2 ? 100 : 1000000);
    if constexpr (std::is_signed_v<T>)
    {
      return static_cast<T>(v * scale - 125 * scale);
    }
    else
    {
      return static_cast<T>(v * scale);
    }
  }
}

template <typename T>
bool
WriteTiledImage(const std::string & fileName, const TiledSpec & s)
{
  TIFF * tif = TIFFOpen(fileName.c_str(), "w");
  if (tif == nullptr)
  {
    return false;
  }

  uint16_t sampleFormat = SAMPLEFORMAT_UINT;
  if constexpr (std::is_floating_point_v<T>)
  {
    sampleFormat = SAMPLEFORMAT_IEEEFP;
  }
  else if constexpr (std::is_signed_v<T>)
  {
    sampleFormat = SAMPLEFORMAT_INT;
  }
  const uint16_t colorSamples = s.photometric == PHOTOMETRIC_RGB ? 3 : 1;

  bool ok = true;
  for (uint32_t page = 0; page < s.pages; ++page)
  {
    const bool strips = page < s.stripPages.size() && s.stripPages[page];

    TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, s.width);
    TIFFSetField(tif, TIFFTAG_IMAGELENGTH, s.height);
    TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, static_cast<uint16_t>(sizeof(T) * 8));
    TIFFSetField(tif, TIFFTAG_SAMPLEFORMAT, sampleFormat);
    TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, s.samplesPerPixel);
    TIFFSetField(tif, TIFFTAG_PLANARCONFIG, s.planarConfig);
    TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, s.photometric);
    TIFFSetField(tif, TIFFTAG_COMPRESSION, s.compression);
    TIFFSetField(tif, TIFFTAG_ORIENTATION, s.orientation);
    if (s.samplesPerPixel > colorSamples && s.photometric != PHOTOMETRIC_PALETTE)
    {
      const std::vector<uint16_t> extraSamples(s.samplesPerPixel - colorSamples, EXTRASAMPLE_UNSPECIFIED);
      TIFFSetField(tif, TIFFTAG_EXTRASAMPLES, static_cast<uint16_t>(extraSamples.size()), extraSamples.data());
    }
    if (s.photometric == PHOTOMETRIC_PALETTE)
    {
      const size_t          paletteSize = size_t{ 1 } << (sizeof(T) * 8);
      std::vector<uint16_t> red(paletteSize);
      std::vector<uint16_t> green(paletteSize);
      std::vector<uint16_t> blue(paletteSize);
      for (size_t i = 0; i < paletteSize; ++i)
      {
        red[i] = static_cast<uint16_t>(i);
        green[i] = static_cast<uint16_t>(paletteSize - 1 - i);
        blue[i] = static_cast<uint16_t>((i * 3) & (paletteSize - 1));
      }
      TIFFSetField(tif, TIFFTAG_COLORMAP, red.data(), green.data(), blue.data());
    }

    if (strips)
    {
      TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, s.height);
      std::vector<T> row(static_cast<size_t>(s.width) * s.samplesPerPixel);
      for (uint32_t y = 0; y < s.height; ++y)
      {
        for (uint32_t x = 0; x < s.width; ++x)
        {
          for (uint32_t c = 0; c < s.samplesPerPixel; ++c)
          {
            row[x * s.samplesPerPixel + c] = TiledPixel<T>(x, y, c, page);
          }
        }
        ok = ok && TIFFWriteScanline(tif, row.data(), y, 0) >= 0;
      }
    }
    else
    {
      TIFFSetField(tif, TIFFTAG_TILEWIDTH, s.tileWidth);
      TIFFSetField(tif, TIFFTAG_TILELENGTH, s.tileHeight);
      const uint32_t planes = s.planarConfig == PLANARCONFIG_SEPARATE ? s.samplesPerPixel : 1;
      const uint32_t samplesPerTilePixel = planes > 1 ? 1 : s.samplesPerPixel;
      for (uint32_t tileY = 0; tileY < s.height; tileY += s.tileHeight)
      {
        for (uint32_t tileX = 0; tileX < s.width; tileX += s.tileWidth)
        {
          for (uint32_t plane = 0; plane < planes; ++plane)
          {
            // Tiles are always stored full size; the part outside the image stays zero.
            std::vector<T> tile(static_cast<size_t>(s.tileWidth) * s.tileHeight * samplesPerTilePixel, T{});
            for (uint32_t ly = 0; ly < s.tileHeight && tileY + ly < s.height; ++ly)
            {
              for (uint32_t lx = 0; lx < s.tileWidth && tileX + lx < s.width; ++lx)
              {
                for (uint32_t k = 0; k < samplesPerTilePixel; ++k)
                {
                  tile[(static_cast<size_t>(ly) * s.tileWidth + lx) * samplesPerTilePixel + k] =
                    TiledPixel<T>(tileX + lx, tileY + ly, planes > 1 ? plane : k, page);
                }
              }
            }
            ok = ok && TIFFWriteTile(tif, tile.data(), tileX, tileY, 0, static_cast<uint16_t>(plane)) >= 0;
          }
        }
      }
    }
    ok = ok && TIFFWriteDirectory(tif) != 0;
  }
  TIFFClose(tif);
  return ok;
}

void
SetFullIORegion(itk::TIFFImageIO * io)
{
  const unsigned int dimension = io->GetNumberOfDimensions();
  itk::ImageIORegion ioRegion(dimension);
  for (unsigned int d = 0; d < dimension; ++d)
  {
    ioRegion.SetIndex(d, 0);
    ioRegion.SetSize(d, io->GetDimensions(d));
  }
  io->SetIORegion(ioRegion);
}

// Writes a tiled TIFF, reads it back through the native (non-RGBA) path, and checks every sample.
template <typename T>
void
VerifyTiledRead(const std::string & name, const TiledSpec & s, itk::IOComponentEnum expectedComponentType)
{
  const std::string fileName = TIFFImageIOGTestOutputPath(name);
  ASSERT_TRUE(WriteTiledImage<T>(fileName, s));

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();
  // The RGBA fallback would report 4 uchar components, so this also proves the native path was taken.
  ASSERT_EQ(tiffImageIO->GetComponentType(), expectedComponentType);
  ASSERT_EQ(tiffImageIO->GetNumberOfComponents(), s.samplesPerPixel);
  ASSERT_EQ(tiffImageIO->GetDimensions(0), s.width);
  ASSERT_EQ(tiffImageIO->GetDimensions(1), s.height);
  if (s.pages > 1)
  {
    ASSERT_EQ(tiffImageIO->GetNumberOfDimensions(), 3u);
    ASSERT_EQ(tiffImageIO->GetDimensions(2), s.pages);
  }
  SetFullIORegion(tiffImageIO);

  std::vector<T> buffer(static_cast<size_t>(s.width) * s.height * s.pages * s.samplesPerPixel);
  std::memset(buffer.data(), 0x5A, buffer.size() * sizeof(T));
  ASSERT_NO_THROW(tiffImageIO->Read(buffer.data()));

  size_t mismatches = 0;
  size_t firstMismatch = 0;
  size_t idx = 0;
  for (uint32_t page = 0; page < s.pages; ++page)
  {
    for (uint32_t row = 0; row < s.height; ++row)
    {
      const uint32_t storedRow = s.orientation == ORIENTATION_BOTLEFT ? s.height - 1 - row : row;
      for (uint32_t x = 0; x < s.width; ++x)
      {
        for (uint32_t c = 0; c < s.samplesPerPixel; ++c, ++idx)
        {
          T expected = TiledPixel<T>(x, storedRow, c, page);
          if constexpr (std::is_integral_v<T>)
          {
            if (s.photometric == PHOTOMETRIC_MINISWHITE)
            {
              expected = static_cast<T>(~expected);
            }
          }
          if (buffer[idx] != expected)
          {
            if (mismatches++ == 0)
            {
              firstMismatch = idx;
            }
          }
        }
      }
    }
  }
  EXPECT_EQ(mismatches, 0u) << "first mismatch at sample index " << firstMismatch;
}

} // namespace

TEST(TIFFImageIOGTest, ReadTiledGray8TileGridMatchesImage)
{
  TiledSpec s;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledGray8Aligned.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledGray8PartialEdgeTiles)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledGray8Edge.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledGray8PartialEdgeTilesInXOnly)
{
  TiledSpec s;
  s.width = 50;
  s.height = 32;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledGray8EdgeX.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledGray8PartialEdgeTilesInYOnly)
{
  TiledSpec s;
  s.width = 48;
  s.height = 37;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledGray8EdgeY.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledGray8TileLargerThanImage)
{
  TiledSpec s;
  s.width = 20;
  s.height = 10;
  s.tileWidth = 64;
  s.tileHeight = 32;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledGray8Large.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledGray8NonSquareTiles)
{
  TiledSpec s;
  s.width = 70;
  s.height = 40;
  s.tileWidth = 32;
  s.tileHeight = 16;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledGray8NonSquare.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledSignedChar)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  VerifyTiledRead<int8_t>("itkTIFFImageIOGTest_TiledInt8.tif", s, itk::IOComponentEnum::CHAR);
}

TEST(TIFFImageIOGTest, ReadTiledUnsignedShort)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  VerifyTiledRead<uint16_t>("itkTIFFImageIOGTest_TiledUInt16.tif", s, itk::IOComponentEnum::USHORT);
}

TEST(TIFFImageIOGTest, ReadTiledShort)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  VerifyTiledRead<int16_t>("itkTIFFImageIOGTest_TiledInt16.tif", s, itk::IOComponentEnum::SHORT);
}

TEST(TIFFImageIOGTest, ReadTiledUnsignedInt)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  VerifyTiledRead<uint32_t>("itkTIFFImageIOGTest_TiledUInt32.tif", s, itk::IOComponentEnum::UINT);
}

TEST(TIFFImageIOGTest, ReadTiledInt)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  VerifyTiledRead<int32_t>("itkTIFFImageIOGTest_TiledInt32.tif", s, itk::IOComponentEnum::INT);
}

TEST(TIFFImageIOGTest, ReadTiledFloat)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  VerifyTiledRead<float>("itkTIFFImageIOGTest_TiledFloat.tif", s, itk::IOComponentEnum::FLOAT);
}

TEST(TIFFImageIOGTest, ReadTiledRGB8)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.samplesPerPixel = 3;
  s.photometric = PHOTOMETRIC_RGB;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledRGB8.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledRGB16)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.samplesPerPixel = 3;
  s.photometric = PHOTOMETRIC_RGB;
  VerifyTiledRead<uint16_t>("itkTIFFImageIOGTest_TiledRGB16.tif", s, itk::IOComponentEnum::USHORT);
}

TEST(TIFFImageIOGTest, ReadTiledGrayWithExtraSample)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.samplesPerPixel = 2;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledGrayAlpha.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledMinIsWhiteIsInverted)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.photometric = PHOTOMETRIC_MINISWHITE;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledMinIsWhite8.tif", s, itk::IOComponentEnum::UCHAR);
  VerifyTiledRead<uint16_t>("itkTIFFImageIOGTest_TiledMinIsWhite16.tif", s, itk::IOComponentEnum::USHORT);
}

TEST(TIFFImageIOGTest, ReadTiledBottomLeftOrientation)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.orientation = ORIENTATION_BOTLEFT;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledBotLeft.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledCompressed)
{
  for (const uint16_t compression : { COMPRESSION_LZW, COMPRESSION_PACKBITS, COMPRESSION_ADOBE_DEFLATE })
  {
    TiledSpec s;
    s.width = 50;
    s.height = 37;
    s.compression = compression;
    VerifyTiledRead<uint16_t>(
      "itkTIFFImageIOGTest_TiledCompressed" + std::to_string(compression) + ".tif", s, itk::IOComponentEnum::USHORT);
  }
}

TEST(TIFFImageIOGTest, ReadTiledMultiPageVolume)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.pages = 3;
  VerifyTiledRead<uint16_t>("itkTIFFImageIOGTest_TiledVolume.tif", s, itk::IOComponentEnum::USHORT);
}

TEST(TIFFImageIOGTest, ReadMixedTiledAndStrippedPagesVolume)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.pages = 3;
  s.stripPages = { false, true, false };
  VerifyTiledRead<uint16_t>("itkTIFFImageIOGTest_MixedTiledStripVolume.tif", s, itk::IOComponentEnum::USHORT);
}

namespace
{
// Palette image whose colormap is red=i, green=max-i, blue=3i mod (max+1), read expanded and as scalar indices.
template <typename T>
void
VerifyTiledPalette(const std::string & name, itk::IOComponentEnum expectedComponentType)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.photometric = PHOTOMETRIC_PALETTE;
  const std::string fileName = TIFFImageIOGTestOutputPath(name);
  ASSERT_TRUE(WriteTiledImage<T>(fileName, s));
  constexpr size_t paletteMask = (size_t{ 1 } << (sizeof(T) * 8)) - 1;

  for (const bool expand : { true, false })
  {
    auto tiffImageIO = itk::TIFFImageIO::New();
    tiffImageIO->SetFileName(fileName);
    tiffImageIO->SetExpandRGBPalette(expand);
    tiffImageIO->ReadImageInformation();
    ASSERT_EQ(tiffImageIO->GetComponentType(), expectedComponentType);
    ASSERT_EQ(tiffImageIO->GetNumberOfComponents(), expand ? 3u : 1u);
    SetFullIORegion(tiffImageIO);

    std::vector<T> buffer(static_cast<size_t>(s.width) * s.height * (expand ? 3 : 1));
    ASSERT_NO_THROW(tiffImageIO->Read(buffer.data()));

    size_t mismatches = 0;
    size_t idx = 0;
    for (uint32_t y = 0; y < s.height; ++y)
    {
      for (uint32_t x = 0; x < s.width; ++x)
      {
        const T index = TiledPixel<T>(x, y, 0, 0);
        mismatches += buffer[idx++] != index;
        if (expand)
        {
          mismatches += buffer[idx++] != static_cast<T>(paletteMask - index);
          mismatches += buffer[idx++] != static_cast<T>((index * 3u) & paletteMask);
        }
      }
    }
    EXPECT_EQ(mismatches, 0u) << "expandRGBPalette=" << expand;
  }
}
} // namespace

TEST(TIFFImageIOGTest, ReadTiledPalette8)
{
  VerifyTiledPalette<unsigned char>("itkTIFFImageIOGTest_TiledPalette8.tif", itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledPalette16)
{
  VerifyTiledPalette<uint16_t>("itkTIFFImageIOGTest_TiledPalette16.tif", itk::IOComponentEnum::USHORT);
}

TEST(TIFFImageIOGTest, ReadTiledMinIsWhiteWideAndSignedTypes)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.photometric = PHOTOMETRIC_MINISWHITE;
  VerifyTiledRead<uint32_t>("itkTIFFImageIOGTest_TiledMinIsWhite32.tif", s, itk::IOComponentEnum::UINT);
  VerifyTiledRead<int16_t>("itkTIFFImageIOGTest_TiledMinIsWhiteInt16.tif", s, itk::IOComponentEnum::SHORT);
}

TEST(TIFFImageIOGTest, ReadTiledPlanarSeparateSingleSample)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.planarConfig = PLANARCONFIG_SEPARATE;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledSeparateOneSample.tif", s, itk::IOComponentEnum::UCHAR);
}

TEST(TIFFImageIOGTest, ReadTiledSingleRowAndSingleColumnImages)
{
  TiledSpec s;
  s.width = 1;
  s.height = 40;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledOneColumn.tif", s, itk::IOComponentEnum::UCHAR);
  s.width = 40;
  s.height = 1;
  VerifyTiledRead<unsigned char>("itkTIFFImageIOGTest_TiledOneRow.tif", s, itk::IOComponentEnum::UCHAR);
}

// A tile that fails to decode must raise an exception, not leave unwritten data in the output.
TEST(TIFFImageIOGTest, ReadTiledCorruptTileDataThrows)
{
  TiledSpec s;
  s.width = 64;
  s.height = 64;
  s.compression = COMPRESSION_LZW;
  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_TiledCorrupt.tif");
  ASSERT_TRUE(WriteTiledImage<unsigned char>(fileName, s));

  // Tile data follows the 8-byte header; an all-ones LZW stream is an invalid code sequence.
  std::vector<unsigned char> bytes;
  {
    std::ifstream in(fileName, std::ios::binary);
    bytes.assign(std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
  }
  ASSERT_GT(bytes.size(), 72u);
  std::fill_n(bytes.begin() + 8, 64, static_cast<unsigned char>(0xFF));
  {
    std::ofstream out(fileName, std::ios::binary);
    out.write(reinterpret_cast<const char *>(bytes.data()), static_cast<std::streamsize>(bytes.size()));
  }

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();
  SetFullIORegion(tiffImageIO);

  std::vector<unsigned char> buffer(static_cast<size_t>(s.width) * s.height);
  EXPECT_THROW(tiffImageIO->Read(buffer.data()), itk::ExceptionObject);
}

// Layouts the native reader does not handle must keep using the RGBA fallback.
TEST(TIFFImageIOGTest, ReadTiledPlanarSeparateRGBUsesRGBAFallback)
{
  TiledSpec s;
  s.width = 50;
  s.height = 37;
  s.samplesPerPixel = 3;
  s.photometric = PHOTOMETRIC_RGB;
  s.planarConfig = PLANARCONFIG_SEPARATE;
  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_TiledPlanarSeparate.tif");
  ASSERT_TRUE(WriteTiledImage<unsigned char>(fileName, s));

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();
  ASSERT_EQ(tiffImageIO->GetComponentType(), itk::IOComponentEnum::UCHAR);
  ASSERT_EQ(tiffImageIO->GetNumberOfComponents(), 4u);
  SetFullIORegion(tiffImageIO);

  std::vector<unsigned char> buffer(static_cast<size_t>(s.width) * s.height * 4);
  ASSERT_NO_THROW(tiffImageIO->Read(buffer.data()));

  size_t mismatches = 0;
  size_t idx = 0;
  for (uint32_t y = 0; y < s.height; ++y)
  {
    for (uint32_t x = 0; x < s.width; ++x)
    {
      for (uint32_t c = 0; c < 3; ++c)
      {
        mismatches += buffer[idx++] != TiledPixel<unsigned char>(x, y, c, 0);
      }
      mismatches += buffer[idx++] != 255;
    }
  }
  EXPECT_EQ(mismatches, 0u);
}

TEST(TIFFImageIOGTest, ReadTiledOneBitUsesRGBAFallback)
{
  constexpr uint32_t width = 20;
  constexpr uint32_t height = 20;
  constexpr uint32_t tileSize = 16;

  const std::string fileName = TIFFImageIOGTestOutputPath("itkTIFFImageIOGTest_TiledOneBit.tif");
  TIFF *            tif = TIFFOpen(fileName.c_str(), "w");
  ASSERT_NE(tif, nullptr);
  TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, width);
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, height);
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 1);
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField(tif, TIFFTAG_TILEWIDTH, tileSize);
  TIFFSetField(tif, TIFFTAG_TILELENGTH, tileSize);
  for (uint32_t tileY = 0; tileY < height; tileY += tileSize)
  {
    for (uint32_t tileX = 0; tileX < width; tileX += tileSize)
    {
      std::vector<unsigned char> tile(tileSize / 8 * tileSize, 0);
      for (uint32_t ly = 0; ly < tileSize; ++ly)
      {
        for (uint32_t lx = 0; lx < tileSize; ++lx)
        {
          if (((tileX + lx + tileY + ly) & 1u) != 0)
          {
            tile[ly * (tileSize / 8) + lx / 8] |= static_cast<unsigned char>(0x80u >> (lx % 8));
          }
        }
      }
      ASSERT_GE(TIFFWriteTile(tif, tile.data(), tileX, tileY, 0, 0), 0);
    }
  }
  TIFFWriteDirectory(tif);
  TIFFClose(tif);

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(fileName);
  tiffImageIO->ReadImageInformation();
  ASSERT_EQ(tiffImageIO->GetNumberOfComponents(), 4u);
  SetFullIORegion(tiffImageIO);

  std::vector<unsigned char> buffer(static_cast<size_t>(width) * height * 4);
  ASSERT_NO_THROW(tiffImageIO->Read(buffer.data()));

  size_t mismatches = 0;
  for (uint32_t y = 0; y < height; ++y)
  {
    for (uint32_t x = 0; x < width; ++x)
    {
      const unsigned char gray = ((x + y) & 1u) != 0 ? 255 : 0;
      const size_t        idx = (static_cast<size_t>(y) * width + x) * 4;
      mismatches += buffer[idx] != gray || buffer[idx + 1] != gray || buffer[idx + 2] != gray || buffer[idx + 3] != 255;
    }
  }
  EXPECT_EQ(mismatches, 0u);
}
