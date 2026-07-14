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
TEST(TIFFImageIOGTest, ReadVolumeExcludesUntaggedPageWhenSubFileTypeIsUsed)
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

  EXPECT_EQ(buffer[0 * sliceBytes], 10) << "untagged page must not consume a slice slot";
  EXPECT_EQ(buffer[1 * sliceBytes], 20) << "second SUBFILETYPE==0 page must land at slice 1";
  EXPECT_EQ(buffer[2 * sliceBytes], sentinel) << "read must not write past the logical volume depth";
}
