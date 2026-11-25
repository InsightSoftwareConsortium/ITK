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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkTIFFImageIO.h"

#include "itkGTest.h"
#include "itksys/SystemTools.hxx"
#include "itkTestDriverIncludeRequiredFactories.h"
#include <string>

// Regression test for 32-bit integer (uint32/int32) TIFF support.
//
// libtiff supports 32-bit integer samples (BITSPERSAMPLE=32 with
// SAMPLEFORMAT_UINT / SAMPLEFORMAT_INT), and TIFFImageIO already identified
// them on read.  However, TIFFImageIO::InternalWrite() previously threw for
// these component types, and the pixel-reading dispatch left the output buffer
// zero-filled, so a uint32/int32 image could neither be written nor read back.
// These tests write an image with values that exceed the 16-bit range and
// verify that it round-trips with full 32-bit fidelity, in both 2-D and
// multi-page (3-D) TIFFs.

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{

struct ITKIOTIFFInt32 : public ::testing::Test
{
  void
  SetUp() override
  {
    RegisterRequiredFactories();
    // Per-test directory: tests run as concurrent CTest processes; a shared one is removed by a sibling's TearDown().
    m_TempDir = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/TIFFImageIOInt32-" +
                ::testing::UnitTest::GetInstance()->current_test_info()->name();
    itksys::SystemTools::MakeDirectory(m_TempDir);
  }

  void
  TearDown() override
  {
    itksys::SystemTools::RemoveADirectory(m_TempDir);
  }

  std::string m_TempDir;

  // A deterministic sample value for pixel linear index `i`. The stride is
  // chosen so values run well past the 16-bit range (65535), exercising all
  // four bytes of a 32-bit sample.
  template <typename PixelType>
  static PixelType
  SampleValue(size_t i)
  {
    const auto magnitude = static_cast<PixelType>(i * 100003 + 70000);
    if constexpr (std::is_signed_v<PixelType>)
    {
      return (i % 2 == 0) ? magnitude : static_cast<PixelType>(-magnitude);
    }
    else
    {
      return magnitude;
    }
  }

  // Write a synthetic image of the requested component type to a TIFF file,
  // read it back, and assert full-fidelity round-tripping. Also assert that
  // the on-disk component type matches, so a write that silently changed the
  // sample format would be caught.
  template <typename PixelType, unsigned int Dimension>
  void
  RoundTrip(const typename itk::Image<PixelType, Dimension>::SizeType & size,
            itk::IOComponentEnum                                        expectedComponentType,
            const std::string &                                         fileName)
  {
    using ImageType = itk::Image<PixelType, Dimension>;

    auto                           image = ImageType::New();
    typename ImageType::RegionType region;
    region.SetSize(size);
    image->SetRegions(region);
    image->Allocate();

    size_t i = 0;
    for (itk::ImageRegionIterator<ImageType> it(image, region); !it.IsAtEnd(); ++it, ++i)
    {
      it.Set(SampleValue<PixelType>(i));
    }

    const std::string filePath = m_TempDir + '/' + fileName;

    auto writer = itk::ImageFileWriter<ImageType>::New();
    writer->SetImageIO(itk::TIFFImageIO::New());
    writer->SetFileName(filePath);
    writer->SetInput(image);
    ASSERT_NO_THROW(writer->Update());

    // The written file must advertise the expected 32-bit integer sample format.
    auto readerIO = itk::TIFFImageIO::New();
    readerIO->SetFileName(filePath);
    readerIO->ReadImageInformation();
    EXPECT_EQ(readerIO->GetComponentType(), expectedComponentType);

    auto reader = itk::ImageFileReader<ImageType>::New();
    reader->SetImageIO(itk::TIFFImageIO::New());
    reader->SetFileName(filePath);
    ASSERT_NO_THROW(reader->Update());

    typename ImageType::Pointer readImage = reader->GetOutput();

    EXPECT_EQ(readImage->GetLargestPossibleRegion().GetSize(), size);

    i = 0;
    itk::ImageRegionConstIterator<ImageType> it(readImage, readImage->GetLargestPossibleRegion());
    for (; !it.IsAtEnd(); ++it, ++i)
    {
      ASSERT_EQ(it.Get(), SampleValue<PixelType>(i)) << "Pixel " << i << " mismatch";
    }
  }
};

} // namespace

TEST_F(ITKIOTIFFInt32, UInt32RoundTrip2D)
{
  itk::Image<uint32_t, 2>::SizeType size = { { 5, 4 } };
  this->RoundTrip<uint32_t, 2>(size, itk::IOComponentEnum::UINT, "uint32_2d.tif");
}

TEST_F(ITKIOTIFFInt32, Int32RoundTrip2D)
{
  itk::Image<int32_t, 2>::SizeType size = { { 5, 4 } };
  this->RoundTrip<int32_t, 2>(size, itk::IOComponentEnum::INT, "int32_2d.tif");
}

TEST_F(ITKIOTIFFInt32, UInt32RoundTrip3D)
{
  // A multi-page (3-D) image exercises the per-page pixel offset arithmetic in
  // the reader, which must use a correctly typed pointer for 32-bit samples.
  itk::Image<uint32_t, 3>::SizeType size = { { 4, 3, 3 } };
  this->RoundTrip<uint32_t, 3>(size, itk::IOComponentEnum::UINT, "uint32_3d.tif");
}

TEST_F(ITKIOTIFFInt32, Int32RoundTrip3D)
{
  itk::Image<int32_t, 3>::SizeType size = { { 4, 3, 3 } };
  this->RoundTrip<int32_t, 3>(size, itk::IOComponentEnum::INT, "int32_3d.tif");
}
