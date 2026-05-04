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

// Pixelwise regression tests against the .vti fixtures produced by
// Modules/IO/VTK/test/generate_vti_fixtures.py.  Each fixture has a
// matching MetaIO (.mhd / .raw) oracle containing the same logical
// image; the test reads both files and asserts pixel equality.
//
// These tests exercise the VTIImageIO *reader* against output from an
// independent Python writer (no ITK code is involved in generating the
// fixtures), which catches mismatches between the reader's
// interpretation of the VTK XML format and the format as specified.
// The companion round-trip scenarios in itkVTIImageIOTest only verify
// writer/reader self-consistency; this test closes the loop at the
// format boundary.

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkRGBAPixel.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkVTIImageIO.h"
#include "itkVector.h"
#include "itkGTest.h"
#include "itkTestDriverIncludeRequiredFactories.h"

#include <cmath>
#include <filesystem>
#include <string>

#ifndef VTI_TEST_INPUT_DIR
#  error "VTI_TEST_INPUT_DIR must be defined by the build system."
#endif

namespace
{
struct VTIImageIOGeneratedFixtures : public ::testing::Test
{
  void
  SetUp() override
  {
    RegisterRequiredFactories();
  }
};

template <typename TImage, typename TEqual>
::testing::AssertionResult
ImagesEqual(TImage * vti, TImage * mhd, TEqual && pixelEqual)
{
  if (vti->GetLargestPossibleRegion() != mhd->GetLargestPossibleRegion())
  {
    return ::testing::AssertionFailure() << "region mismatch " << vti->GetLargestPossibleRegion() << " vs "
                                         << mhd->GetLargestPossibleRegion();
  }
  itk::ImageRegionConstIteratorWithIndex<TImage> vIt(vti, vti->GetLargestPossibleRegion());
  itk::ImageRegionConstIteratorWithIndex<TImage> mIt(mhd, mhd->GetLargestPossibleRegion());
  for (vIt.GoToBegin(), mIt.GoToBegin(); !vIt.IsAtEnd(); ++vIt, ++mIt)
  {
    if (!pixelEqual(vIt.Get(), mIt.Get()))
    {
      return ::testing::AssertionFailure()
             << "pixel mismatch at " << vIt.GetIndex() << ": vti=" << vIt.Get() << " mhd=" << mIt.Get();
    }
  }
  return ::testing::AssertionSuccess();
}

template <typename TImage, typename TEqual>
void
CompareVtiToMhd(const std::string & label,
                const std::string & vtiFixture,
                const std::string & mhdFixture,
                TEqual &&           pixelEqual,
                bool                tryCompression = true)
{
  using ReaderType = itk::ImageFileReader<TImage>;
  const std::string vtiPath = std::string(VTI_TEST_INPUT_DIR) + "/" + vtiFixture;
  const std::string mhdPath = std::string(VTI_TEST_INPUT_DIR) + "/" + mhdFixture;

  auto vtiReader = ReaderType::New();
  vtiReader->SetFileName(vtiPath);
  vtiReader->SetImageIO(itk::VTIImageIO::New());
  ASSERT_NO_THROW(vtiReader->Update());

  auto mhdReader = ReaderType::New();
  mhdReader->SetFileName(mhdPath);
  ASSERT_NO_THROW(mhdReader->Update());

  auto * vti = vtiReader->GetOutput();
  auto * mhd = mhdReader->GetOutput();
  EXPECT_TRUE(ImagesEqual(vti, mhd, pixelEqual)) << "fixture vs MHD oracle: " << vtiPath << " vs " << mhdPath;

  if (tryCompression)
  {
    // Write a compressed VTI from the just-read image and compare it to the
    // same MHD oracle: exercises the writer's ZLib appended-raw path against
    // the same independent MHD-layout reference.
    const std::filesystem::path tempDir = std::filesystem::temp_directory_path() / "itkVTICompressed";
    std::filesystem::create_directories(tempDir);
    const std::string compressedVtiPath = (tempDir / (label + ".vti")).string();
    ASSERT_NO_THROW(itk::WriteImage(vti, compressedVtiPath, true));
    auto cvti = itk::ReadImage<TImage>(compressedVtiPath);
    EXPECT_TRUE(ImagesEqual(cvti.GetPointer(), mhd, pixelEqual))
      << "compressed-write round-trip: " << compressedVtiPath << " vs " << mhdPath;
  }
}

template <typename T>
bool
ExactEqual(const T & a, const T & b)
{
  return a == b;
}

bool
FloatAlmostEqual(float a, float b)
{
  return std::abs(a - b) <= 1.0e-6f * std::max(1.0f, std::abs(a));
}

bool
TensorAlmostEqual(const itk::SymmetricSecondRankTensor<float, 3> & a,
                  const itk::SymmetricSecondRankTensor<float, 3> & b)
{
  for (unsigned int i = 0; i < 6; ++i)
  {
    if (std::abs(a[i] - b[i]) > 1.0e-6f * std::max(1.0f, std::abs(a[i])))
    {
      return false;
    }
  }
  return true;
}

bool
Vector3AlmostEqual(const itk::Vector<float, 3> & a, const itk::Vector<float, 3> & b)
{
  for (unsigned int i = 0; i < 3; ++i)
  {
    if (std::abs(a[i] - b[i]) > 1.0e-6f * std::max(1.0f, std::abs(a[i])))
    {
      return false;
    }
  }
  return true;
}
} // namespace

// Fixture A: UInt8 scalar, appended-raw (uncompressed), UInt64 header.
TEST_F(VTIImageIOGeneratedFixtures, ScalarU8AppendedRaw)
{
  CompareVtiToMhd<itk::Image<unsigned char, 3>>("scalar_u8_appended_raw",
                                                "VTI_scalar_u8_appended_raw.vti",
                                                "VTI_scalar_u8_appended_raw.mhd",
                                                ExactEqual<unsigned char>);
}

// Fixture B: Float32 scalar, ZLib appended-raw, UInt64 header.
TEST_F(VTIImageIOGeneratedFixtures, ScalarF32ZLibAppended)
{
  CompareVtiToMhd<itk::Image<float, 3>>("scalar_f32_zlib_appended",
                                        "VTI_scalar_f32_zlib_appended.vti",
                                        "VTI_scalar_f32_zlib_appended.mhd",
                                        FloatAlmostEqual);
}

// Fixture C: RGBA<UInt8> 4-component, appended-raw.
TEST_F(VTIImageIOGeneratedFixtures, RgbaU8AppendedRaw)
{
  CompareVtiToMhd<itk::Image<itk::RGBAPixel<unsigned char>, 3>>("rgba_u8_appended_raw",
                                                                "VTI_rgba_u8_appended_raw.vti",
                                                                "VTI_rgba_u8_appended_raw.mhd",
                                                                ExactEqual<itk::RGBAPixel<unsigned char>>);
}

// Fixture C2: Vector<Float32,3>, ZLib appended-raw, UInt64 header.  Covers
// the same code path (multi-component Float32 ZLib decompression of
// appended-raw data) that the upstream-broken
// itkVTIImageIOReadWriteTestVHFColorZLib test would exercise if its
// ParaView-produced fixture were available via ExternalData (blocked on
// ITK #4340).  The fixture's <PointData Vectors="vectors"> hint drives
// the reader to IOPixelEnum::VECTOR dispatch.
TEST_F(VTIImageIOGeneratedFixtures, Vector3F32ZLibAppended)
{
  CompareVtiToMhd<itk::Image<itk::Vector<float, 3>, 3>>("vector3_f32_zlib_appended",
                                                        "VTI_vector3_f32_zlib_appended.vti",
                                                        "VTI_vector3_f32_zlib_appended.mhd",
                                                        Vector3AlmostEqual);
}

// Fixture D: symmetric tensor Float32, ASCII.  VTI layout is VTK-canonical
// [XX, YY, ZZ, XY, YZ, XZ]; reader remaps to ITK's [e00, e01, e02, e11,
// e12, e22] so the in-memory tensor matches the MHD oracle's ITK layout.
// Compression skipped because F-007 (binary tensor write) is deferred.
TEST_F(VTIImageIOGeneratedFixtures, TensorF32Ascii)
{
  CompareVtiToMhd<itk::Image<itk::SymmetricSecondRankTensor<float, 3>, 3>>("tensor_f32_ascii",
                                                                           "VTI_tensor_f32_ascii.vti",
                                                                           "VTI_tensor_f32_ascii.mhd",
                                                                           TensorAlmostEqual,
                                                                           /*tryCompression=*/false);
}
