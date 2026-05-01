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
#include "itkTestingMacros.h"
#include "itkVTIImageIO.h"
#include "itkVector.h"

#include <cmath>
#include <string>
#include <filesystem>

namespace
{
template <typename TImage, typename TEqual>
int
CompareImages(TImage * vti, TImage * mhd, TEqual && pixelEqual)
{
  if (vti->GetLargestPossibleRegion() != mhd->GetLargestPossibleRegion())
  {
    std::cerr << "FAILED: region mismatch " << vti->GetLargestPossibleRegion() << " vs "
              << mhd->GetLargestPossibleRegion() << std::endl;
    return EXIT_FAILURE;
  }

  itk::ImageRegionConstIteratorWithIndex<TImage> vIt(vti, vti->GetLargestPossibleRegion());
  itk::ImageRegionConstIteratorWithIndex<TImage> mIt(mhd, mhd->GetLargestPossibleRegion());
  for (vIt.GoToBegin(), mIt.GoToBegin(); !vIt.IsAtEnd(); ++vIt, ++mIt)
  {
    if (!pixelEqual(vIt.Get(), mIt.Get()))
    {
      std::cerr << "FAILED: pixel mismatch at " << vIt.GetIndex() << ": vti=" << vIt.Get() << " mhd=" << mIt.Get()
                << std::endl;
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}

template <typename TImage, typename TEqual>
int
CompareVtiToMhd(const std::string & label,
                const std::string & vtiPath,
                const std::string & mhdPath,
                TEqual &&           pixelEqual,
                bool                tryCompression = true)
{
  std::cout << label << std::endl;
  using ReaderType = itk::ImageFileReader<TImage>;

  auto vtiReader = ReaderType::New();
  vtiReader->SetFileName(vtiPath);
  vtiReader->SetImageIO(itk::VTIImageIO::New());
  ITK_TRY_EXPECT_NO_EXCEPTION(vtiReader->Update());

  auto mhdReader = ReaderType::New();
  mhdReader->SetFileName(mhdPath);
  ITK_TRY_EXPECT_NO_EXCEPTION(mhdReader->Update());

  auto vti = vtiReader->GetOutput();
  auto mhd = mhdReader->GetOutput();

  if (CompareImages(vti, mhd, pixelEqual) != EXIT_SUCCESS)
  {
    return EXIT_FAILURE;
  }
  std::cout << "OK: " << vtiPath << " matches " << mhdPath << std::endl;

  if (tryCompression)
  {
    // Now write a compressed VTI and compare it to the same MHD image
    const std::filesystem::path tempDir = std::filesystem::temp_directory_path() / "itkVTICompressed";
    std::filesystem::create_directories(tempDir); // Make sure the temp directory exists
    const std::string compressedVtiPath = (tempDir / (label + ".vti")).string();
    itk::WriteImage(vti, compressedVtiPath, true);
    auto cvti = itk::ReadImage<TImage>(compressedVtiPath);
    if (CompareImages(cvti.GetPointer(), mhd, pixelEqual) != EXIT_SUCCESS)
    {
      return EXIT_FAILURE;
    }
    std::cout << "OK: " << compressedVtiPath << " matches " << mhdPath << std::endl;
  }
  else
  {
    std::cout << "Skipped compression test" << std::endl;
  }

  return EXIT_SUCCESS;
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

int
itkVTIImageIOGeneratedFixturesTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <inputDir>" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string inputDir = argv[1];
  int               status = EXIT_SUCCESS;

  // Fixture A: UInt8 scalar, appended-raw (uncompressed), UInt64 header.
  status |= CompareVtiToMhd<itk::Image<unsigned char, 3>>("scalar_u8_appended_raw",
                                                          inputDir + "/VTI_scalar_u8_appended_raw.vti",
                                                          inputDir + "/VTI_scalar_u8_appended_raw.mhd",
                                                          ExactEqual<unsigned char>);

  // Fixture B: Float32 scalar, ZLib appended-raw, UInt64 header.
  status |= CompareVtiToMhd<itk::Image<float, 3>>("scalar_f32_zlib_appended",
                                                  inputDir + "/VTI_scalar_f32_zlib_appended.vti",
                                                  inputDir + "/VTI_scalar_f32_zlib_appended.mhd",
                                                  FloatAlmostEqual);

  // Fixture C: RGBA<UInt8> 4-component, appended-raw.
  status |= CompareVtiToMhd<itk::Image<itk::RGBAPixel<unsigned char>, 3>>("rgba_u8_appended_raw",
                                                                          inputDir + "/VTI_rgba_u8_appended_raw.vti",
                                                                          inputDir + "/VTI_rgba_u8_appended_raw.mhd",
                                                                          ExactEqual<itk::RGBAPixel<unsigned char>>);

  // Fixture C2: Vector<Float32,3> 3-component, ZLib appended-raw, UInt64
  // header.  Covers the same code path (multi-component Float32 ZLib
  // decompression of appended-raw data) that the upstream-broken
  // itkVTIImageIOReadWriteTestVHFColorZLib test would exercise if its
  // ParaView-produced fixture were available via ExternalData
  // (blocked on ITK #4340).  The fixture's <PointData Vectors="vectors">
  // hint drives the reader to IOPixelEnum::VECTOR dispatch.
  status |= CompareVtiToMhd<itk::Image<itk::Vector<float, 3>, 3>>("vector3_f32_zlib_appended",
                                                                  inputDir + "/VTI_vector3_f32_zlib_appended.vti",
                                                                  inputDir + "/VTI_vector3_f32_zlib_appended.mhd",
                                                                  Vector3AlmostEqual);

  // Fixture D: symmetric tensor Float32, ASCII.  VTI layout is VTK-canonical
  // [XX, YY, ZZ, XY, YZ, XZ]; reader remaps to ITK's [e00, e01, e02, e11,
  // e12, e22] so the in-memory tensor matches the MHD oracle's ITK layout.
  status |=
    CompareVtiToMhd<itk::Image<itk::SymmetricSecondRankTensor<float, 3>, 3>>("tensor_f32_ascii",
                                                                             inputDir + "/VTI_tensor_f32_ascii.vti",
                                                                             inputDir + "/VTI_tensor_f32_ascii.mhd",
                                                                             TensorAlmostEqual,
                                                                             false);

  if (status == EXIT_SUCCESS)
  {
    std::cout << "itkVTIImageIOGeneratedFixturesTest PASSED" << std::endl;
  }
  return status;
}
