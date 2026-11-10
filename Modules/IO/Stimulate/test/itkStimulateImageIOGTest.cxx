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

#include "itkStimulateImageIO.h"
#include "itkGTest.h"

#include <array>
#include <filesystem>
#include <string>
#include <system_error>

namespace
{
class StimulateImageIOPathTest : public ::testing::TestWithParam<const char *>
{
protected:
  void
  SetUp() override
  {
    const auto temporaryRoot = std::filesystem::temp_directory_path();
    for (unsigned int attempt = 0; attempt < 10000; ++attempt)
    {
      const auto directory = temporaryRoot / ("itk-stimulate-" + std::to_string(attempt));
      if (std::filesystem::create_directory(directory))
      {
        m_Directory = directory;
        return;
      }
    }
    FAIL() << "Could not reserve a temporary directory";
  }

  void
  TearDown() override
  {
    if (!m_Directory.empty())
    {
      std::error_code error;
      std::filesystem::remove_all(m_Directory, error);
      EXPECT_FALSE(error) << error.message();
    }
  }

  std::filesystem::path m_Directory;
};

TEST_P(StimulateImageIOPathTest, RoundTripPreservesPixelsAndGeometry)
{
  const auto directory = m_Directory / GetParam();
  ASSERT_TRUE(std::filesystem::create_directory(directory));
  const auto fileName = (directory / "image.spr").string();

  const std::array<short, 16> pixels{ -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7 };
  const auto                  writer = itk::StimulateImageIO::New();
  writer->SetFileName(fileName);
  writer->SetNumberOfDimensions(2);
  writer->SetDimensions(0, 4);
  writer->SetDimensions(1, 4);
  writer->SetOrigin(0, -1.25);
  writer->SetOrigin(1, 2.5);
  writer->SetSpacing(0, 0.5);
  writer->SetSpacing(1, 1.25);
  writer->SetPixelType(itk::IOPixelEnum::SCALAR);
  writer->SetComponentType(itk::IOComponentEnum::SHORT);
  writer->SetNumberOfComponents(1);
  ASSERT_NO_THROW(writer->Write(pixels.data()));

  const auto reader = itk::StimulateImageIO::New();
  reader->SetFileName(fileName);
  ASSERT_NO_THROW(reader->ReadImageInformation());
  ASSERT_EQ(reader->GetNumberOfDimensions(), 2);
  ASSERT_EQ(reader->GetDimensions(0), 4);
  ASSERT_EQ(reader->GetDimensions(1), 4);
  ASSERT_EQ(reader->GetComponentType(), itk::IOComponentEnum::SHORT);
  ASSERT_EQ(reader->GetNumberOfComponents(), 1);
  EXPECT_DOUBLE_EQ(reader->GetOrigin(0), -1.25);
  EXPECT_DOUBLE_EQ(reader->GetOrigin(1), 2.5);
  EXPECT_DOUBLE_EQ(reader->GetSpacing(0), 0.5);
  EXPECT_DOUBLE_EQ(reader->GetSpacing(1), 1.25);

  std::array<short, 16> restored{};
  ASSERT_NO_THROW(reader->Read(restored.data()));
  EXPECT_EQ(restored, pixels);
}

INSTANTIATE_TEST_SUITE_P(HeaderWords,
                         StimulateImageIOPathTest,
                         ::testing::Values("plain",
                                           "numDim",
                                           "dim",
                                           "origin",
                                           "extent",
                                           "fov",
                                           "interval",
                                           "dataType",
                                           "displayRange",
                                           "fidName",
                                           "sdtOrient",
                                           "dsplyThres",
                                           "endian",
                                           "mapParmFileName",
                                           "mapTypeName",
                                           "stimFileName",
                                           "mapConf",
                                           "periodStr",
                                           "dim_fov_path",
                                           "dimensions",
                                           "dimitri",
                                           "fovea"));
} // namespace
