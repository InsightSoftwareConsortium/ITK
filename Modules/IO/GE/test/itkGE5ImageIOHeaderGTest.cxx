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

#include "itkGE5ImageIO.h"
#include "itkGEImageHeader.h"
#include "itkByteSwapper.h"
#include "itkGTest.h"

#include <array>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <memory>
#include <random>
#include <stdexcept>
#include <string>
#include <system_error>
#include <vector>

namespace
{
class GE5HeaderReader : public itk::GE5ImageIO
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GE5HeaderReader);

  using Self = GE5HeaderReader;
  using Superclass = itk::GE5ImageIO;
  using Pointer = itk::SmartPointer<Self>;

  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(GE5HeaderReader);

  using Superclass::ReadHeader;

protected:
  GE5HeaderReader() = default;
  ~GE5HeaderReader() override = default;
};

using FileBytes = std::array<unsigned char, 8192>;
constexpr std::size_t  ExamLengthOffset = 136;
constexpr std::size_t  SeriesLengthOffset = 144;
constexpr std::size_t  ImageLengthOffset = 152;
constexpr std::int32_t ExamMinimumLength = 308;
constexpr std::int32_t SeriesMinimumLength = 16;
constexpr std::int32_t MRMinimumLength = 400;
constexpr std::int32_t CTMinimumLength = 190;
constexpr std::int32_t PixelDataOffset = 6144;

enum class HeaderLayout
{
  Minimum,
  Padded,
  Version2,
  Headerless
};

template <typename TValue>
void
PutBigEndian(FileBytes & bytes, const std::size_t offset, TValue value)
{
  itk::ByteSwapper<TValue>::SwapFromSystemToBigEndian(&value);
  std::memcpy(bytes.data() + offset, &value, sizeof(value));
}

template <std::size_t VSize>
void
PutText(FileBytes & bytes, const std::size_t offset, const char (&text)[VSize])
{
  std::memcpy(bytes.data() + offset, text, VSize);
}

FileBytes
MakeHeader(const bool isCT, const HeaderLayout layout)
{
  FileBytes         bytes{};
  const bool        version2 = layout == HeaderLayout::Version2;
  const bool        headerless = layout == HeaderLayout::Headerless;
  const std::size_t exam = headerless ? 114 : (version2 ? 240 : 512);
  const std::size_t series = headerless ? 1138 : (version2 ? 1280 : 1792);
  const std::size_t image = headerless ? 2158 : (version2 ? 2308 : 3072);
  const auto        versionOffset = [version2](const std::size_t version3Offset, const std::size_t version2Offset) {
    return version2 ? version2Offset : version3Offset;
  };

  if (headerless)
  {
    PutText(bytes, 7, "SIGNA");
  }
  else
  {
    PutBigEndian(bytes, 0, std::int32_t{ 0x494d4746 });
    PutBigEndian(bytes, 4, PixelDataOffset);
    PutBigEndian(bytes, 8, std::int32_t{ 4 });
    PutBigEndian(bytes, 12, std::int32_t{ 6 });
    PutBigEndian(bytes, 16, std::int32_t{ 16 });
    PutBigEndian(bytes, 52, static_cast<std::int16_t>(version2 ? 2 : 3));
    PutBigEndian(bytes, 132, std::int32_t{ 512 });
    PutBigEndian(bytes, 140, std::int32_t{ 1792 });
    PutBigEndian(bytes, 148, std::int32_t{ 3072 });
    PutBigEndian(bytes, ExamLengthOffset, layout == HeaderLayout::Minimum ? ExamMinimumLength : std::int32_t{ 1024 });
    PutBigEndian(
      bytes, SeriesLengthOffset, layout == HeaderLayout::Minimum ? SeriesMinimumLength : std::int32_t{ 1020 });
    PutBigEndian(bytes,
                 ImageLengthOffset,
                 layout == HeaderLayout::Minimum ? (isCT ? CTMinimumLength : MRMinimumLength) : std::int32_t{ 1022 });
    if (version2)
    {
      // Version 2 overrides the stored pointers and lengths before reading the fixed-layout headers.
      PutBigEndian(bytes, ExamLengthOffset, std::int32_t{ -1 });
      PutBigEndian(bytes, SeriesLengthOffset, std::int32_t{ 0 });
      PutBigEndian(bytes, ImageLengthOffset, std::int32_t{ 1 });
    }
  }

  PutBigEndian(bytes, exam + 8, std::int16_t{ 42 });
  PutText(bytes, exam + 10, "Test Hospital");
  PutText(bytes, exam + versionOffset(84, 88), "12-345");
  PutText(bytes, exam + versionOffset(97, 101), "Header Test");
  if (isCT)
  {
    PutText(bytes, exam + versionOffset(305, 309), "CT");
  }
  else
  {
    PutText(bytes, exam + versionOffset(305, 309), "MR");
  }
  PutBigEndian(bytes, series + 10, std::int16_t{ 17 });
  PutBigEndian(bytes, series + 12, std::int32_t{ 946684800 });
  PutBigEndian(bytes, image + 12, std::int16_t{ 9 });
  PutBigEndian(bytes, image + versionOffset(26, 28), 2.5F);
  PutBigEndian(bytes, image + versionOffset(30, 32), std::int16_t{ 4 });
  PutBigEndian(bytes, image + versionOffset(32, 34), std::int16_t{ 6 });
  PutBigEndian(bytes, image + versionOffset(34, 36), 3.0F);
  PutBigEndian(bytes, image + versionOffset(38, 40), 7.5F);
  PutBigEndian(bytes, image + versionOffset(42, 44), std::int16_t{ 4 });
  PutBigEndian(bytes, image + versionOffset(46, 48), std::int16_t{ 6 });
  PutBigEndian(bytes, image + versionOffset(50, 52), 0.75F);
  PutBigEndian(bytes, image + versionOffset(54, 56), 1.25F);
  PutBigEndian(bytes, image + versionOffset(114, 116), std::int16_t{ GE_AXIAL });
  PutBigEndian(bytes, image + versionOffset(186, 192), 9.5F);
  if (!isCT)
  {
    PutBigEndian(bytes, image + versionOffset(194, 200), std::int32_t{ 2000 });
    PutBigEndian(bytes, image + versionOffset(198, 204), std::int32_t{ 500 });
    PutBigEndian(bytes, image + versionOffset(202, 208), std::int32_t{ 30 });
    PutBigEndian(bytes, image + versionOffset(206, 212), std::int32_t{ 60 });
    PutBigEndian(bytes, image + versionOffset(210, 216), std::int16_t{ 2 });
    PutBigEndian(bytes, image + versionOffset(212, 218), std::int16_t{ 2 });
    PutBigEndian(bytes, image + versionOffset(218, 224), std::int32_t{ 3 });
    PutBigEndian(bytes, image + versionOffset(254, 260), std::int16_t{ 90 });
    PutText(bytes, image + versionOffset(308, 320), "TEST_SEQUENCE");
    PutBigEndian(bytes, image + versionOffset(398, 416), std::int16_t{ 7 });
  }
  return bytes;
}

class TemporaryGE5File
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TemporaryGE5File);

  TemporaryGE5File()
    : m_Directory(CreateDirectory())
  {}

  ~TemporaryGE5File()
  {
    std::error_code error;
    std::filesystem::remove_all(m_Directory, error);
  }

  std::string
  GetFileName() const
  {
    return (m_Directory / "header.ge5").string();
  }

  void
  Write(const FileBytes & bytes) const
  {
    std::ofstream file;
    file.exceptions(std::ios::failbit | std::ios::badbit);
    file.open(GetFileName(), std::ios::binary);
    file.write(reinterpret_cast<const char *>(bytes.data()), static_cast<std::streamsize>(bytes.size()));
    file.close();
  }

private:
  static std::filesystem::path
  CreateDirectory()
  {
    std::random_device random;
    const auto         parent = std::filesystem::temp_directory_path();
    for (unsigned int attempt = 0; attempt < 100; ++attempt)
    {
      const auto directory = parent / ("itkGE5Header-" + std::to_string(random()));
      if (std::filesystem::create_directory(directory))
      {
        return directory;
      }
    }
    throw std::runtime_error("Could not create a temporary GE5 test directory");
  }

  const std::filesystem::path m_Directory;
};

struct InvalidLengthCase
{
  std::string  name;
  bool         isCT;
  std::size_t  offset;
  std::int32_t length;
};

std::vector<InvalidLengthCase>
InvalidLengths()
{
  std::vector<InvalidLengthCase> cases;
  for (const bool isCT : { false, true })
  {
    const std::string                 modality = isCT ? "CT" : "MR";
    const std::array<std::size_t, 3>  offsets{ ExamLengthOffset, SeriesLengthOffset, ImageLengthOffset };
    const std::array<std::int32_t, 3> minima{ ExamMinimumLength,
                                              SeriesMinimumLength,
                                              isCT ? CTMinimumLength : MRMinimumLength };
    const std::array<std::string, 3>  sections{ "Exam", "Series", "Image" };
    const std::array<std::string, 4>  names{ "NegativeOne", "Zero", "One", "BelowMinimum" };
    for (std::size_t section = 0; section < offsets.size(); ++section)
    {
      const std::array<std::int32_t, 4> lengths{ -1, 0, 1, minima[section] - 1 };
      for (std::size_t index = 0; index < lengths.size(); ++index)
      {
        cases.push_back({ modality + sections[section] + names[index], isCT, offsets[section], lengths[index] });
      }
    }
  }
  return cases;
}

class GE5ImageIOHeaderInvalidLength : public testing::TestWithParam<InvalidLengthCase>
{};

TEST_P(GE5ImageIOHeaderInvalidLength, RejectsInvalidLength)
{
  const auto & testCase = GetParam();
  auto         bytes = MakeHeader(testCase.isCT, HeaderLayout::Padded);
  PutBigEndian(bytes, testCase.offset, testCase.length);
  const TemporaryGE5File file;
  file.Write(bytes);
  const auto reader = GE5HeaderReader::New();
  ASSERT_TRUE(reader->CanReadFile(file.GetFileName().c_str()));
  EXPECT_THROW((std::unique_ptr<GEImageHeader>{ reader->ReadHeader(file.GetFileName().c_str()) }),
               itk::ExceptionObject);
}

INSTANTIATE_TEST_SUITE_P(HeaderLengths,
                         GE5ImageIOHeaderInvalidLength,
                         testing::ValuesIn(InvalidLengths()),
                         [](const testing::TestParamInfo<InvalidLengthCase> & paramInfo) {
                           return paramInfo.param.name;
                         });

struct ValidHeaderCase
{
  std::string  name;
  bool         isCT;
  HeaderLayout layout;
};

class GE5ImageIOHeaderValidLength : public testing::TestWithParam<ValidHeaderCase>
{};

TEST_P(GE5ImageIOHeaderValidLength, PreservesMetadata)
{
  const auto &           testCase = GetParam();
  const TemporaryGE5File file;
  file.Write(MakeHeader(testCase.isCT, testCase.layout));
  const auto reader = GE5HeaderReader::New();
  ASSERT_TRUE(reader->CanReadFile(file.GetFileName().c_str()));
  std::unique_ptr<GEImageHeader> header;
  ASSERT_NO_THROW(header.reset(reader->ReadHeader(file.GetFileName().c_str())));
  ASSERT_NE(header, nullptr);
  EXPECT_EQ(header->examNumber, 42);
  EXPECT_EQ(header->seriesNumber, 17);
  EXPECT_EQ(header->imageNumber, 9);
  EXPECT_STREQ(header->hospital, "Test Hospital");
  EXPECT_STREQ(header->patientId, "12345");
  EXPECT_STREQ(header->name, "Header Test");
  EXPECT_STREQ(header->modality, testCase.isCT ? "CT" : "MR");
  EXPECT_NE(header->date[0], '\0');
  EXPECT_EQ(header->imageXsize, 4);
  EXPECT_EQ(header->imageYsize, 6);
  EXPECT_EQ(header->acqXsize, 4);
  EXPECT_EQ(header->acqYsize, 6);
  EXPECT_FLOAT_EQ(header->sliceThickness, 2.5F);
  EXPECT_FLOAT_EQ(header->xFOV, 3.0F);
  EXPECT_FLOAT_EQ(header->yFOV, 7.5F);
  EXPECT_FLOAT_EQ(header->imageXres, 0.75F);
  EXPECT_FLOAT_EQ(header->imageYres, 1.25F);
  EXPECT_FLOAT_EQ(header->brhcS, 9.5F);
  EXPECT_EQ(header->offset, testCase.layout == HeaderLayout::Headerless ? 8144 : PixelDataOffset);
  EXPECT_FLOAT_EQ(header->TR, testCase.isCT ? 0.0F : 2000.0F);
  EXPECT_FLOAT_EQ(header->TI, testCase.isCT ? 0.0F : 500.0F);
  EXPECT_FLOAT_EQ(header->TE, testCase.isCT ? 0.0F : 30.0F);
  EXPECT_FLOAT_EQ(header->TE2, testCase.isCT ? 0.0F : 60.0F);
  EXPECT_EQ(header->numberOfEchoes, testCase.isCT ? 1 : 2);
  EXPECT_EQ(header->echoNumber, testCase.isCT ? 1 : 2);
  EXPECT_EQ(header->NEX, testCase.isCT ? 1 : 3);
  EXPECT_EQ(header->flipAngle, testCase.isCT ? 0 : 90);
  EXPECT_STREQ(header->pulseSequence, testCase.isCT ? "" : "TEST_SEQUENCE");
  EXPECT_EQ(header->numberOfSlices, testCase.isCT ? 1 : 7);
}

INSTANTIATE_TEST_SUITE_P(HeaderLayouts,
                         GE5ImageIOHeaderValidLength,
                         testing::Values(ValidHeaderCase{ "MRExactMinimum", false, HeaderLayout::Minimum },
                                         ValidHeaderCase{ "CTExactMinimum", true, HeaderLayout::Minimum },
                                         ValidHeaderCase{ "MRNormalPadding", false, HeaderLayout::Padded },
                                         ValidHeaderCase{ "CTNormalPadding", true, HeaderLayout::Padded },
                                         ValidHeaderCase{ "MRVersion2Overrides", false, HeaderLayout::Version2 },
                                         ValidHeaderCase{ "CTVersion2Overrides", true, HeaderLayout::Version2 },
                                         ValidHeaderCase{ "MRHeaderless", false, HeaderLayout::Headerless },
                                         ValidHeaderCase{ "CTHeaderless", true, HeaderLayout::Headerless }),
                         [](const testing::TestParamInfo<ValidHeaderCase> & paramInfo) {
                           return paramInfo.param.name;
                         });
} // namespace
