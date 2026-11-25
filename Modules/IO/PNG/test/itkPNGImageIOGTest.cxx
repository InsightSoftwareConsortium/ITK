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
#include "itkPNGImageIO.h"

#include "itkGTest.h"

#include <fstream>
#include <iterator>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) _STRING(s)

namespace
{

std::string
MakeOutputPath(const std::string & name)
{
  return std::string(TOSTRING(ITK_TEST_OUTPUT_DIR)) + "/" + name;
}

std::vector<char>
ReadFileBytes(const std::string & path)
{
  std::ifstream ifs(path, std::ios::binary);
  return std::vector<char>((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
}

itk::PNGImageIO::Pointer
MakeWriter(const std::string &  path,
           unsigned int         width,
           unsigned int         height,
           itk::IOComponentEnum componentType = itk::IOComponentEnum::UCHAR,
           unsigned int         numComp = 1)
{
  auto io = itk::PNGImageIO::New();
  io->SetFileName(path);
  io->SetNumberOfDimensions(2);
  io->SetDimensions(0, width);
  io->SetDimensions(1, height);
  if (numComp > 1)
  {
    io->SetPixelType(itk::IOPixelEnum::VECTOR);
  }
  else
  {
    io->SetPixelType(itk::IOPixelEnum::SCALAR);
  }
  io->SetComponentType(componentType);
  io->SetNumberOfComponents(numComp);
  return io;
}

void
WriteValidGrayscalePNG(const std::string & path, unsigned int width, unsigned int height)
{
  const auto                       io = MakeWriter(path, width, height);
  const std::vector<unsigned char> buffer(static_cast<size_t>(width) * height, 128);
  io->Write(buffer.data());
}

} // namespace

TEST(PNGImageIOWriteSlice, RejectsUnsupportedComponentTypeAndPreservesExistingFile)
{
  const std::string path = MakeOutputPath("PNGImageIOWriteSlice_B70.png");
  WriteValidGrayscalePNG(path, 4, 4);
  const std::vector<char> before = ReadFileBytes(path);
  ASSERT_FALSE(before.empty());

  const auto               io = MakeWriter(path, 4, 4, itk::IOComponentEnum::SHORT);
  const std::vector<short> buffer(4 * 4, 0);
  EXPECT_THROW(io->Write(buffer.data()), itk::ExceptionObject);

  const std::vector<char> after = ReadFileBytes(path);
  EXPECT_EQ(before, after);

  const auto readIO = itk::PNGImageIO::New();
  ASSERT_TRUE(readIO->CanReadFile(path.c_str()));
  readIO->SetFileName(path);
  ASSERT_NO_THROW(readIO->ReadImageInformation());
  EXPECT_EQ(readIO->GetComponentType(), itk::IOComponentEnum::UCHAR);

  std::vector<unsigned char> readBuffer(4 * 4, 0);
  ASSERT_NO_THROW(readIO->Read(readBuffer.data()));
  EXPECT_EQ(readBuffer[0], 128);
}

TEST(PNGImageIOWriteSlice, RejectsUnsupportedComponentCountAndPreservesExistingFile)
{
  const std::string path = MakeOutputPath("PNGImageIOWriteSlice_B72.png");
  WriteValidGrayscalePNG(path, 4, 4);
  const std::vector<char> before = ReadFileBytes(path);
  ASSERT_FALSE(before.empty());

  const auto                       io = MakeWriter(path, 4, 4, itk::IOComponentEnum::UCHAR, 5);
  const std::vector<unsigned char> buffer(4 * 4 * 5, 3);
  EXPECT_THROW(io->Write(buffer.data()), itk::ExceptionObject);

  const std::vector<char> after = ReadFileBytes(path);
  EXPECT_EQ(before, after);
}
