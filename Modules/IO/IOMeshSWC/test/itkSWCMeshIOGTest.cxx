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

#include "itkSWCMeshIO.h"
#include "itkGTest.h"

#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

namespace
{
std::string
WriteTempSWC(const std::string & name, const std::string & content)
{
  const std::string path = (std::filesystem::temp_directory_path() / name).string();
  std::ofstream     out(path, std::ios::binary);
  out << content;
  out.close();
  return path;
}
} // namespace

// A file whose last record has no trailing newline must not drop that node.
TEST(SWCMeshIO, ReadsLastNodeWithoutTrailingNewline)
{
  const std::string path = WriteTempSWC("swc_gtest_no_trailing_newline.swc", "1 1 0 0 0 1.0 -1\n2 3 1 1 1 0.5 1");
  auto              io = itk::SWCMeshIO::New();
  io->SetFileName(path);
  ASSERT_TRUE(io->CanReadFile(path.c_str()));
  io->ReadMeshInformation();
  EXPECT_EQ(io->GetNumberOfPoints(), 2u);
  EXPECT_EQ(io->GetNumberOfCells(), 1u);
}

// Blank lines and comment lines are skipped, not parsed as data.
TEST(SWCMeshIO, SkipsBlankAndCommentLines)
{
  const std::string path = WriteTempSWC("swc_gtest_blanks.swc", "# header\n\n1 1 0 0 0 1.0 -1\n\n2 3 1 1 1 0.5 1\n");
  auto              io = itk::SWCMeshIO::New();
  io->SetFileName(path);
  io->ReadMeshInformation();
  EXPECT_EQ(io->GetNumberOfPoints(), 2u);
  EXPECT_EQ(io->GetNumberOfCells(), 1u);
}

// A record with fewer than seven fields raises instead of fabricating a point.
TEST(SWCMeshIO, ThrowsOnMalformedRecord)
{
  const std::string path = WriteTempSWC("swc_gtest_malformed.swc", "1 1 0 0 0 1.0 -1\n2 3 1 1\n");
  auto              io = itk::SWCMeshIO::New();
  io->SetFileName(path);
  EXPECT_THROW(io->ReadMeshInformation(), itk::ExceptionObject);
}

// A node referencing an undefined parent raises instead of connecting to point 0.
TEST(SWCMeshIO, ThrowsOnUndefinedParent)
{
  const std::string path = WriteTempSWC("swc_gtest_badparent.swc", "1 1 0 0 0 1.0 -1\n2 3 1 1 1 0.5 99\n");
  auto              io = itk::SWCMeshIO::New();
  io->SetFileName(path);
  io->ReadMeshInformation();
  std::vector<unsigned int> cellBuffer(16, 0u);
  EXPECT_THROW(io->ReadCells(cellBuffer.data()), itk::ExceptionObject);
}
