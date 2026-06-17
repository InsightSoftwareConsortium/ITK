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

// Parity tests for itk::DCMTKSeriesFileNames (issue #2735): recursive scan,
// distinct series-UID enumeration / grouping, UseSeriesDetails restrictions,
// and cache invalidation. Fixtures are built at runtime by copying the
// rect-centered / rect-offset DICOM series into temporary layouts, so no new
// committed test data is required.

#include "gtest/gtest.h"
#include "itkDCMTKSeriesFileNames.h"
#include "itksys/SystemTools.hxx"
#include "itksys/Directory.hxx"
#include <string>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
// Copy every *.dcm file from srcDir into dstDir (created if needed), optionally
// prefixing the destination file name so two series can share one flat dir.
unsigned int
CopyDicomSlices(const std::string & srcDir, const std::string & dstDir, const std::string & prefix = "")
{
  itksys::SystemTools::MakeDirectory(dstDir);
  itksys::Directory directory;
  directory.Load(srcDir.c_str());
  unsigned int copied = 0;
  for (unsigned int i = 0; i < directory.GetNumberOfFiles(); ++i)
  {
    const std::string name = directory.GetFile(i);
    if (itksys::SystemTools::GetFilenameLastExtension(name) != ".dcm")
    {
      continue;
    }
    const std::string src = srcDir + '/' + name;
    const std::string dst = dstDir + '/' + prefix + name;
    if (itksys::SystemTools::CopyFileAlways(src, dst))
    {
      ++copied;
    }
  }
  return copied;
}

std::string
FreshDir(const std::string & name)
{
  const std::string dir = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/dcmtkparity_" + name;
  itksys::SystemTools::RemoveADirectory(dir);
  itksys::SystemTools::MakeDirectory(dir);
  return dir;
}
} // namespace

// B (issue #2735): the Recursive flag must control descent into subdirectories.
TEST(DCMTKSeriesFileNamesParity, RecursiveFlagHonored)
{
  const std::string root = FreshDir("recursive");
  ASSERT_GT(CopyDicomSlices(TOSTRING(RECT_CENTERED_DIR), root + "/seriesA"), 0u);
  ASSERT_GT(CopyDicomSlices(TOSTRING(RECT_OFFSET_DIR), root + "/seriesB"), 0u);

  auto flat = itk::DCMTKSeriesFileNames::New();
  flat->SetInputDirectory(root);
  flat->RecursiveOff();
  EXPECT_TRUE(flat->GetInputFileNames().empty()) << "No DICOM lives directly under root; flat scan must find none";

  auto recursive = itk::DCMTKSeriesFileNames::New();
  recursive->SetInputDirectory(root);
  recursive->RecursiveOn();
  EXPECT_FALSE(recursive->GetInputFileNames().empty()) << "Recursive scan must descend into seriesA / seriesB";
}

// A (issue #2735): GetSeriesUIDs() must return one entry per distinct series,
// not one per file; files must be grouped by series; GetInputFileNames() must
// return a single series, not the union.
TEST(DCMTKSeriesFileNamesParity, DistinctSeriesGroupedNoDuplicates)
{
  const std::string  root = FreshDir("grouping");
  const unsigned int nA = CopyDicomSlices(TOSTRING(RECT_CENTERED_DIR), root, "a_");
  const unsigned int nB = CopyDicomSlices(TOSTRING(RECT_OFFSET_DIR), root, "b_");
  ASSERT_GT(nA, 0u);
  ASSERT_GT(nB, 0u);

  auto sf = itk::DCMTKSeriesFileNames::New();
  sf->SetInputDirectory(root);

  const std::vector<std::string> uids = sf->GetSeriesUIDs();
  EXPECT_EQ(uids.size(), 2u) << "Two distinct SeriesInstanceUIDs, no per-file duplicates";

  std::size_t grouped = 0;
  for (const std::string & uid : uids)
  {
    const std::vector<std::string> files = sf->GetFileNames(uid);
    EXPECT_FALSE(files.empty()) << "Each series UID must resolve to its files";
    grouped += files.size();
  }
  EXPECT_EQ(grouped, static_cast<std::size_t>(nA) + nB);

  const std::vector<std::string> first = sf->GetInputFileNames();
  EXPECT_LT(first.size(), grouped) << "GetInputFileNames returns a single series, not the union";
}
