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

// Parity tests for itk::DCMTKSeriesFileNames: recursive scan, series grouping, restrictions, caching.

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
// Copy *.dcm from srcDir into dstDir, optionally prefixing names so two series can share a flat dir.
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

// The Recursive flag must control descent into subdirectories.
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

// GetSeriesUIDs() returns one entry per distinct series (grouped), not one per file.
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

// rect-centered's ImagePositionPatient (0020,0032) varies per slice, so restricting on it over-splits the series.
TEST(DCMTKSeriesFileNamesParity, AddSeriesRestrictionSplitsByVaryingTag)
{
  const std::string  root = FreshDir("restriction");
  const unsigned int n = CopyDicomSlices(TOSTRING(RECT_CENTERED_DIR), root);
  ASSERT_GT(n, 1u);

  auto plain = itk::DCMTKSeriesFileNames::New();
  plain->SetInputDirectory(root);
  EXPECT_EQ(plain->GetSeriesUIDs().size(), 1u) << "rect-centered is a single series by default";

  auto restricted = itk::DCMTKSeriesFileNames::New();
  restricted->SetInputDirectory(root);
  restricted->SetUseSeriesDetails(true);
  restricted->AddSeriesRestriction("0020|0032"); // ImagePositionPatient varies per slice
  EXPECT_EQ(restricted->GetSeriesUIDs().size(), static_cast<std::size_t>(n))
    << "Restricting on a per-slice-varying tag must split into one series per slice";

  auto restrictionOffDetails = itk::DCMTKSeriesFileNames::New();
  restrictionOffDetails->SetInputDirectory(root);
  restrictionOffDetails->SetUseSeriesDetails(false);
  restrictionOffDetails->AddSeriesRestriction("0020|0032");
  EXPECT_EQ(restrictionOffDetails->GetSeriesUIDs().size(), 1u)
    << "Restrictions must be ignored when UseSeriesDetails is off";
}

// The parse is cached across calls and invalidated when the object is modified.
TEST(DCMTKSeriesFileNamesParity, CacheReusedAndInvalidatedOnModify)
{
  const std::string dirA = FreshDir("cacheA");
  const std::string dirB = FreshDir("cacheB");
  ASSERT_GT(CopyDicomSlices(TOSTRING(RECT_CENTERED_DIR), dirA), 0u);
  ASSERT_GT(CopyDicomSlices(TOSTRING(RECT_OFFSET_DIR), dirB), 0u);

  auto sf = itk::DCMTKSeriesFileNames::New();
  sf->SetInputDirectory(dirA);
  const std::vector<std::string> first = sf->GetInputFileNames();
  const std::vector<std::string> again = sf->GetInputFileNames();
  EXPECT_EQ(first, again) << "Repeated calls without modification return the cached result";

  // Changing the directory must invalidate the cache.
  sf->SetInputDirectory(dirB);
  const std::vector<std::string> afterDirChange = sf->GetInputFileNames();
  ASSERT_FALSE(afterDirChange.empty());
  EXPECT_NE(first, afterDirChange) << "SetInputDirectory must invalidate the cache";

  // Adding a per-slice-varying restriction must invalidate and re-split.
  sf->SetInputDirectory(dirA);
  EXPECT_EQ(sf->GetSeriesUIDs().size(), 1u);
  sf->SetUseSeriesDetails(true);
  sf->AddSeriesRestriction("0020|0032");
  EXPECT_GT(sf->GetSeriesUIDs().size(), 1u) << "AddSeriesRestriction must invalidate the cache";
}
