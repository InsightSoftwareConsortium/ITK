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

// Regression for issue #6463: itk::DCMTKSeriesFileNames must order slices
// geometrically (ImagePositionPatient projected on the slice normal), matching
// itk::GDCMSeriesFileNames, even when InstanceNumber (0020,0013) is absent or
// constant. The rect-centered / rect-offset fixtures all carry InstanceNumber
// 0, which previously left DCMTK's ordering to an unstable sort and scrambled
// the reconstructed volume.

#include "gtest/gtest.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkDCMTKSeriesFileNames.h"
#include "itksys/SystemTools.hxx"
#include <string>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
std::vector<std::string>
Basenames(const std::vector<std::string> & paths)
{
  std::vector<std::string> names;
  names.reserve(paths.size());
  for (const auto & path : paths)
  {
    names.push_back(itksys::SystemTools::GetFilenameName(path));
  }
  return names;
}
} // namespace

TEST(DCMTKSeriesFileNamesOrdering, MatchesGDCMOrdering)
{
  const std::vector<std::string> dirs = { TOSTRING(RECT_CENTERED_DIR), TOSTRING(RECT_OFFSET_DIR) };
  for (const auto & dir : dirs)
  {
    auto gdcm = itk::GDCMSeriesFileNames::New();
    gdcm->SetInputDirectory(dir);
    const std::vector<std::string> gdcmNames = Basenames(gdcm->GetInputFileNames());

    auto dcmtk = itk::DCMTKSeriesFileNames::New();
    dcmtk->SetInputDirectory(dir);
    const std::vector<std::string> dcmtkNames = Basenames(dcmtk->GetInputFileNames());

    ASSERT_FALSE(gdcmNames.empty()) << "No DICOM files found in " << dir;
    EXPECT_EQ(gdcmNames.size(), dcmtkNames.size()) << "Different slice counts for " << dir;
    EXPECT_EQ(gdcmNames, dcmtkNames) << "GDCM and DCMTK disagree on slice ordering for " << dir;
  }
}
