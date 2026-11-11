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

// Characterization tests pinning the observable contract of
// itk::GDCMSeriesFileNames (issue #6467). These must remain green when the
// backend is migrated from the deprecated gdcm::SerieHelper to
// gdcm::Scanner + gdcm::IPPSorter: one series identifier for a single-series
// directory, deterministic geometric ordering that reconstructs a valid
// uniformly-spaced volume, GetInputFileNames == first series, and the
// Recursive flag controlling descent.

#include "gtest/gtest.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkGDCMImageIO.h"
#include "itkImageSeriesReader.h"
#include "itkImage.h"
#include "itkMath.h"
#include "itksys/SystemTools.hxx"
#include "itksys/Directory.hxx"
#include "gdcmImageChangeTransferSyntax.h"
#include "gdcmImageReader.h"
#include "gdcmImageWriter.h"
#include "gdcmReader.h"
#include "gdcmWriter.h"
#include "gdcmDataElement.h"
#include "gdcmStringFilter.h"
#include "gdcmTag.h"
#include <cstring>
#include <fstream>
#include <string>
#include <vector>

#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{
const std::string &
SeriesDir()
{
  static const std::string dir = TOSTRING(DICOM_SERIES_INPUT);
  return dir;
}

// Calls write(name) for every .dcm file in srcDir and counts the successes.
template <typename TWrite>
unsigned int
ForEachDicomSlice(const std::string & srcDir, TWrite write)
{
  itksys::Directory directory;
  directory.Load(srcDir.c_str());
  unsigned int written = 0;
  for (unsigned int i = 0; i < directory.GetNumberOfFiles(); ++i)
  {
    const std::string name = directory.GetFile(i);
    if (itksys::SystemTools::GetFilenameLastExtension(name) == ".dcm" && write(name))
    {
      ++written;
    }
  }
  return written;
}

unsigned int
CopyDicomSlices(const std::string & srcDir, const std::string & dstDir)
{
  itksys::SystemTools::MakeDirectory(dstDir);
  return ForEachDicomSlice(srcDir, [&](const std::string & name) {
    return itksys::SystemTools::CopyFileAlways(srcDir + '/' + name, dstDir + '/' + name);
  });
}

std::string
FirstDicomSlice(const std::string & srcDir)
{
  itksys::Directory directory;
  directory.Load(srcDir.c_str());
  for (unsigned int i = 0; i < directory.GetNumberOfFiles(); ++i)
  {
    const std::string name = directory.GetFile(i);
    if (itksys::SystemTools::GetFilenameLastExtension(name) == ".dcm")
    {
      return srcDir + '/' + name;
    }
  }
  return {};
}

std::string
ReadSeriesInstanceUID(const std::string & file)
{
  gdcm::Reader reader;
  reader.SetFileName(file.c_str());
  if (!reader.Read())
  {
    return {};
  }
  gdcm::StringFilter sf;
  sf.SetFile(reader.GetFile());
  std::string uid = sf.ToString(gdcm::Tag(0x0020, 0x000e));
  while (!uid.empty() && (uid.back() == '\0' || uid.back() == ' '))
  {
    uid.pop_back();
  }
  return uid;
}

// Copy one slice, overwriting Instance Number (0020,0013) so duplicate-IPP
// fixtures exercise the legacy ordering fallbacks.
bool
WriteWithInstanceNumber(const std::string & src, const std::string & dst, const char * instanceNumber)
{
  gdcm::Reader reader;
  reader.SetFileName(src.c_str());
  if (!reader.Read())
  {
    return false;
  }
  gdcm::DataElement de(gdcm::Tag(0x0020, 0x0013));
  de.SetVR(gdcm::VR::IS);
  de.SetByteValue(instanceNumber, static_cast<uint32_t>(std::strlen(instanceNumber)));
  reader.GetFile().GetDataSet().Replace(de);
  gdcm::Writer writer;
  writer.SetFileName(dst.c_str());
  writer.SetFile(reader.GetFile());
  return writer.Write();
}

// Copy one slice with the fixture's GE private b-value (0043,1039) set, optionally as implicit VR.
bool
WriteWithGEBValue(const std::string & src, const std::string & dst, const char * bValue, bool implicitVR)
{
  gdcm::ImageReader reader;
  reader.SetFileName(src.c_str());
  if (!reader.Read() || !reader.GetFile().GetDataSet().FindDataElement(gdcm::Tag(0x0043, 0x0010)))
  {
    return false;
  }
  gdcm::DataElement bValueElement(gdcm::Tag(0x0043, 0x1039));
  bValueElement.SetVR(gdcm::VR::IS);
  bValueElement.SetByteValue(bValue, static_cast<uint32_t>(std::strlen(bValue)));
  reader.GetFile().GetDataSet().Replace(bValueElement);
  gdcm::ImageChangeTransferSyntax change;
  change.SetTransferSyntax(implicitVR ? gdcm::TransferSyntax::ImplicitVRLittleEndian
                                      : gdcm::TransferSyntax::ExplicitVRLittleEndian);
  change.SetInput(reader.GetImage());
  if (!change.Change())
  {
    return false;
  }
  gdcm::ImageWriter writer;
  writer.SetFileName(dst.c_str());
  writer.SetFile(reader.GetFile());
  writer.SetImage(change.GetOutput());
  return writer.Write();
}

// Copy one slice, stripping Rows/Columns/PixelData so it mimics a non-image
// DICOM object (SR, RTSTRUCT, ...) sharing the series' SeriesInstanceUID;
// stripSeriesUID additionally drops SeriesInstanceUID (DICOMDIR-like).
bool
WriteNonImageObject(const std::string & src, const std::string & dst, bool stripSeriesUID = false)
{
  gdcm::Reader reader;
  reader.SetFileName(src.c_str());
  if (!reader.Read())
  {
    return false;
  }
  gdcm::DataSet & ds = reader.GetFile().GetDataSet();
  ds.Remove(gdcm::Tag(0x0028, 0x0010));
  ds.Remove(gdcm::Tag(0x0028, 0x0011));
  ds.Remove(gdcm::Tag(0x7fe0, 0x0010));
  if (stripSeriesUID)
  {
    ds.Remove(gdcm::Tag(0x0020, 0x000e));
  }
  gdcm::Writer writer;
  writer.SetCheckFileMetaInformation(false);
  writer.SetFileName(dst.c_str());
  writer.SetFile(reader.GetFile());
  return writer.Write();
}

// Each slice alternates between two b-values so a working restriction yields two series.
unsigned int
WriteAlternatingBValueSeries(const std::string & root, bool implicitVR)
{
  itksys::SystemTools::RemoveADirectory(root);
  itksys::SystemTools::MakeDirectory(root);
  unsigned int slice = 0;
  bool         allWritten = true;
  const auto   written = ForEachDicomSlice(SeriesDir(), [&](const std::string & name) {
    // Even-length IS values, so no padding byte is added.
    const char * bValue = (slice++ % 2 == 0) ? "0\\0\\0\\0 " : "1000\\0\\0\\0";
    const bool   ok = WriteWithGEBValue(SeriesDir() + '/' + name, root + '/' + name, bValue, implicitVR);
    allWritten = allWritten && ok;
    return ok;
  });
  return allWritten ? written : 0;
}

void
ExpectPrivateRestrictionSplitsSeries(bool implicitVR)
{
  const std::string root =
    TOSTRING(ITK_TEST_OUTPUT_DIR) + (implicitVR ? "/gdcmcontract_private_implicit" : "/gdcmcontract_private_explicit");
  const unsigned int written = WriteAlternatingBValueSeries(root, implicitVR);
  ASSERT_GT(written, 1u);

  auto unrestricted = itk::GDCMSeriesFileNames::New();
  unrestricted->SetUseSeriesDetails(true);
  unrestricted->SetInputDirectory(root);
  ASSERT_EQ(unrestricted->GetSeriesUIDs().size(), 1u);

  auto restricted = itk::GDCMSeriesFileNames::New();
  restricted->SetUseSeriesDetails(true);
  restricted->AddSeriesRestriction("0043|1039");
  restricted->SetInputDirectory(root);
  std::vector<std::string> uids;
  ASSERT_NO_THROW(uids = restricted->GetSeriesUIDs());
  ASSERT_EQ(uids.size(), 2u) << "The private b-value splits the series in two";
  unsigned int files = 0;
  for (const std::string & uid : uids)
  {
    files += static_cast<unsigned int>(restricted->GetFileNames(uid).size());
  }
  EXPECT_EQ(files, written);

  // A private tag that no file carries leaves the grouping unchanged.
  auto absent = itk::GDCMSeriesFileNames::New();
  absent->SetUseSeriesDetails(true);
  absent->AddSeriesRestriction("2001|1003");
  absent->SetInputDirectory(root);
  std::vector<std::string> absentUids;
  ASSERT_NO_THROW(absentUids = absent->GetSeriesUIDs());
  EXPECT_EQ(absentUids, unrestricted->GetSeriesUIDs());
}
} // namespace

TEST(GDCMSeriesFileNamesContract, SingleSeriesEnumerationAndGrouping)
{
  auto sf = itk::GDCMSeriesFileNames::New();
  sf->SetInputDirectory(SeriesDir());

  const std::vector<std::string> uids = sf->GetSeriesUIDs();
  ASSERT_EQ(uids.size(), 1u) << "DicomSeries is a single series";

  const std::vector<std::string> byUid = sf->GetFileNames(uids.front());
  EXPECT_GT(byUid.size(), 1u) << "Series has multiple slices";

  const std::vector<std::string> input = sf->GetInputFileNames();
  EXPECT_EQ(input, byUid) << "GetInputFileNames must return the (single) series, ordered identically";
}

TEST(GDCMSeriesFileNamesContract, OrderingReconstructsValidVolume)
{
  using ImageType = itk::Image<short, 3>;
  auto sf = itk::GDCMSeriesFileNames::New();
  sf->SetInputDirectory(SeriesDir());
  const std::vector<std::string> files = sf->GetInputFileNames();
  ASSERT_GT(files.size(), 1u);

  auto reader = itk::ImageSeriesReader<ImageType>::New();
  reader->SetImageIO(itk::GDCMImageIO::New());
  reader->SetFileNames(files);
  ASSERT_NO_THROW(reader->Update());

  const ImageType::Pointer image = reader->GetOutput();
  EXPECT_EQ(image->GetLargestPossibleRegion().GetSize()[2], files.size()) << "Each ordered slice becomes one z-plane";
  for (unsigned int d = 0; d < 3; ++d)
  {
    const double spacing = image->GetSpacing()[d];
    EXPECT_TRUE(std::isfinite(spacing) && spacing > 0.0) << "Spacing[" << d << "] must be positive and finite";
  }
}

TEST(GDCMSeriesFileNamesContract, RecursiveFlagHonored)
{
  const std::string root = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/gdcmcontract_recursive";
  itksys::SystemTools::RemoveADirectory(root);
  itksys::SystemTools::MakeDirectory(root);
  ASSERT_GT(CopyDicomSlices(SeriesDir(), root + "/nested"), 1u);

  // Recursive is read lazily during BuildSeriesMap(); order relative to
  // SetInputDirectory does not matter.
  auto flat = itk::GDCMSeriesFileNames::New();
  flat->SetRecursive(false);
  flat->SetInputDirectory(root);
  EXPECT_TRUE(flat->GetInputFileNames().empty()) << "No DICOM directly under root; flat scan finds none";

  auto recursive = itk::GDCMSeriesFileNames::New();
  recursive->SetRecursive(true);
  recursive->SetInputDirectory(root);
  EXPECT_FALSE(recursive->GetInputFileNames().empty()) << "Recursive scan must descend into the subdirectory";
}

TEST(GDCMSeriesFileNamesContract, AmbiguousOrderingThrowsByDefault)
{
  const std::string root = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/gdcmcontract_ambiguous_throw";
  itksys::SystemTools::RemoveADirectory(root);
  itksys::SystemTools::MakeDirectory(root);
  const std::string slice = FirstDicomSlice(SeriesDir());
  ASSERT_FALSE(slice.empty());
  // Two copies of the same slice: duplicate ImagePositionPatient.
  ASSERT_TRUE(itksys::SystemTools::CopyFileAlways(slice, root + "/copy_a.dcm"));
  ASSERT_TRUE(itksys::SystemTools::CopyFileAlways(slice, root + "/copy_b.dcm"));

  auto sf = itk::GDCMSeriesFileNames::New();
  EXPECT_TRUE(sf->GetFailOnAmbiguousOrdering());
  sf->SetInputDirectory(root);
  EXPECT_NO_THROW(sf->GetSeriesUIDs()) << "Enumeration does not order and must not throw";
  EXPECT_THROW(sf->GetInputFileNames(), itk::ExceptionObject);
}

TEST(GDCMSeriesFileNamesContract, LegacyFallbackUsesInstanceNumberThenFilename)
{
  const std::string slice = FirstDicomSlice(SeriesDir());
  ASSERT_FALSE(slice.empty());

  // Duplicate IPP with unique Instance Numbers: fallback must order by
  // Instance Number, chosen here to be the reverse of lexicographic order.
  const std::string byInstance = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/gdcmcontract_fallback_instance";
  itksys::SystemTools::RemoveADirectory(byInstance);
  itksys::SystemTools::MakeDirectory(byInstance);
  ASSERT_TRUE(WriteWithInstanceNumber(slice, byInstance + "/z_first.dcm", "1 "));
  ASSERT_TRUE(WriteWithInstanceNumber(slice, byInstance + "/a_second.dcm", "2 "));

  auto sf = itk::GDCMSeriesFileNames::New();
  sf->FailOnAmbiguousOrderingOff();
  sf->SetInputDirectory(byInstance);
  const std::vector<std::string> instanceOrdered = sf->GetInputFileNames();
  ASSERT_EQ(instanceOrdered.size(), 2u);
  EXPECT_NE(instanceOrdered[0].find("z_first"), std::string::npos);
  EXPECT_NE(instanceOrdered[1].find("a_second"), std::string::npos);

  // Duplicate IPP and duplicate Instance Numbers: final fallback is
  // lexicographic filename order.
  const std::string byName = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/gdcmcontract_fallback_filename";
  itksys::SystemTools::RemoveADirectory(byName);
  itksys::SystemTools::MakeDirectory(byName);
  ASSERT_TRUE(itksys::SystemTools::CopyFileAlways(slice, byName + "/copy_b.dcm"));
  ASSERT_TRUE(itksys::SystemTools::CopyFileAlways(slice, byName + "/copy_a.dcm"));

  auto lex = itk::GDCMSeriesFileNames::New();
  lex->FailOnAmbiguousOrderingOff();
  lex->SetInputDirectory(byName);
  const std::vector<std::string> nameOrdered = lex->GetInputFileNames();
  ASSERT_EQ(nameOrdered.size(), 2u);
  EXPECT_LT(nameOrdered[0], nameOrdered[1]);
}

TEST(GDCMSeriesFileNamesContract, NonImageObjectsExcluded)
{
  const std::string root = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/gdcmcontract_nonimage";
  itksys::SystemTools::RemoveADirectory(root);
  itksys::SystemTools::MakeDirectory(root);
  const unsigned int copied = CopyDicomSlices(SeriesDir(), root);
  ASSERT_GT(copied, 1u);
  const std::string slice = FirstDicomSlice(SeriesDir());
  ASSERT_FALSE(slice.empty());
  ASSERT_TRUE(WriteNonImageObject(slice, root + "/structured_report.dcm"));

  auto sf = itk::GDCMSeriesFileNames::New();
  sf->SetInputDirectory(root);
  const std::vector<std::string> uids = sf->GetSeriesUIDs();
  ASSERT_EQ(uids.size(), 1u) << "Non-image object must not surface as an extra series";
  const std::vector<std::string> files = sf->GetFileNames(uids.front());
  EXPECT_EQ(files.size(), copied) << "Only image slices are enumerated";
  for (const std::string & fn : files)
  {
    EXPECT_EQ(fn.find("structured_report"), std::string::npos);
  }
}

TEST(GDCMSeriesFileNamesContract, DefaultGroupingUsesRawSeriesInstanceUID)
{
  const std::string slice = FirstDicomSlice(SeriesDir());
  ASSERT_FALSE(slice.empty());
  const std::string uid = ReadSeriesInstanceUID(slice);
  ASSERT_FALSE(uid.empty());

  auto sf = itk::GDCMSeriesFileNames::New();
  EXPECT_FALSE(sf->GetUseSeriesDetails()) << "Series details are a documented opt-in";
  sf->SetInputDirectory(SeriesDir());
  const std::vector<std::string> uids = sf->GetSeriesUIDs();
  ASSERT_EQ(uids.size(), 1u);
  EXPECT_EQ(uids.front(), uid) << "Default identifiers are plain SeriesInstanceUIDs (0020,000e)";
  EXPECT_FALSE(sf->GetFileNames(uid).empty()) << "A raw SeriesInstanceUID from other DICOM tooling must match";
}

TEST(GDCMSeriesFileNamesContract, SeriesRestrictionSurvivesSetUseSeriesDetails)
{
  // Instance Number is unique per slice, so restricting on it splits the
  // single series into one series per file — easy to detect.
  auto restrictionFirst = itk::GDCMSeriesFileNames::New();
  restrictionFirst->AddSeriesRestriction("0020|0013");
  restrictionFirst->SetUseSeriesDetails(true);
  restrictionFirst->SetInputDirectory(SeriesDir());
  const std::vector<std::string> uids = restrictionFirst->GetSeriesUIDs();
  EXPECT_GT(uids.size(), 1u) << "Restriction must take effect regardless of call order";

  auto detailsFirst = itk::GDCMSeriesFileNames::New();
  detailsFirst->SetUseSeriesDetails(true);
  detailsFirst->AddSeriesRestriction("0020|0013");
  detailsFirst->SetInputDirectory(SeriesDir());
  EXPECT_EQ(detailsFirst->GetSeriesUIDs(), uids) << "Call order must not change the series grouping";
}

TEST(GDCMSeriesFileNamesContract, MixedContentDirectoryEnumeratesOnlyImageSeries)
{
  const std::string root = TOSTRING(ITK_TEST_OUTPUT_DIR) + "/gdcmcontract_mixed";
  itksys::SystemTools::RemoveADirectory(root);
  itksys::SystemTools::MakeDirectory(root);
  const unsigned int copied = CopyDicomSlices(SeriesDir(), root);
  ASSERT_GT(copied, 1u);
  const std::string slice = FirstDicomSlice(SeriesDir());
  ASSERT_FALSE(slice.empty());
  // SR-like object sharing the series' SeriesInstanceUID.
  ASSERT_TRUE(WriteNonImageObject(slice, root + "/report_in_series.dcm"));
  // DICOMDIR-like object with no SeriesInstanceUID at all.
  ASSERT_TRUE(WriteNonImageObject(slice, root + "/dicomdir_like.dcm", true));
  // Plain non-DICOM file.
  {
    std::ofstream readme(root + "/README.txt");
    readme << "not a DICOM file\n";
  }

  auto sf = itk::GDCMSeriesFileNames::New();
  sf->SetInputDirectory(root);
  const std::vector<std::string> uids = sf->GetSeriesUIDs();
  ASSERT_EQ(uids.size(), 1u) << "Only the image series is enumerated";
  const std::vector<std::string> ordered = sf->GetInputFileNames();
  ASSERT_EQ(ordered.size(), copied);
  for (const std::string & fn : ordered)
  {
    EXPECT_EQ(fn.find("report_in_series"), std::string::npos);
    EXPECT_EQ(fn.find("dicomdir_like"), std::string::npos);
    EXPECT_EQ(fn.find("README"), std::string::npos);
  }

  // The mixed content must not disturb geometric ordering of the series.
  using ImageType = itk::Image<short, 3>;
  auto reader = itk::ImageSeriesReader<ImageType>::New();
  reader->SetImageIO(itk::GDCMImageIO::New());
  reader->SetFileNames(ordered);
  ASSERT_NO_THROW(reader->Update());
  EXPECT_EQ(reader->GetOutput()->GetLargestPossibleRegion().GetSize()[2], ordered.size());
}

TEST(GDCMSeriesFileNamesContract, SeriesRestrictionOnPrivateTagsRefinesSeries)
{
  ExpectPrivateRestrictionSplitsSeries(false);
}

TEST(GDCMSeriesFileNamesContract, SeriesRestrictionOnPrivateTagsInImplicitVRFiles)
{
  ExpectPrivateRestrictionSplitsSeries(true);
}
