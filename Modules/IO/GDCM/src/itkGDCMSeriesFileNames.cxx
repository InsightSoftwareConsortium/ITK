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

#include "itkGDCMSeriesFileNames.h"
#include "itksys/SystemTools.hxx"
#include "itkPrintHelper.h"
#include "gdcmDirectory.h"
#include "gdcmScanner.h"
#include "gdcmIPPSorter.h"
#include "gdcmTag.h"
#include <algorithm>
#include <cctype>
#include <map>
#include <stdexcept>
#include <vector>

namespace itk
{

namespace
{
// Default series-detail tags, matching
// gdcm::SerieHelper::CreateDefaultUniqueSeriesIdentifier.
constexpr std::pair<unsigned short, unsigned short> DefaultDetailTags[] = {
  { 0x0020, 0x0011 }, // Series Number
  { 0x0018, 0x0024 }, // Sequence Name
  { 0x0018, 0x0050 }, // Slice Thickness
  { 0x0028, 0x0010 }, // Rows
  { 0x0028, 0x0011 }, // Columns
};
} // namespace

GDCMSeriesFileNames::GDCMSeriesFileNames() = default;

GDCMSeriesFileNames::~GDCMSeriesFileNames() = default;


void
GDCMSeriesFileNames::SetInputDirectory(const char * name)
{
  if (!name)
  {
    itkExceptionStringMacro("SetInputDirectory() received a nullptr string");
  }
  const std::string fname = name;
  this->SetInputDirectory(fname);
}

void
GDCMSeriesFileNames::AddSeriesRestriction(const std::string & tag)
{
  // Parse a "group|element" tag (hex) and add it to the series-identifier
  // criteria so it sub-refines a SeriesInstanceUID into multiple series, as
  // documented and as used by the ITK examples (e.g. "0008|0021").
  const std::string::size_type bar = tag.find('|');
  if (bar == std::string::npos)
  {
    itkWarningMacro("Ignoring malformed series restriction tag '" << tag << "' (expected \"group|element\")");
    return;
  }
  try
  {
    const auto group = static_cast<unsigned short>(std::stoul(tag.substr(0, bar), nullptr, 16));
    const auto element = static_cast<unsigned short>(std::stoul(tag.substr(bar + 1), nullptr, 16));
    m_UserRefineTags.emplace_back(group, element);
  }
  catch (const std::exception &)
  {
    itkWarningMacro("Ignoring malformed series restriction tag '" << tag << "' (expected hex \"group|element\")");
    return;
  }
  this->Modified();
}

void
GDCMSeriesFileNames::SetInputDirectory(const std::string & name)
{
  if (name.empty())
  {
    itkWarningMacro("You need to specify a directory where the DICOM files are located");
    return;
  }
  if (m_InputDirectory == name)
  {
    return;
  }
  if (!itksys::SystemTools::FileIsDirectory(name.c_str()))
  {
    itkWarningMacro(<< name << " is not a directory");
    return;
  }
  m_InputDirectory = name;
  this->Modified();
}

void
GDCMSeriesFileNames::BuildSeriesMap()
{
  // Reuse the previous parse unless the object has been modified since.
  if (m_CacheBuildTime.GetMTime() > this->GetMTime())
  {
    return;
  }
  m_SeriesUIDs.clear();
  m_SeriesFiles.clear();
  m_InstanceNumbers.clear();

  if (m_InputDirectory.empty())
  {
    return;
  }

  gdcm::Directory dir;
  dir.Load(m_InputDirectory, m_Recursive);
  const gdcm::Directory::FilenamesType & filenames = dir.GetFilenames();
  if (filenames.empty())
  {
    return;
  }

  std::vector<std::pair<unsigned short, unsigned short>> refineTags;
  if (m_UseSeriesDetails)
  {
    refineTags.assign(std::begin(DefaultDetailTags), std::end(DefaultDetailTags));
    refineTags.insert(refineTags.end(), m_UserRefineTags.begin(), m_UserRefineTags.end());
  }

  const gdcm::Tag seriesUID(0x0020, 0x000e);
  const gdcm::Tag instanceNumber(0x0020, 0x0013);
  const gdcm::Tag rows(0x0028, 0x0010);
  gdcm::Scanner   scanner;
  scanner.AddTag(seriesUID);
  scanner.AddTag(instanceNumber);
  scanner.AddTag(rows);
  for (const auto & [group, element] : refineTags)
  {
    scanner.AddTag(gdcm::Tag(group, element));
  }
  if (!scanner.Scan(filenames))
  {
    itkWarningMacro("Failed to scan DICOM tags in " << m_InputDirectory);
    return;
  }

  // Build the unique series identifier per file, replicating
  // gdcm::SerieHelper::CreateUniqueSeriesIdentifier.
  auto makeIdentifier = [&](const char * fn) -> std::string {
    const char *      uidValue = scanner.GetValue(fn, seriesUID);
    std::string       id = (uidValue != nullptr) ? uidValue : "";
    const std::string uid = id;
    for (const auto & [group, element] : refineTags)
    {
      const char *      value = scanner.GetValue(fn, gdcm::Tag(group, element));
      const std::string s = (value != nullptr) ? value : "";
      if (id == uid && !s.empty())
      {
        id += '.';
      }
      id += s;
    }
    // Eliminate all non-alphanumeric characters (keep '.').
    id.erase(std::remove_if(id.begin(), id.end(), [](unsigned char c) { return c != '.' && std::isalnum(c) == 0; }),
             id.end());
    return id;
  };

  for (const std::string & fn : filenames)
  {
    if (!scanner.IsKey(fn.c_str()))
    {
      continue; // not a DICOM file the scanner could read
    }
    if (scanner.GetValue(fn.c_str(), rows) == nullptr)
    {
      continue; // no Rows: not an image object (SR, RTSTRUCT, DICOMDIR, ...)
    }
    const std::string id = makeIdentifier(fn.c_str());
    SeriesEntry &     entry = m_SeriesFiles[id];
    if (entry.Files.empty())
    {
      m_SeriesUIDs.push_back(id);
    }
    entry.Files.push_back(fn);
    if (const char * number = scanner.GetValue(fn.c_str(), instanceNumber))
    {
      m_InstanceNumbers[fn] = number;
    }
  }

  m_CacheBuildTime.Modified();
}

void
GDCMSeriesFileNames::OrderSeries(SeriesEntry & entry)
{
  if (entry.Ordered || entry.Files.size() < 2)
  {
    entry.Ordered = true;
    return;
  }
  // Geometric ordering: ImagePositionPatient projected on the slice normal.
  // gdcm::IPPSorter is strict: it FAILS on duplicate IPP and gantry-tilt
  // acquisitions (see issue #6468).
  gdcm::IPPSorter sorter;
  sorter.SetComputeZSpacing(false);
  bool wasSortingAchieved = sorter.Sort(entry.Files);

  // Set a public flag so that callers know this fallback occurred, and can show a warning.
  m_DidUseAmbiguousOrdering = !wasSortingAchieved;

  if (wasSortingAchieved)
  {
    entry.Files = sorter.GetFilenames();
    entry.Ordered = true;
    return;
  }
  if (m_FailOnAmbiguousOrdering)
  {
    itkExceptionMacro("Series cannot be ordered geometrically (duplicate ImagePositionPatient or inconsistent "
                      "orientation, see issue #6468). Set FailOnAmbiguousOrdering to false to accept the legacy "
                      "non-standard ordering heuristics.");
  }

  // Legacy SerieHelper heuristics (Instance Number, then lexicographic),
  // kept only for determinism and backward compatibility: an untrustworthy,
  // non-standard hack whose output should not be trusted.
  std::map<long, std::string> byInstanceNumber;
  bool                        instanceNumbersUsable = true;
  for (const std::string & fn : entry.Files)
  {
    const auto found = m_InstanceNumbers.find(fn);
    try
    {
      const long number = std::stol(found != m_InstanceNumbers.end() ? found->second : std::string());
      instanceNumbersUsable = byInstanceNumber.emplace(number, fn).second;
    }
    catch (const std::exception &)
    {
      instanceNumbersUsable = false;
    }
    if (!instanceNumbersUsable)
    {
      break;
    }
  }
  if (instanceNumbersUsable)
  {
    entry.Files.clear();
    for (const auto & [number, fn] : byInstanceNumber)
    {
      entry.Files.push_back(fn);
    }
  }
  else
  {
    std::sort(entry.Files.begin(), entry.Files.end());
  }
  entry.Ordered = true;
}

const GDCMSeriesFileNames::SeriesUIDContainerType &
GDCMSeriesFileNames::GetSeriesUIDs()
{
  this->BuildSeriesMap();
  if (m_SeriesUIDs.empty())
  {
    itkWarningMacro("No Series were found");
  }
  return m_SeriesUIDs;
}

const GDCMSeriesFileNames::FileNamesContainerType &
GDCMSeriesFileNames::GetFileNames(const std::string serie)
{
  this->BuildSeriesMap();
  m_InputFileNames.clear();
  if (serie.empty())
  {
    // Return the first series encountered (single-series assumption).
    if (!m_SeriesUIDs.empty())
    {
      SeriesEntry & entry = m_SeriesFiles[m_SeriesUIDs.front()];
      this->OrderSeries(entry);
      m_InputFileNames = entry.Files;
    }
    else
    {
      itkWarningMacro("No Series can be found, make sure your restrictions are not too strong");
    }
    return m_InputFileNames;
  }
  const auto it = m_SeriesFiles.find(serie);
  if (it == m_SeriesFiles.end())
  {
    itkWarningMacro("No Series were found");
    return m_InputFileNames;
  }
  this->OrderSeries(it->second);
  m_InputFileNames = it->second.Files;
  return m_InputFileNames;
}

const GDCMSeriesFileNames::FileNamesContainerType &
GDCMSeriesFileNames::GetInputFileNames()
{
  // Do not specify any UID
  return this->GetFileNames("");
}

const GDCMSeriesFileNames::FileNamesContainerType &
GDCMSeriesFileNames::GetOutputFileNames()
{
  // We are trying to extract the original filename and compose it with a path:

  // There are two different approaches if directory does not exist:
  // 1. Exit
  // 2. Mkdir
  // bool SystemTools::FileExists(const char* filename)
  // bool SystemTools::FileIsDirectory(const char* name)
  m_OutputFileNames.clear();

  if (m_OutputDirectory.empty())
  {
    itkDebugMacro("No output directory was specified");
    return m_OutputFileNames;
  }

  itksys::SystemTools::ConvertToUnixSlashes(m_OutputDirectory);
  if (m_OutputDirectory.back() != '/')
  {
    m_OutputDirectory += '/';
  }

  if (!m_InputFileNames.empty())
  {
    bool hasExtension = false;
    for (const auto & inputFileName : m_InputFileNames)
    {
      // look for extension ".dcm" and ".DCM"
      std::string::size_type dcmPos = inputFileName.rfind(".dcm");
      if ((dcmPos != std::string::npos) && (dcmPos == inputFileName.length() - 4))
      {
        hasExtension = true;
      }
      else
      {
        dcmPos = inputFileName.rfind(".DCM");
        if ((dcmPos != std::string::npos) && (dcmPos == inputFileName.length() - 4))
        {
          hasExtension = true;
        }
      }

      // look for extension ".dicom" and ".DICOM"
      std::string::size_type dicomPos = inputFileName.rfind(".dicom");
      if ((dicomPos != std::string::npos) && (dicomPos == inputFileName.length() - 6))
      {
        hasExtension = true;
      }
      else
      {
        dicomPos = inputFileName.rfind(".DICOM");
        if ((dicomPos != std::string::npos) && (dicomPos == inputFileName.length() - 6))
        {
          hasExtension = true;
        }
      }

      // construct a filename, adding an extension if necessary
      std::string filename = m_OutputDirectory + itksys::SystemTools::GetFilenameName(inputFileName);
      if (!hasExtension)
      {
        // input filename has no extension, add a ".dcm"
        filename += ".dcm";
      }

      // Add the file name to the output list
      m_OutputFileNames.push_back(filename);
    }
  }
  else
  {
    itkDebugMacro("No files were found.");
  }

  return m_OutputFileNames;
}

void
GDCMSeriesFileNames::PrintSelf(std::ostream & os, Indent indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  os << indent << "InputDirectory: " << m_InputDirectory << std::endl;
  os << indent << "OutputDirectory: " << m_OutputDirectory << std::endl;

  os << indent << "InputFileNames: " << m_InputFileNames << std::endl;
  os << indent << "OutputFileNames: " << m_OutputFileNames << std::endl;

  os << indent << "SeriesUIDs: " << m_SeriesUIDs << std::endl;

  itkPrintSelfBooleanMacro(UseSeriesDetails);
  itkPrintSelfBooleanMacro(FailOnAmbiguousOrdering);
  itkPrintSelfBooleanMacro(DidUseAmbiguousOrdering);
  itkPrintSelfBooleanMacro(Recursive);
  itkPrintSelfBooleanMacro(LoadSequences);
  itkPrintSelfBooleanMacro(LoadPrivateTags);
}

void
GDCMSeriesFileNames::SetUseSeriesDetails(bool useSeriesDetails)
{
  if (m_UseSeriesDetails != useSeriesDetails)
  {
    m_UseSeriesDetails = useSeriesDetails;
    this->Modified();
  }
}
} // namespace itk
