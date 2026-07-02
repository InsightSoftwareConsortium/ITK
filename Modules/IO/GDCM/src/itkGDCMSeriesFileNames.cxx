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
// Order one series geometrically with gdcm::IPPSorter (ImagePositionPatient
// projected on the slice normal). IPPSorter is strict: it FAILS on duplicate
// IPP and gantry-tilt acquisitions (see issue #6468). On failure the input
// order is left unchanged rather than fabricating an order.
std::vector<std::string>
OrderSeriesGeometrically(const std::vector<std::string> & files)
{
  if (files.size() < 2)
  {
    return files;
  }
  gdcm::IPPSorter sorter;
  sorter.SetComputeZSpacing(false);
  if (sorter.Sort(files))
  {
    return sorter.GetFilenames();
  }
  return files;
}
} // namespace


GDCMSeriesFileNames::GDCMSeriesFileNames()
{
  this->SetUseSeriesDetails(true); // seeds the default series-detail tags
}

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
    m_RefineTags.emplace_back(group, element);
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

  const gdcm::Tag seriesUID(0x0020, 0x000e);
  gdcm::Scanner   scanner;
  scanner.AddTag(seriesUID);
  if (m_UseSeriesDetails)
  {
    for (const auto & [group, element] : m_RefineTags)
    {
      scanner.AddTag(gdcm::Tag(group, element));
    }
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
    if (m_UseSeriesDetails)
    {
      for (const auto & [group, element] : m_RefineTags)
      {
        const char *      value = scanner.GetValue(fn, gdcm::Tag(group, element));
        const std::string s = (value != nullptr) ? value : "";
        if (id == uid && !s.empty())
        {
          id += '.';
        }
        id += s;
      }
    }
    // Eliminate all non-alphanumeric characters (keep '.').
    id.erase(std::remove_if(id.begin(), id.end(), [](unsigned char c) { return c != '.' && std::isalnum(c) == 0; }),
             id.end());
    return id;
  };

  std::map<std::string, FileNamesContainerType> grouped;
  for (const std::string & fn : filenames)
  {
    if (!scanner.IsKey(fn.c_str()))
    {
      continue; // not a DICOM file the scanner could read
    }
    const std::string id = makeIdentifier(fn.c_str());
    if (grouped.find(id) == grouped.end())
    {
      m_SeriesUIDs.push_back(id);
    }
    grouped[id].push_back(fn);
  }

  for (auto & [id, files] : grouped)
  {
    m_SeriesFiles[id] = OrderSeriesGeometrically(files);
  }

  m_CacheBuildTime.Modified();
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
      m_InputFileNames = m_SeriesFiles[m_SeriesUIDs.front()];
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
  m_InputFileNames = it->second;
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
  itkPrintSelfBooleanMacro(Recursive);
  itkPrintSelfBooleanMacro(LoadSequences);
  itkPrintSelfBooleanMacro(LoadPrivateTags);
}

void
GDCMSeriesFileNames::SetUseSeriesDetails(bool useSeriesDetails)
{
  m_UseSeriesDetails = useSeriesDetails;
  m_RefineTags.clear();
  if (m_UseSeriesDetails)
  {
    // Default detail tags, matching gdcm::SerieHelper::CreateDefaultUniqueSeriesIdentifier.
    m_RefineTags.emplace_back(0x0020, 0x0011); // Series Number
    m_RefineTags.emplace_back(0x0018, 0x0024); // Sequence Name
    m_RefineTags.emplace_back(0x0018, 0x0050); // Slice Thickness
    m_RefineTags.emplace_back(0x0028, 0x0010); // Rows
    m_RefineTags.emplace_back(0x0028, 0x0011); // Columns
  }
  this->Modified();
}
} // namespace itk
