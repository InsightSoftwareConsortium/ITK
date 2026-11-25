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

#include "itkDCMTKSeriesFileNames.h"
#include "itksys/SystemTools.hxx"
#include "itkProgressReporter.h"
#include "itkDCMTKFileReader.h"
#include "itkPrintHelper.h"
#include "itksys/Directory.hxx"
#include <algorithm>
#include <exception>
#include <vector>

namespace itk
{
namespace
{
// Order slices by geometric position (ImagePositionPatient projected on the
// slice normal), matching itk::GDCMSeriesFileNames / gdcm::SerieHelper.
// Falls back to InstanceNumber then file name when orientation/position is
// absent, so an unset or constant InstanceNumber can no longer scramble the
// order via an unstable sort.
void
OrderFileReadersByPosition(std::vector<DCMTKFileReader *> & headers)
{
  if (headers.size() < 2)
  {
    return;
  }

  // Derive the slice normal from the first slice that carries a valid
  // orientation; a single mis-globbed entry without one must not suppress
  // geometric ordering for the whole series.
  double dircos[6] = {};
  bool   haveOrientation = false;
  for (auto * reader : headers)
  {
    if (reader->GetDirCosArray(dircos) == EXIT_SUCCESS)
    {
      haveOrientation = true;
      break;
    }
  }
  const double normal[3] = { dircos[1] * dircos[5] - dircos[2] * dircos[4],
                             dircos[2] * dircos[3] - dircos[0] * dircos[5],
                             dircos[0] * dircos[4] - dircos[1] * dircos[3] };

  struct SortKey
  {
    DCMTKFileReader * reader;
    double            distance;
    long              instanceNumber;
  };
  std::vector<SortKey> keys;
  keys.reserve(headers.size());
  bool useGeometry = haveOrientation;
  for (auto * reader : headers)
  {
    double origin[3] = { 0.0, 0.0, 0.0 };
    useGeometry = (reader->GetOrigin(origin) == EXIT_SUCCESS) && useGeometry;
    const double distance = normal[0] * origin[0] + normal[1] * origin[1] + normal[2] * origin[2];
    keys.push_back({ reader, distance, reader->GetFileNumber() });
  }

  std::stable_sort(keys.begin(), keys.end(), [useGeometry](const SortKey & a, const SortKey & b) {
    if (useGeometry && a.distance != b.distance)
    {
      return a.distance < b.distance;
    }
    if (a.instanceNumber != b.instanceNumber)
    {
      return a.instanceNumber < b.instanceNumber;
    }
    return a.reader->GetFileName() < b.reader->GetFileName();
  });

  std::transform(keys.begin(), keys.end(), headers.begin(), [](const SortKey & k) { return k.reader; });
}

// Collect file paths under dir, descending into subdirectories when recursive.
void
CollectCandidateFiles(const std::string & dir, bool recursive, std::vector<std::string> & out)
{
  itksys::Directory directory;
  directory.Load(dir.c_str());
  for (unsigned int i = 0; i < directory.GetNumberOfFiles(); ++i)
  {
    const std::string curFile = directory.GetFile(i);
    if (curFile == "." || curFile == "..")
    {
      continue;
    }
    std::string path = dir;
    path += '/';
    path += curFile;
    if (itksys::SystemTools::FileIsDirectory(path.c_str()))
    {
      if (recursive)
      {
        CollectCandidateFiles(path, recursive, out);
      }
    }
    else
    {
      out.push_back(path);
    }
  }
}
} // namespace

DCMTKSeriesFileNames::DCMTKSeriesFileNames()
{
  m_InputDirectory = "";
  m_OutputDirectory = "";
  m_Recursive = false;
  m_LoadSequences = false;
  m_LoadPrivateTags = false;
  this->SetUseSeriesDetails(true); // seeds the default series-detail restrictions
}

DCMTKSeriesFileNames::~DCMTKSeriesFileNames() = default;

void
DCMTKSeriesFileNames::SetInputDirectory(const char * name)
{
  if (!name)
  {
    itkExceptionStringMacro("SetInputDirectory() received a nullptr string");
  }
  std::string fname = name;
  this->SetInputDirectory(fname);
}

void
DCMTKSeriesFileNames::SetInputDirectory(const std::string & name)
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
  // as a side effect it also execute
  this->Modified();
}

std::string
DCMTKSeriesFileNames::CreateSeriesIdentifier(DCMTKFileReader * reader) const
{
  std::string id;
  reader->GetElementUI(0x0020, 0x000e, id, false); // Series Instance UID
  if (m_UseSeriesDetails)
  {
    for (const auto & [group, element] : m_Restrictions)
    {
      std::string value;
      reader->GetElementAsString(group, element, value, false);
      id += '_';
      id += value;
    }
  }
  return id;
}

void
DCMTKSeriesFileNames::BuildSeriesMap()
{
  // Reuse the previous parse unless the object has been modified since.
  if (m_CacheBuildTime.GetMTime() > this->GetMTime())
  {
    return;
  }

  this->m_SeriesUIDs.clear();
  this->m_SeriesFiles.clear();

  // make an absolute path from whatever is passed in
  std::string fullPath = itksys::SystemTools::CollapseFullPath(m_InputDirectory.c_str());
  // work in Unix filename conventions, but convert to actually use filename
  itksys::SystemTools::ConvertToUnixSlashes(fullPath);

  std::vector<std::string> candidateFiles;
  CollectCandidateFiles(fullPath, m_Recursive, candidateFiles);

  // Group readers by series identifier, preserving first-encountered order
  // of the distinct identifiers.
  std::map<std::string, std::vector<DCMTKFileReader *>> groups;
  for (const std::string & localFilePath : candidateFiles)
  {
    if (!DCMTKFileReader::IsImageFile(localFilePath))
    {
      continue;
    }
    auto * reader = new DCMTKFileReader;
    try
    {
      reader->SetFileName(localFilePath);
      reader->LoadFile();
    }
    catch (...)
    {
      delete reader;
      continue;
    }
    const std::string id = this->CreateSeriesIdentifier(reader);
    if (groups.find(id) == groups.end())
    {
      this->m_SeriesUIDs.push_back(id);
    }
    groups[id].push_back(reader);
  }

  // Order each series geometrically and store its file names; free the headers.
  for (auto & [id, readers] : groups)
  {
    OrderFileReadersByPosition(readers);
    FileNamesContainerType names;
    names.reserve(readers.size());
    for (auto * reader : readers)
    {
      names.push_back(reader->GetFileName());
      delete reader;
    }
    this->m_SeriesFiles[id] = std::move(names);
  }

  m_CacheBuildTime.Modified();
}

const DCMTKSeriesFileNames::FileNamesContainerType &
DCMTKSeriesFileNames::GetFileNames(const std::string series)
{
  this->BuildSeriesMap();
  const auto it = m_SeriesFiles.find(series);
  if (it == m_SeriesFiles.end())
  {
    itkWarningMacro("No files were found for series " << series);
    m_InputFileNames.clear();
  }
  else
  {
    m_InputFileNames = it->second;
  }
  return m_InputFileNames;
}

const DCMTKSeriesFileNames::SeriesUIDContainerType &
DCMTKSeriesFileNames::GetSeriesUIDs()
{
  this->BuildSeriesMap();
  return this->m_SeriesUIDs;
}


const DCMTKSeriesFileNames::FileNamesContainerType &
DCMTKSeriesFileNames::GetInputFileNames()
{
  this->BuildSeriesMap();
  // Return the first series encountered (single-series assumption), matching
  // itk::GDCMSeriesFileNames.
  m_InputFileNames.clear();
  if (!m_SeriesUIDs.empty())
  {
    m_InputFileNames = m_SeriesFiles[m_SeriesUIDs.front()];
  }
  return this->m_InputFileNames;
}

const DCMTKSeriesFileNames::FileNamesContainerType &
DCMTKSeriesFileNames::GetOutputFileNames()
{
  return m_OutputFileNames;
}

void
DCMTKSeriesFileNames::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "InputDirectory: " << m_InputDirectory << std::endl;
  os << indent << "OutputDirectory: " << m_OutputDirectory << std::endl;

  for (unsigned int i = 0; i < m_InputFileNames.size(); ++i)
  {
    os << indent << "InputFileNames[" << i << "]: " << m_InputFileNames[i] << std::endl;
  }

  for (unsigned int i = 0; i < m_OutputFileNames.size(); ++i)
  {
    os << indent << "OutputFileNames[" << i << "]: " << m_OutputFileNames[i] << std::endl;
  }

  for (unsigned int i = 0; i < m_SeriesUIDs.size(); ++i)
  {
    os << indent << "SeriesUIDs[" << i << "]: " << m_SeriesUIDs[i] << std::endl;
  }

  itkPrintSelfBooleanMacro(UseSeriesDetails);
  itkPrintSelfBooleanMacro(Recursive);
  itkPrintSelfBooleanMacro(LoadSequences);
  itkPrintSelfBooleanMacro(LoadPrivateTags);
}

void
DCMTKSeriesFileNames::SetUseSeriesDetails(bool useSeriesDetails)
{
  m_UseSeriesDetails = useSeriesDetails;
  m_Restrictions.clear();
  if (m_UseSeriesDetails)
  {
    // Default detail tags, matching gdcm::SerieHelper::CreateDefaultUniqueSeriesIdentifier.
    m_Restrictions.emplace_back(0x0020, 0x0011); // Series Number
    m_Restrictions.emplace_back(0x0018, 0x0024); // Sequence Name
    m_Restrictions.emplace_back(0x0018, 0x0050); // Slice Thickness
    m_Restrictions.emplace_back(0x0028, 0x0010); // Rows
    m_Restrictions.emplace_back(0x0028, 0x0011); // Columns
  }
  this->Modified();
}

void
DCMTKSeriesFileNames::AddSeriesRestriction(const std::string & tag)
{
  // Parse a "group|element" tag (hex) and append it to the identifier criteria.
  const std::string::size_type bar = tag.find('|');
  if (bar == std::string::npos || bar == 0 || bar + 1 >= tag.size())
  {
    itkWarningMacro("Ignoring malformed series restriction tag '" << tag << "' (expected \"group|element\")");
    return;
  }
  unsigned long group = 0;
  unsigned long element = 0;
  try
  {
    group = std::stoul(tag.substr(0, bar), nullptr, 16);
    element = std::stoul(tag.substr(bar + 1), nullptr, 16);
  }
  catch (const std::exception &)
  {
    itkWarningMacro("Ignoring malformed series restriction tag '" << tag << "' (expected \"group|element\")");
    return;
  }
  if (group > 0xFFFF || element > 0xFFFF)
  {
    itkWarningMacro("Ignoring out-of-range series restriction tag '" << tag << "' (group/element exceed 16 bits)");
    return;
  }
  m_Restrictions.emplace_back(static_cast<unsigned short>(group), static_cast<unsigned short>(element));
  this->Modified();
}
} // namespace itk
