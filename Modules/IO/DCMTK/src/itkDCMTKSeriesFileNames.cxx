/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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

namespace itk
{
DCMTKSeriesFileNames ::DCMTKSeriesFileNames()
{
  m_InputDirectory = "";
  m_OutputDirectory = "";
  m_UseSeriesDetails = true;
  m_Recursive = false;
  m_LoadSequences = false;
  m_LoadPrivateTags = false;
}

DCMTKSeriesFileNames::~DCMTKSeriesFileNames() = default;

void
DCMTKSeriesFileNames::SetInputDirectory(const char * name)
{
  if (!name)
  {
    itkExceptionMacro(<< "SetInputDirectory() received a nullptr string");
  }
  std::string fname = name;
  this->SetInputDirectory(fname);
}

void
DCMTKSeriesFileNames::SetInputDirectory(std::string const & name)
{
  if (name.empty())
  {
    itkWarningMacro(<< "You need to specify a directory where "
                       "the DICOM files are located");
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

void
DCMTKSeriesFileNames::GetDicomData(const std::string & series, bool saveFileNames)
{
  if (saveFileNames)
  {
    this->m_InputFileNames.clear();
  }
  this->m_SeriesUIDs.clear();

  // make an absolute path from whatever is passed in
  std::string fullPath = itksys::SystemTools::CollapseFullPath(m_InputDirectory.c_str());

  // work in Unix filename conventions, but convert to actually use filename
  itksys::SystemTools::ConvertToUnixSlashes(fullPath);

  std::string localFilePath = fullPath;

  // load the directory
  itksys::Directory directory;
  directory.Load(localFilePath.c_str());

  unsigned int numFiles = directory.GetNumberOfFiles();

  std::vector<DCMTKFileReader *> allHeaders;

  for (unsigned int i = 0; i < numFiles; i++)
  {
    std::string curFile = directory.GetFile(i);
    if (curFile == "." || curFile == "..")
    {
      continue;
    }
    localFilePath = fullPath;
    localFilePath += '/';
    localFilePath += curFile;
    if (!itksys::SystemTools::FileIsDirectory(localFilePath.c_str()))
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
      std::string uid;
      reader->GetElementUI(0x0020, 0x000e, uid);
      //
      // if you've restricked it to a particular series instance ID
      if (series.empty() || series == uid)
      {
        allHeaders.push_back(reader);
      }
      else
      {
        delete reader;
      }
      //
      // save the UID at any rate
      this->m_SeriesUIDs.push_back(uid);
    }
  }

  if (saveFileNames)
  {
    std::sort(allHeaders.begin(), allHeaders.end(), CompareDCMTKFileReaders);
  }
  //
  // save the filenames, and delete the headers
  for (auto & allHeader : allHeaders)
  {
    if (saveFileNames)
    {
      m_InputFileNames.push_back(allHeader->GetFileName());
    }
    delete allHeader;
  }
}

const DCMTKSeriesFileNames::FileNamesContainerType &
DCMTKSeriesFileNames::GetFileNames(const std::string series)
{
  this->GetDicomData(series, true);
  return m_InputFileNames;
}

const DCMTKSeriesFileNames::SeriesUIDContainerType &
DCMTKSeriesFileNames::GetSeriesUIDs()
{
  this->GetDicomData("", false);
  return this->m_SeriesUIDs;
}


const DCMTKSeriesFileNames::FileNamesContainerType &
DCMTKSeriesFileNames::GetInputFileNames()
{
  // Do not specify any UID
  this->GetDicomData("", true);
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

  for (unsigned int i = 0; i < m_InputFileNames.size(); i++)
  {
    os << indent << "InputFileNames[" << i << "]: " << m_InputFileNames[i] << std::endl;
  }

  for (unsigned int i = 0; i < m_OutputFileNames.size(); i++)
  {
    os << indent << "OutputFileNames[" << i << "]: " << m_OutputFileNames[i] << std::endl;
  }

  for (unsigned int i = 0; i < m_SeriesUIDs.size(); i++)
  {
    os << indent << "SeriesUIDs[" << i << "]: " << m_SeriesUIDs[i] << std::endl;
  }

  if (m_UseSeriesDetails)
  {
    os << indent << "UseSeriesDetails: True" << std::endl;
  }
  else
  {
    os << indent << "UseSeriesDetails: False" << std::endl;
  }

  if (m_Recursive)
  {
    os << indent << "Recursive: True" << std::endl;
  }
  else
  {
    os << indent << "Recursive: False" << std::endl;
  }

  if (m_LoadSequences)
  {
    os << indent << "LoadSequences: True" << std::endl;
  }
  else
  {
    os << indent << "LoadSequences: False" << std::endl;
  }

  if (m_LoadPrivateTags)
  {
    os << indent << "LoadPrivateTags: True" << std::endl;
  }
  else
  {
    os << indent << "LoadPrivateTags: False" << std::endl;
  }
}

void
DCMTKSeriesFileNames::SetUseSeriesDetails(bool useSeriesDetails)
{
  m_UseSeriesDetails = useSeriesDetails;
  //  m_SerieHelper->SetUseSeriesDetails(m_UseSeriesDetails);
  //  m_SerieHelper->CreateDefaultUniqueSeriesIdentifier();
}
} // namespace itk
