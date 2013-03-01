/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef _itkDICOMSeriesFileNames_h
#define _itkDICOMSeriesFileNames_h

#include "itksys/SystemTools.hxx"
#include "itksys/Directory.hxx"
#include "itkDICOMSeriesFileNames.h"

namespace itk
{
DICOMSeriesFileNames
::DICOMSeriesFileNames():
  m_Ascending(true),
  m_Directory(""),
  m_FileNameSortingOrder(SortByImagePositionPatient)
{}

const DICOMSeriesFileNames::FileNamesArrayType &
DICOMSeriesFileNames
::GetFileNames(bool recursive)
{
  // make sure the SeriesUIDs are up to date
  this->GetSeriesUIDs(recursive);

  if ( m_SeriesUIDs.size() > 0 )
    {
    return this->GetFileNames( *m_SeriesUIDs.begin() );
    }
  else
    {
    m_FileNames.clear();
    return m_FileNames;
    }
}

const DICOMSeriesFileNames::FileNamesArrayType &
DICOMSeriesFileNames
::GetFileNames(const std::string & seriesUID, bool recursive)
{
  if ( m_Directory == "" )
    {
    itkExceptionMacro (<< "No directory defined!");
    }

  // Make sure the SeriesUIDs are up to date. This may require the
  // directory to be scanned.
  this->GetSeriesUIDs(recursive);

  // Get the filenames, sorted by user selection.
  m_FileNames.clear();
  if ( m_SeriesUIDs.size() > 0 )
    {
    switch ( m_FileNameSortingOrder )
      {
      case SortByImageNumber:
        {
        std::vector< std::pair< int, std::string > > iSortedFileNames;
        m_AppHelper.GetSliceNumberFilenamePairs(seriesUID,
                                                iSortedFileNames,
                                                m_Ascending);
        for ( std::vector< std::pair< int, std::string > >::iterator it =
                iSortedFileNames.begin(); it != iSortedFileNames.end(); ++it )
          {
          m_FileNames.push_back( ( *it ).second );
          }
        }
        break;

      case SortBySliceLocation:
        {
        std::vector< std::pair< float, std::string > > fSortedFileNames;
        m_AppHelper.GetSliceLocationFilenamePairs(seriesUID,
                                                  fSortedFileNames,
                                                  m_Ascending);
        for ( std::vector< std::pair< float, std::string > >::iterator it =
                fSortedFileNames.begin(); it != fSortedFileNames.end(); ++it )
          {
          m_FileNames.push_back( ( *it ).second );
          }
        }
        break;

      case SortByImagePositionPatient:
        {
        std::vector< std::pair< float, std::string > > fSortedFileNames;
        m_AppHelper.GetImagePositionPatientFilenamePairs(seriesUID,
                                                         fSortedFileNames,
                                                         m_Ascending);
        for ( std::vector< std::pair< float, std::string > >::iterator it =
                fSortedFileNames.begin(); it != fSortedFileNames.end(); ++it )
          {
          m_FileNames.push_back( ( *it ).second );
          }
        }
        break;
      }
    }
  return m_FileNames;
}

void
DICOMSeriesFileNames
::RecurseDirectory(std::string directory, FileNamesArrayType & filenames)
{
  itksys::Directory dicomDir;

  if ( !dicomDir.Load ( directory.c_str() ) )
    {
    itkExceptionMacro (<< "Directory " << directory.c_str() << " cannot be read!");
    }

  for ( unsigned long i = 0; i < dicomDir.GetNumberOfFiles(); i++ )
    {
    // Only read files
    if ( itksys::SystemTools::FileIsDirectory( ( directory + "/" + dicomDir.GetFile(i) ).c_str() ) )
      {
      if ( strcmp(dicomDir.GetFile(i), ".") != 0
           && strcmp(dicomDir.GetFile(i), "..") != 0 )
        {
        this->RecurseDirectory( ( directory + "/" + dicomDir.GetFile(i) ).c_str(), filenames );
        }
      continue;
      }

    // store the full filename
    filenames.push_back( directory + "/" + dicomDir.GetFile(i) );
    }
}

std::string
DICOMSeriesFileNames
::GetFileName(const std::string & instanceUID)
{
  return m_AppHelper.GetFileName(instanceUID);
}

const DICOMSeriesFileNames::FileNamesArrayType &
DICOMSeriesFileNames
::GetSeriesUIDs(bool recursive)
{
  if ( m_Directory == "" )
    {
    itkExceptionMacro (<< "No directory defined!");
    }

  // Check whether we need to rescan the directory
  if ( m_DirectorySetTime < m_DirectoryScanTime )
    {
    return m_SeriesUIDs;
    }

  // Process all files in the directory
  itksys::Directory dicomDir;
  if ( !dicomDir.Load ( m_Directory.c_str() ) )
    {
    itkExceptionMacro (<< "Directory " << m_Directory.c_str() << " cannot be read!");
    }

  // Initialize the AppHelper
  m_AppHelper.Clear();

  // Scan directory for files
  FileNamesArrayType filenames;
  for ( unsigned long i = 0; i < dicomDir.GetNumberOfFiles(); i++ )
    {
    // Only read files
    if ( itksys::SystemTools::FileIsDirectory( ( m_Directory + "/" + dicomDir.GetFile(i) ).c_str() ) && recursive )
      {
      if ( strcmp(dicomDir.GetFile(i), ".") != 0
           && strcmp(dicomDir.GetFile(i), "..") != 0 )
        {
        this->RecurseDirectory( ( m_Directory + "/" + dicomDir.GetFile(i) ).c_str(), filenames );
        }
      continue;
      }

    // store the full filename
    filenames.push_back( m_Directory + "/" + dicomDir.GetFile(i) );
    }

  // Scan the header of each file
  FileNamesArrayType::iterator iter;
  for ( iter = filenames.begin(); iter != filenames.end(); ++iter )
    {
    const char *fn = ( *iter ).c_str();
    m_Parser.OpenFile(fn);
    m_Parser.ClearAllDICOMTagCallbacks();
    m_AppHelper.RegisterCallbacks(&m_Parser);
    m_Parser.ReadHeader();
    }

  if ( this->GetDebug() )
    {
    m_AppHelper.OutputSeries();
    }

  // Get the SeriesUIDs
  m_AppHelper.GetSeriesUIDs(m_SeriesUIDs);
  m_AppHelper.GetSeriesDescriptions(m_SeriesDescriptions);
  m_AppHelper.GetBodyParts(m_BodyParts);
  m_AppHelper.GetScanOptions(m_ScanOptions);

  // Keep track of when we scanned the directory last
  m_DirectoryScanTime.Modified();

  return m_SeriesUIDs;
}

int
DICOMSeriesFileNames
::CanReadFile(const char *fname)
{
  bool canOpen = m_Parser.OpenFile(fname);

  if ( canOpen == false )
    {
    itkWarningMacro(<<  "DICOMParser couldn't open : " << fname);
    return 0;
    }
  bool canRead = m_Parser.IsDICOMFile();
  if ( canRead == true )
    {
    return 1;
    }
  else
    {
    return 0;
    }
}

void
DICOMSeriesFileNames
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Directory: " << m_Directory << std::endl;
  os << indent << "Ascending: " << ( m_Ascending ? "On" : "Off" ) << std::endl;
  os << indent << "File name sorting order: ";
  switch ( m_FileNameSortingOrder )
    {
    case SortByImageNumber:
      os << "SortByImageNumber" << std::endl;
      break;
    case SortBySliceLocation:
      os << "SortBySliceLocation" << std::endl;
      break;
    case SortByImagePositionPatient:
      os << "SortByImagePositionPatient"
         << std::endl;
      break;
    }

  for ( unsigned int i = 0; i < m_FileNames.size(); i++ )
    {
    os << indent << "FileNames[" << i << "]: " << m_FileNames[i] << std::endl;
    }
}
} //namespace ITK

#endif
