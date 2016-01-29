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
#ifndef _itkGDCMSeriesFileNames_h
#define _itkGDCMSeriesFileNames_h

#include "itkGDCMSeriesFileNames.h"
#include "itksys/SystemTools.hxx"
#include "itkProgressReporter.h"

namespace itk
{
GDCMSeriesFileNames::GDCMSeriesFileNames()
{
  m_SerieHelper = new gdcm::SerieHelper();
  m_InputDirectory = "";
  m_OutputDirectory = "";
  m_UseSeriesDetails = true;
  m_Recursive = false;
  m_LoadSequences = false;
  m_LoadPrivateTags = false;
}

GDCMSeriesFileNames::~GDCMSeriesFileNames()
{
  delete m_SerieHelper;
}

#if !defined( ITK_LEGACY_REMOVE )
gdcm::SerieHelper * GDCMSeriesFileNames::GetSeriesHelper(void)
{
  itkLegacyBodyMacro(GDCMSeriesFileNames::GetSeriesHelper, 4.9);
  return m_SerieHelper;
}
#endif

void GDCMSeriesFileNames::SetInputDirectory(const char *name)
{
  if ( !name )
    {
    itkExceptionMacro(<< "SetInputDirectory() received a ITK_NULLPTR string");
    }
  std::string fname = name;
  this->SetInputDirectory(fname);
}

void GDCMSeriesFileNames::SetInputDirectory(std::string const & name)
{
  if ( name == "" )
    {
    itkWarningMacro(<< "You need to specify a directory where "
                       "the DICOM files are located");
    return;
    }
  if ( m_InputDirectory == name )
    {
    return;
    }
  if ( !itksys::SystemTools::FileIsDirectory( name.c_str() ) )
    {
    itkWarningMacro(<< name << " is not a directory");
    return;
    }
  m_InputDirectory = name;
  m_SerieHelper->Clear();
  m_SerieHelper->SetUseSeriesDetails(m_UseSeriesDetails);
  m_SerieHelper->SetLoadMode( ( m_LoadSequences ? 0 : gdcm::LD_NOSEQ )
                              | ( m_LoadPrivateTags ? 0 : gdcm::LD_NOSHADOW ) );
  m_SerieHelper->SetDirectory(name, m_Recursive);
  //as a side effect it also execute
  this->Modified();
}

const GDCMSeriesFileNames::SeriesUIDContainerType & GDCMSeriesFileNames::GetSeriesUIDs()
{
  m_SeriesUIDs.clear();
  // Accessing the first serie found (assume there is at least one)
  gdcm::FileList *flist = m_SerieHelper->GetFirstSingleSerieUIDFileSet();
  while ( flist )
    {
    if ( flist->size() ) //make sure we have at leat one serie
      {
      gdcm::File *file = ( *flist )[0]; //for example take the first one

      // Create its unique series ID
      std::string id = m_SerieHelper->
                       CreateUniqueSeriesIdentifier(file).c_str();

      m_SeriesUIDs.push_back( id.c_str() );
      }
    flist = m_SerieHelper->GetNextSingleSerieUIDFileSet();
    }
  if ( !m_SeriesUIDs.size() )
    {
    itkWarningMacro(<< "No Series were found");
    }
  return m_SeriesUIDs;
}

const GDCMSeriesFileNames::FileNamesContainerType & GDCMSeriesFileNames::GetFileNames(const std::string serie)
{
  m_InputFileNames.clear();
  // Accessing the first serie found (assume there is at least one)
  gdcm::FileList *flist = m_SerieHelper->GetFirstSingleSerieUIDFileSet();
  if ( !flist )
    {
    itkWarningMacro(
      << "No Series can be found, make sure your restrictions are not too strong");
    return m_InputFileNames;
    }
  if ( serie != "" ) // user did not specify any sub selection based on UID
    {
    bool found = false;
    while ( flist && !found )
      {
      if ( flist->size() ) //make sure we have at leat one serie
        {
        gdcm::File *file = ( *flist )[0]; //for example take the first one
        std::string id = m_SerieHelper->
                         CreateUniqueSeriesIdentifier(file).c_str();

        if ( id == serie )
          {
          found = true; // we found a match
          break;
          }
        }
      flist = m_SerieHelper->GetNextSingleSerieUIDFileSet();
      }
    if ( !found )
      {
      itkWarningMacro(<< "No Series were found");
      return m_InputFileNames;
      }
    }
  m_SerieHelper->OrderFileList(flist);

  gdcm::FileList::iterator it;
  if ( flist->size() )
    {
    ProgressReporter progress(this, 0,
      static_cast<itk::SizeValueType>(flist->size()), 10);
    for ( it = flist->begin();
          it != flist->end(); ++it )
      {
#if GDCM_MAJOR_VERSION < 2
      gdcm::File *header = *it;
      if ( !header )
        {
        itkWarningMacro(<< "GDCMSeriesFileNames got ITK_NULLPTR header, "
                           "this is a serious bug");
        continue;
        }
      if ( !header->IsReadable() )
        {
        itkWarningMacro( << "GDCMSeriesFileNames got a non DICOM file:"
                         << header->GetFileName() );
        continue;
        }
      m_InputFileNames.push_back( header->GetFileName() );
      progress.CompletedPixel();
#else
      gdcm::FileWithName *header = *it;
      m_InputFileNames.push_back(header->filename);
      progress.CompletedPixel();
#endif
      }
    }
  else
    {
    itkDebugMacro(<< "No files were found");
    }

  return m_InputFileNames;
}

const GDCMSeriesFileNames::FileNamesContainerType & GDCMSeriesFileNames::GetInputFileNames()
{
  // Do not specify any UID
  return this->GetFileNames("");
}

const GDCMSeriesFileNames::FileNamesContainerType & GDCMSeriesFileNames::GetOutputFileNames()
{
  // We are trying to extract the original filename and compose it with a path:

  //There are two different approaches if directory does not exist:
  // 1. Exit
  // 2. Mkdir
  //bool SystemTools::FileExists(const char* filename)
  //bool SystemTools::FileIsDirectory(const char* name)
  m_OutputFileNames.clear();

  if ( m_OutputDirectory.empty() )
    {
    itkDebugMacro(<< "No output directory was specified");
    return m_OutputFileNames;
    }

  itksys::SystemTools::ConvertToUnixSlashes(m_OutputDirectory);
  if ( m_OutputDirectory[m_OutputDirectory.size() - 1] != '/' )
    {
    m_OutputDirectory += '/';
    }

  if ( m_InputFileNames.size() )
    {
    bool hasExtension = false;
    for ( std::vector< std::string >::const_iterator it = m_InputFileNames.begin();
          it != m_InputFileNames.end(); ++it )
      {
      // look for extension ".dcm" and ".DCM"
      std::string::size_type dcmPos = ( *it ).rfind(".dcm");
      if ( ( dcmPos != std::string::npos )
           && ( dcmPos == ( *it ).length() - 4 ) )
        {
        hasExtension = true;
        }
      else
        {
        dcmPos = ( *it ).rfind(".DCM");
        if ( ( dcmPos != std::string::npos )
             && ( dcmPos == ( *it ).length() - 4 ) )
          {
          hasExtension = true;
          }
        }

      // look for extension ".dicom" and ".DICOM"
      std::string::size_type dicomPos = ( *it ).rfind(".dicom");
      if ( ( dicomPos != std::string::npos )
           && ( dicomPos == ( *it ).length() - 6 ) )
        {
        hasExtension = true;
        }
      else
        {
        dicomPos = ( *it ).rfind(".DICOM");
        if ( ( dicomPos != std::string::npos )
             && ( dicomPos == ( *it ).length() - 6 ) )
          {
          hasExtension = true;
          }
        }

      // construct a filename, adding an extension if necessary
      std::string filename =
        m_OutputDirectory + itksys::SystemTools::GetFilenameName(*it);
      if ( !hasExtension )
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
    itkDebugMacro(<< "No files were found.");
    }

  return m_OutputFileNames;
}

void GDCMSeriesFileNames::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i;
  os << indent << "InputDirectory: " << m_InputDirectory << std::endl;
  os << indent << "LoadSequences:" << m_LoadSequences << std::endl;
  os << indent << "LoadPrivateTags:" << m_LoadPrivateTags << std::endl;
  if ( m_Recursive )
    {
    os << indent << "Recursive: True" << std::endl;
    }
  else
    {
    os << indent << "Recursive: False" << std::endl;
    }

  for ( i = 0; i < m_InputFileNames.size(); i++ )
    {
    os << indent << "InputFileNames[" << i << "]: " << m_InputFileNames[i] << std::endl;
    }

  os << indent << "OutputDirectory: " << m_OutputDirectory << std::endl;
  for ( i = 0; i < m_OutputFileNames.size(); i++ )
    {
    os << indent << "OutputFileNames[" << i << "]: " << m_OutputFileNames[i] << std::endl;
    }
}

void GDCMSeriesFileNames::SetUseSeriesDetails(bool useSeriesDetails)
{
  m_UseSeriesDetails = useSeriesDetails;
  m_SerieHelper->SetUseSeriesDetails(m_UseSeriesDetails);
  m_SerieHelper->CreateDefaultUniqueSeriesIdentifier();
}
} //namespace ITK

#endif
