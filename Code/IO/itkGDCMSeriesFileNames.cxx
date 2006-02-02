/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGDCMSeriesFileNames.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGDCMSeriesFileNames_h
#define _itkGDCMSeriesFileNames_h

#include "itkGDCMSeriesFileNames.h"
#include "gdcm/src/gdcmSerieHelper.h"
#include "gdcm/src/gdcmFile.h"
#include "gdcm/src/gdcmUtil.h"
#include <itksys/SystemTools.hxx>

#include <vector>
#include <string>

namespace itk
{

GDCMSeriesFileNames::GDCMSeriesFileNames()
{
  m_SerieHelper = new gdcm::SerieHelper();
  m_InputDirectory = "";
  m_OutputDirectory = "";
  m_UseSeriesDetails = true;
}

GDCMSeriesFileNames::~GDCMSeriesFileNames()
{
  delete m_SerieHelper;
}

void GDCMSeriesFileNames::SetInputDirectory (std::string const &name)
{
  if ( name == "" )
    {
    itkWarningMacro( << "You need to specify a directory where "
      "the DICOM files are located");
    return;
    }
  if( m_InputDirectory == name )
    {
    return;
    }
  m_InputDirectory = name;
  m_SerieHelper->Clear();
  m_SerieHelper->SetUseSeriesDetails( m_UseSeriesDetails );
  m_SerieHelper->SetDirectory( name ); //as a side effect it also execute
  this->Modified();
}

const SerieUIDContainer &GDCMSeriesFileNames::GetSeriesUIDs() 
{
  m_SeriesUIDs.clear();
  // Accessing the first serie found (assume there is at least one)
  gdcm::FileList *flist = m_SerieHelper->GetFirstSingleSerieUIDFileSet();
  while(flist)
    {
    if( flist->size() ) //make sure we have at leat one serie
      {
      gdcm::File *file = (*flist)[0]; //for example take the first one

      // Create its unique series ID
      std::string id = m_SerieHelper->
                       CreateUniqueSeriesIdentifier( file ).c_str();

      m_SeriesUIDs.push_back( id.c_str() );
      }
    flist = m_SerieHelper->GetNextSingleSerieUIDFileSet();
    }
  if( !m_SeriesUIDs.size() )
    {
    itkWarningMacro(<<"No Series were found");
    }
  return m_SeriesUIDs;
}

const FilenamesContainer &GDCMSeriesFileNames::GetFileNames(const std::string serie) 
{
  m_InputFileNames.clear();
  // Accessing the first serie found (assume there is at least one)
  gdcm::FileList *flist = m_SerieHelper->GetFirstSingleSerieUIDFileSet();
  if( !flist )
    {
    itkWarningMacro(<<"No Series can be found, make sure you restiction are not too strong");
    return m_InputFileNames;
    }
  if( serie != "" ) // user did not specify any sub selection based on UID
    {
    bool found = false;
    while(flist && !found)
      {
      if( flist->size() ) //make sure we have at leat one serie
        {
        gdcm::File *file = (*flist)[0]; //for example take the first one
        std::string id = m_SerieHelper->
          CreateUniqueSeriesIdentifier( file ).c_str();

        if( id == serie )
          {
          found = true; // we found a match
          break;
          }
        }
      flist = m_SerieHelper->GetNextSingleSerieUIDFileSet();
      }
    if( !found)
      {
      itkWarningMacro(<<"No Series were found");
      return m_InputFileNames;
      }
    }
  m_SerieHelper->OrderFileList(flist);

  gdcm::FileList::iterator it;
  if( flist->size() )
    {
    for(it = flist->begin(); 
        it != flist->end(); ++it )
      {
        gdcm::File * header = *it;
        if( !header )
          {
          itkWarningMacro( << "GDCMSeriesFileNames got NULL header, "
            "this is a serious bug" );
          continue;
          }
        if( !header->IsReadable() )
          {
          itkWarningMacro( << "GDCMSeriesFileNames got a non DICOM file:" 
            << header->GetFileName() );
          continue;
          }
        m_InputFileNames.push_back( header->GetFileName() );
      }
    }
  else
    {
    itkDebugMacro(<<"No files were found");
    }

  return m_InputFileNames;
}

const FilenamesContainer &GDCMSeriesFileNames::GetInputFileNames() 
{
  // Do not specify any UID
  return GetFileNames("");
}

const FilenamesContainer &GDCMSeriesFileNames::GetOutputFileNames() 
{
  // We are trying to extract the original filename and compose it with a path:

  //There are two different approachs if directory does not exist:
  // 1. Exit
  // 2. Mkdir
  //bool SystemTools::FileExists(const char* filename)
  //bool SystemTools::FileIsDirectory(const char* name)
  m_OutputFileNames.clear();
  
  if( m_OutputDirectory.empty() )
    {
    itkDebugMacro(<<"No output directory was specified");
    return m_OutputFileNames;
    }

  itksys::SystemTools::ConvertToUnixSlashes( m_OutputDirectory );
  if(m_OutputDirectory[m_OutputDirectory.size()-1] != '/')
    {
    m_OutputDirectory += '/';
    }

  if( m_InputFileNames.size() )
    {
    bool hasExtension = false;
    for(std::vector<std::string>::const_iterator it = m_InputFileNames.begin();
      it != m_InputFileNames.end(); ++it )
      {
      // look for extension ".dcm" and ".DCM"
      std::string::size_type dcmPos = (*it).rfind(".dcm");
      if ( (dcmPos != std::string::npos)
           && (dcmPos == (*it).length() - 4) )
        {
        hasExtension = true;
        }
      else
        {
        dcmPos = (*it).rfind(".DCM");
        if ( (dcmPos != std::string::npos)
             && (dcmPos == (*it).length() - 4) )
          {
          hasExtension = true;
          }
        }

      // look for extension ".dicom" and ".DICOM"
      std::string::size_type dicomPos = (*it).rfind(".dicom");
      if ( (dicomPos != std::string::npos)
           && (dicomPos == (*it).length() - 6) )
        {
        hasExtension = true;
        }
      else
        {
        dicomPos = (*it).rfind(".DICOM");
        if ( (dicomPos != std::string::npos)
             && (dicomPos == (*it).length() - 6) )
          {
          hasExtension = true;
          }
        }

      // construct a filename, adding an extension if necessary
      std::string filename =
        m_OutputDirectory + itksys::SystemTools::GetFilenameName( *it );
      if (!hasExtension)
        {
        // input filename has no extension, add a ".dcm"
        filename += ".dcm";
        }

      // Add the file name to the output list
      m_OutputFileNames.push_back( filename );
      }
    }
  else
    {
    itkDebugMacro(<<"No files were found.");
    }

  return m_OutputFileNames;
}

void GDCMSeriesFileNames::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i;
  os << indent << "InputDirectory: " << m_InputDirectory << std::endl;
  for (i = 0; i < m_InputFileNames.size(); i++)
    {
    os << indent << "InputFilenames[" << i << "]: " << m_InputFileNames[i] << std::endl;
    }

  os << indent << "OutputDirectory: " << m_OutputDirectory << std::endl;
  for (i = 0; i < m_OutputFileNames.size(); i++)
    {
    os << indent << "OutputFilenames[" << i << "]: " << m_OutputFileNames[i] << std::endl;
    }
}
} //namespace ITK

#endif
