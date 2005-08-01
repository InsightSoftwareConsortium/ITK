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
  m_Directory = "";
}

GDCMSeriesFileNames::~GDCMSeriesFileNames()
{
  delete m_SerieHelper;
}

const SerieUIDContainer &GDCMSeriesFileNames::GetSeriesUIDs() 
{
  if ( m_Directory == "" )
    {
    itkWarningMacro( << "You need to specify a directory where "
      "the DICOM files are located");
    }
  m_SeriesUIDs.clear();
  // Accessing the first serie found (assume there is at least one)
  gdcm::GdcmFileList *flist = m_SerieHelper->GetFirstCoherentFileList();
  while(flist)
    {
    if( flist->size() ) //make sure we have at leat one serie
      {
      gdcm::File *file = (*flist)[0]; //for example take the first one
      std::string uid = file->GetEntryValue (0x0020, 0x000e); 
      m_SeriesUIDs.push_back( uid );
      }
    flist = m_SerieHelper->GetNextCoherentFileList();
    }
  if( !m_SeriesUIDs.size() )
    {
    itkWarningMacro(<<"No Series were found");
    }
  return m_SeriesUIDs;
}

const FilenamesContainer &GDCMSeriesFileNames::GetFileNames(const std::string serie) 
{
  m_FileNames.clear();
  // Accessing the first serie found (assume there is at least one)
  gdcm::GdcmFileList *flist = m_SerieHelper->GetFirstCoherentFileList();
  bool found = false;
  while(flist)
    {
    if( flist->size() ) //make sure we have at leat one serie
      {
      gdcm::File *file = (*flist)[0]; //for example take the first one
      std::string uid = file->GetEntryValue (0x0020, 0x000e); 
      // We need to use a specialized call to compare those two strings:
      if( gdcm::Util::DicomStringEqual(uid, serie.c_str()) )
        {
        found = true; // we found a match
        break;
        }
      }
    flist = m_SerieHelper->GetNextCoherentFileList();
    }
  if( !found)
    {
    itkWarningMacro(<<"No Series were found");
    return m_FileNames;
    }
  m_SerieHelper->OrderGdcmFileList(flist);

  gdcm::GdcmFileList::iterator it;
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
        m_FileNames.push_back( header->GetFileName() );
      }
    }
  else
    {
    itkDebugMacro(<<"No files were found");
    }

  return m_FileNames;
}

const FilenamesContainer &GDCMSeriesFileNames::GetInputFileNames() 
{
  m_InputFileNames.clear();
  // Get the DICOM filenames from the directory
  gdcm::SerieHelper *helper = new gdcm::SerieHelper();
  helper->SetDirectory( m_InputDirectory );
  // Accessing the first serie found (assume there is at least one)
  gdcm::GdcmFileList *flist = helper->GetFirstCoherentFileList();
  helper->OrderGdcmFileList(flist);

  gdcm::GdcmFileList::iterator it;
  if( flist && flist->size() )
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
  delete helper;

  return m_InputFileNames;
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
      std::string filename;
      if (hasExtension)
        {
        filename = 
          m_OutputDirectory + itksys::SystemTools::GetFilenameName( *it );
        }
      else
        {
        // input filename has no extension, add a ".dcm"
        filename = 
          m_OutputDirectory + itksys::SystemTools::GetFilenameName( *it )
          + ".dcm";
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
