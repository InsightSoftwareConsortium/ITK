/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDICOMSeriesFileNames.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDICOMSeriesFileNames_h
#define _itkDICOMSeriesFileNames_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "DICOMParser.h"
#include "DICOMAppHelper.h"

#include <vector>
#include <string>

#include <itksys/SystemTools.hxx>
#include <itksys/Directory.hxx>
#include "itkDICOMSeriesFileNames.h"
#include <stdio.h>

namespace itk
{

DICOMSeriesFileNames
::DICOMSeriesFileNames() :
  m_Directory(""),
  m_FileNameSortingOrder( SortByImagePositionPatient )
{
}

const std::vector<std::string> &
DICOMSeriesFileNames
::GetFileNames()
{
  if ( m_Directory == "" )
    {
    itkExceptionMacro ( << "No directory defined!");
    }

  // Process all files in the directory
  itksys::Directory dicomDir;
  if (!dicomDir.Load (m_Directory.c_str()))
    {
    itkExceptionMacro ( << "Directory " << m_Directory.c_str() << " cannot be read!");
    }

  // clear the file names
  m_AppHelper.ClearSeriesUIDMap();
  m_AppHelper.ClearSliceOrderingMap();

  // Scan directory for files
  m_FileNames.clear();
  for (unsigned long i = 0; i < dicomDir.GetNumberOfFiles(); i++)
    {
    // Only read files
    if (itksys::SystemTools::FileIsDirectory( (m_Directory + "/" + dicomDir.GetFile(i)).c_str() ))
      {
      continue;
      }

    // store the full filename
    m_FileNames.push_back(m_Directory + "/" + dicomDir.GetFile(i));
    }

  // Scan the header of each file
  std::vector<std::string>::iterator iter;
  for (iter = m_FileNames.begin();
       iter != m_FileNames.end();
       iter++)
    {
    const char* fn = (*iter).c_str();
    m_Parser.OpenFile(fn);
    m_Parser.ClearAllDICOMTagCallbacks();
    m_AppHelper.RegisterCallbacks(&m_Parser);
    m_AppHelper.SetFileName(fn);
    m_AppHelper.SetDICOMDataFile(m_Parser.GetDICOMFile());
    m_Parser.ReadHeader();
    }

  if (this->GetDebug())
    {
    m_AppHelper.OutputSeries();
    }

  // Get the filenames, sorted by user selection
  m_FileNames.clear();
  switch (m_FileNameSortingOrder)
    {
    case SortByImageNumber:
    {
    std::vector<std::pair<int, std::string> > iSortedFileNames;
    m_AppHelper.GetSliceNumberFilenamePairs(iSortedFileNames);
    for (std::vector<std::pair<int, std::string> >::iterator it =
           iSortedFileNames.begin(); it != iSortedFileNames.end(); ++it)
      {
      m_FileNames.push_back( (*it).second );
      }
    }
    break;
    
    case SortBySliceLocation:
    {
    std::vector<std::pair<float, std::string> > fSortedFileNames;
    m_AppHelper.GetSliceLocationFilenamePairs(fSortedFileNames);
    for (std::vector<std::pair<float, std::string> >::iterator it =
           fSortedFileNames.begin(); it != fSortedFileNames.end(); ++it)
      {
      m_FileNames.push_back( (*it).second );
      }
    }
    break;
    
    case SortByImagePositionPatient:
    {
    std::vector<std::pair<float, std::string> > fSortedFileNames;
    m_AppHelper.GetImagePositionPatientFilenamePairs(fSortedFileNames);
    for (std::vector<std::pair<float, std::string> >::iterator it =
           fSortedFileNames.begin(); it != fSortedFileNames.end(); ++it)
      {
      m_FileNames.push_back( (*it).second );
      }
    }
    break;
    }
  
  return m_FileNames;
}

int
DICOMSeriesFileNames
::CanReadFile(const char* fname)
{
  bool canOpen = m_Parser.OpenFile(fname);
  if (canOpen == false)
    {
    itkWarningMacro( <<  "DICOMParser couldn't open : " << fname);
    return 0;
    }
  bool canRead = m_Parser.IsDICOMFile();
  if (canRead == true)
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
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Directory: " << m_Directory << std::endl;
  os << indent << "File name sorting order: ";
  switch (m_FileNameSortingOrder)
    {
    case SortByImageNumber: os << "SortByImageNumber" << std::endl;
      break;
    case SortBySliceLocation: os << "SortBySliceLocation" << std::endl;
      break;
    case SortByImagePositionPatient: os << "SortByImagePositionPatient"
                                        << std::endl;
      break;
    }
      
  for (unsigned int i = 0; i < m_FileNames.size(); i++)
    {
    os << indent << "FileNames[" << i << "]: " << m_FileNames[i] << std::endl;
    }
}

} //namespace ITK

#endif
