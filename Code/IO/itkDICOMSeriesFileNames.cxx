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
  m_Directory("")
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
  m_AppHelper.ClearSliceNumberMap();

  // Scan directory for files
  m_FileNames.clear();
  for (unsigned long i = 0; i < dicomDir.GetNumberOfFiles(); i++)
    {
    // Only read files
    if (itksys::SystemTools::FileIsDirectory(dicomDir.GetFile(i)))
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
    char* fn = (char*) (*iter).c_str();
    bool couldOpen = m_Parser.OpenFile(fn);

    m_Parser.ClearAllDICOMTagCallbacks();
    m_AppHelper.RegisterCallbacks(&m_Parser);
    m_AppHelper.SetFileName(fn);
    m_Parser.ReadHeader();
    }

  if (this->GetDebug())
    {
    m_AppHelper.OutputSeries();
    }

  // Get the filenames, sorted by slice number
  std::vector<std::pair<int, std::string> > sortedFileNames;
  m_AppHelper.GetSliceNumberFilenamePairs(sortedFileNames);

  // Now, store the sorted names in a vector
  if (sortedFileNames.size() > 0)
    {
    m_FileNames.clear();
    std::vector<std::pair<int, std::string> >::iterator siter;
    for (siter = sortedFileNames.begin();
         siter != sortedFileNames.end();
         siter++)
      {
      m_FileNames.push_back((*siter).second);
      }
    }
  else
    {
    itkWarningMacro( << "Couldn't get sorted files. Slices may be in wrong order!");
    }
  return m_FileNames;
}
int
DICOMSeriesFileNames
::CanReadFile(const char* fname)
{
  bool canOpen = m_Parser.OpenFile((char*) fname);
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
  for (unsigned int i = 0; i < m_FileNames.size(); i++)
    {
    os << indent << "Filenames[" << i << "]: " << m_FileNames[i] << std::endl;
    }
}

} //namespace ITK

#endif
