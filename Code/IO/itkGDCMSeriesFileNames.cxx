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
#include <gdcm.h>
#include <itksys/SystemTools.hxx>

#include <vector>
#include <string>

namespace itk
{

const std::vector<std::string> &GDCMSeriesFileNames::GetInputFileNames() 
{
  // Get the DICOM filenames from the directory
  gdcmSerieHeaderHelper *helper = new gdcmSerieHeaderHelper();
  helper->SetDirectory( m_InputDirectory.c_str() );
  helper->OrderGdcmFileList();
  //We assume that there is only one study / one serie

  std::list<gdcmHeaderHelper*> flist = helper->GetGdcmFileList();
  if( flist.size() )
    {
    for(std::list<gdcmHeaderHelper*>::iterator it = flist.begin(); 
        it != flist.end(); ++it )
      {
      std::string foo = (*it)->GetFileName();
      m_InputFileNames.push_back( foo );
      }
    }
  else
    {
    itkDebugMacro(<<"No files were found");
    }
  //delete helper;

  return m_InputFileNames;
}

const std::vector<std::string> &GDCMSeriesFileNames::GetOutputFileNames() 
{
  // We are trying to extract the original filename and compose it with a path:

  //There is two different approach if directory does not exist:
  // 1. Exit
  // 2. Mkdir
  //bool SystemTools::FileExists(const char* filename)
  //bool SystemTools::FileIsDirectory(const char* name)
  
  if( m_OutputDirectory.empty() )
    {
    itkDebugMacro(<<"No output directory was specified");
    m_OutputFileNames.clear();
    return m_OutputFileNames;
    }

  itksys::SystemTools::ConvertToUnixSlashes( m_OutputDirectory );
  if(m_OutputDirectory[m_OutputDirectory.size()-1] != '/')
    {
    m_OutputDirectory += '/';
    }

  if( m_InputFileNames.size() )
    {
    for(std::vector<std::string>::const_iterator it = m_InputFileNames.begin();
        it != m_InputFileNames.end(); ++it )
        {
        std::string filename = 
          m_OutputDirectory + itksys::SystemTools::GetFilenameName( *it );
        
        //std::cerr << "filename:" << filename << std::endl;
        m_OutputFileNames.push_back( filename );
        }
      }
    else
      {
      itkDebugMacro(<<"No files were found.");
      m_OutputFileNames.clear();
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
