/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularExpressionSeriesFileNames.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegularExpressionSeriesFileNames_cxx
#define _itkRegularExpressionSeriesFileNames_cxx

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <vector>
#include <string>

#include <itksys/SystemTools.hxx>
#include <itksys/Directory.hxx>
#include <itksys/RegularExpression.hxx>
#include "itkRegularExpressionSeriesFileNames.h"


struct lt_pair_numeric_string_string
{
  bool operator()(const std::pair<std::string, std::string> s1, 
                  const std::pair<std::string, std::string> s2) const
  {
    return atof(s1.second.c_str()) < atof(s2.second.c_str());
  }
};

struct lt_pair_alphabetic_string_string
{
  bool operator()(const std::pair<std::string, std::string> s1, 
                  const std::pair<std::string, std::string> s2) const
  {
    return s1.second < s2.second;
  }
};

namespace itk
{
const std::vector<std::string> &
RegularExpressionSeriesFileNames
::GetFileNames()
{
  // Validate the ivars
  if ( m_Directory == "" )
    {
    itkExceptionMacro ( << "No directory defined!");
    }

  itksys::RegularExpression reg;
  if (!reg.compile(m_RegularExpression.c_str()))
    {
    itkExceptionMacro(<< "Error compiling regular expression " << m_RegularExpression);
    }

  // Process all files in the directory
  itksys::Directory fileDir;
  if (!fileDir.Load (m_Directory.c_str()))
    {
    itkExceptionMacro ( << "Directory " << m_Directory.c_str() << " cannot be read!");
    }

  std::vector<std::pair<std::string,std::string> > sortedBySubMatch;

  // Scan directory for files. Each file is checked to see if it
  // matches the m_RegularExpression.
  for (unsigned long i = 0; i < fileDir.GetNumberOfFiles(); i++)
    {
    // Only read files
    if (itksys::SystemTools::FileIsDirectory(fileDir.GetFile(i)))
      {
      continue;
      }

    if (reg.find(fileDir.GetFile(i)))
      {
      // Store the full filename and the selected sub expression match
      std::pair<std::string,std::string> fileNameMatch;
      fileNameMatch.first = m_Directory + "/" + fileDir.GetFile(i);
      fileNameMatch.second = reg.match(m_SubMatch);
      sortedBySubMatch.push_back(fileNameMatch);
      }
    }
  
  // Sort the files. The files are sorted by the sub match defined by
  // m_SubMatch. Sorting can be alpahbetic or numeric.
  if (m_NumericSort)
    {
    std::sort(sortedBySubMatch.begin(),
              sortedBySubMatch.end(),
              lt_pair_numeric_string_string());
    }
  else
    {
    std::sort(sortedBySubMatch.begin(),
              sortedBySubMatch.end(),
              lt_pair_alphabetic_string_string());
    }

  // Now, store the sorted names in a vector
  m_FileNames.clear();
  std::vector<std::pair<std::string, std::string> >::iterator siter;
  for (siter = sortedBySubMatch.begin();
       siter != sortedBySubMatch.end();
       siter++)
    {
    m_FileNames.push_back((*siter).first);
    }

  return m_FileNames;
}

void
RegularExpressionSeriesFileNames
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Directory: " << m_Directory << std::endl;
  os << indent << "SubMatch: " << m_SubMatch << std::endl;
  os << indent << "NumericSort: " << m_NumericSort << std::endl;
  os << indent << "RegularExpression: " << m_RegularExpression << std::endl;

  for (unsigned int i = 0; i < m_FileNames.size(); i++)
    {
    os << indent << "Filenames[" << i << "]: " << m_FileNames[i] << std::endl;
    }
}
} //namespace ITK

#endif
