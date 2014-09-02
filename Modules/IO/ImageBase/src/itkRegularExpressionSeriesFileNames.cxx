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
#ifndef _itkRegularExpressionSeriesFileNames_cxx
#define _itkRegularExpressionSeriesFileNames_cxx


#include <algorithm>

#include "itksys/SystemTools.hxx"
#include "itksys/Directory.hxx"
#include "itksys/RegularExpression.hxx"
#include "itkRegularExpressionSeriesFileNames.h"

struct lt_pair_numeric_string_string {
  bool operator()(const std::pair< std::string, std::string > & s1,
                  const std::pair< std::string, std::string > & s2) const
  {
    return atof( s1.second.c_str() ) < atof( s2.second.c_str() );
  }
};

struct lt_pair_alphabetic_string_string {
  bool operator()(const std::pair< std::string, std::string > & s1,
                  const std::pair< std::string, std::string > & s2) const
  {
    return s1.second < s2.second;
  }
};

namespace itk
{
const std::vector< std::string > &
RegularExpressionSeriesFileNames
::GetFileNames()
{
  // Validate the ivars
  if ( m_Directory == "" )
    {
    itkExceptionMacro (<< "No directory defined!");
    }

  itksys::RegularExpression reg;
  if ( !reg.compile( m_RegularExpression.c_str() ) )
    {
    itkExceptionMacro(<< "Error compiling regular expression " << m_RegularExpression);
    }

  // Process all files in the directory
  itksys::Directory fileDir;
  if ( !fileDir.Load ( m_Directory.c_str() ) )
    {
    itkExceptionMacro (<< "Directory " << m_Directory.c_str() << " cannot be read!");
    }

  std::vector< std::pair< std::string, std::string > > sortedBySubMatch;

  // Scan directory for files. Each file is checked to see if it
  // matches the m_RegularExpression.
  for ( unsigned long i = 0; i < fileDir.GetNumberOfFiles(); i++ )
    {
    // Only read files
    if ( itksys::SystemTools::FileIsDirectory( ( m_Directory + "/" + fileDir.GetFile(i) ).c_str() ) )
      {
      continue;
      }

    if ( reg.find( fileDir.GetFile(i) ) )
      {
      // Store the full filename and the selected sub expression match
      std::pair< std::string, std::string > fileNameMatch;
      fileNameMatch.first = m_Directory + "/" + fileDir.GetFile(i);
      fileNameMatch.second = reg.match(m_SubMatch);
      sortedBySubMatch.push_back(fileNameMatch);
      }
    }

  // Sort the files. The files are sorted by the sub match defined by
  // m_SubMatch. Sorting can be alpahbetic or numeric.
  if ( m_NumericSort )
    {
    std::sort( sortedBySubMatch.begin(),
               sortedBySubMatch.end(),
               lt_pair_numeric_string_string() );
    }
  else
    {
    std::sort( sortedBySubMatch.begin(),
               sortedBySubMatch.end(),
               lt_pair_alphabetic_string_string() );
    }

  // Now, store the sorted names in a vector
  m_FileNames.clear();
  std::vector< std::pair< std::string, std::string > >::iterator siter;
  for ( siter = sortedBySubMatch.begin();
        siter != sortedBySubMatch.end();
        ++siter )
    {
    m_FileNames.push_back( ( *siter ).first );
    }

  return m_FileNames;
}

void
RegularExpressionSeriesFileNames
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Directory: " << m_Directory << std::endl;
  os << indent << "SubMatch: " << m_SubMatch << std::endl;
  os << indent << "NumericSort: " << m_NumericSort << std::endl;
  os << indent << "RegularExpression: " << m_RegularExpression << std::endl;

  for ( unsigned int i = 0; i < m_FileNames.size(); i++ )
    {
    os << indent << "FileNames[" << i << "]: " << m_FileNames[i] << std::endl;
    }
}
} //namespace ITK

#endif
