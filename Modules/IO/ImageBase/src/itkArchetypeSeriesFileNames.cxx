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
#ifndef _itkArchetypeSeriesFileNames_h
#define _itkArchetypeSeriesFileNames_h


#include "itkArchetypeSeriesFileNames.h"
#include "itkRegularExpressionSeriesFileNames.h"
#include "itksys/SystemTools.hxx"
#include <algorithm>

namespace itk
{
ArchetypeSeriesFileNames
::ArchetypeSeriesFileNames():m_Archetype("")
{}

void
ArchetypeSeriesFileNames
::SetArchetype(const std::string & archetype)
{
  if ( archetype != m_Archetype )
    {
    m_Archetype = archetype;
    this->Modified();
    m_ArchetypeMTime.Modified();
    }
}

ArchetypeSeriesFileNames::VectorSizeType
ArchetypeSeriesFileNames
::GetNumberOfGroupings()
{
  if ( m_ScanTime < m_ArchetypeMTime )
    {
    this->Scan();
    }

  return m_Groupings.size();
}

const std::vector< std::string > &
ArchetypeSeriesFileNames
::GetFileNames(VectorSizeType group)
{
  if ( m_ScanTime < m_ArchetypeMTime )
    {
    this->Scan();
    }

  if ( group < m_Groupings.size() )
    {
    m_FileNames = m_Groupings[group];
    }
  else
    {
    m_FileNames.clear();
    }

  return m_FileNames;
}

void
ArchetypeSeriesFileNames
::Scan()
{
  // For each group of contiguous numbers in m_Archetype, create a
  // regular expression that is identical to m_Archetype except that
  // it replaces that group of numbers with the pattern ([0-9]+).
  // Each of these new strings will then be passed into a
  // RegularExpressionSeriesFileNames to generate the list of those
  // files that fit that pattern.

  m_Groupings.clear();

  std::string unixArchetype = m_Archetype;
  itksys::SystemTools::ConvertToUnixSlashes(unixArchetype);

  if ( itksys::SystemTools::FileIsDirectory( unixArchetype.c_str() ) )
    {
    return;
    }

  // Parse the fileNameName and fileNamePath
  std::string origFileName = itksys::SystemTools::GetFilenameName( unixArchetype.c_str() );
  std::string fileNamePath = itksys::SystemTools::GetFilenamePath( unixArchetype.c_str() );
  std::string pathPrefix;

  // "Clean" the filename by escaping any special characters with backslashes.
  // This allows us to pass in filenames that include these special characters.
  std::string fileName;
  for ( unsigned int j = 0; j < origFileName.length(); j++ )
    {
    char oneChar = origFileName[j];
    if ( oneChar == '^'
         || oneChar == '$'
         || oneChar == '.'
         || oneChar == '['
         || oneChar == ']'
         || oneChar == '-'
         || oneChar == '*'
         || oneChar == '+'
         || oneChar == '?'
         || oneChar == '('
         || oneChar == ')' )
      {
      fileName += "\\";
      }
    fileName += oneChar;
    }

  // If there is no "/" in the name, the directory is not specified.
  // In that case, use the default ".".  This is necessary for the
  // RegularExpressionSeriesFileNames.
  if ( fileNamePath == "" )
    {
    fileNamePath = ".";
    pathPrefix = "./";
    }
  else
    {
    pathPrefix = "";
    }

  StringVectorType      regExpFileNameVector;
  std::string           regExpString = "([0-9]+)";
  IntVectorType         numGroupStart;
  IntVectorType         numGroupLength;
  for ( std::string::iterator sit = fileName.begin(); sit < fileName.end(); ++sit )
    {
    // If the element is a number, find its starting index and length.
    if ( ( *sit ) >= '0' && ( *sit ) <= '9' )
      {
      int sIndex = static_cast< int >( sit - fileName.begin() );
      numGroupStart.push_back(sIndex);

      // Loop to one past the end of the group of numbers.
      while ( sit != fileName.end() && ( *sit ) >= '0' && ( *sit ) <= '9' )
        {
        ++sit;
        }

      numGroupLength.push_back(static_cast< int >( sit - fileName.begin() ) - sIndex);

      if ( sit == fileName.end() )
        {
        break;
        }
      }
    }

  // Create a set of regular expressions, one for each group of
  // numbers in m_FileName. We walk the regular expression groups
  // from right to left since numbers at the end of filenames are more
  // likely to be image numbers.
  // It is also necessary to walk backward so that the numGroupStart
  // indices remain correct since the length of numbers we are replacing may
  // be different from the length of regExpString.
  IntVectorType::reverse_iterator numGroupLengthItr = numGroupLength.rbegin();
  IntVectorType::reverse_iterator numGroupStartItr  = numGroupStart.rbegin();
  while ( numGroupLengthItr != numGroupLength.rend()
          && numGroupStartItr != numGroupStart.rend() )
    {
    std::string regExpFileName = fileName;

    regExpFileName.replace(*numGroupStartItr, *numGroupLengthItr, regExpString);
    // Include only filenames that exactly match this regular expression.  Don't
    // match filenames that have this string as a substring (ie. that have extra
    // prefixes or suffixes).
    regExpFileName = "^" + regExpFileName + "$";
    regExpFileNameVector.push_back(regExpFileName);
    ++numGroupLengthItr;
    ++numGroupStartItr;
    }

  // Use a RegularExpressionSeriesFileNames to find the files to return
  StringVectorType names;

  StringVectorType::const_iterator regExpFileNameVectorItr =
    regExpFileNameVector.begin();
  while ( regExpFileNameVectorItr != regExpFileNameVector.end() )
    {
    itk::RegularExpressionSeriesFileNames::Pointer fit = itk::RegularExpressionSeriesFileNames::New();
    fit->SetDirectory( fileNamePath.c_str() );
    fit->SetRegularExpression( regExpFileNameVectorItr->c_str() );
    fit->SetSubMatch(1);
    fit->NumericSortOn();
    names = fit->GetFileNames();

    std::vector< std::string >::iterator ait;
    ait = std::find(names.begin(), names.end(), pathPrefix + unixArchetype);

    // Accept the list if it contains the archetype and is not the
    // "trivial" list (containing only the archetype)
    if ( ait != names.end() && names.size() > 1 )
      {
      m_Groupings.push_back(names);
      }
    ++regExpFileNameVectorItr;
    }

  // If the group list is empty, create a single group containing the
  // archetype.
  if ( m_Groupings.size() == 0 && itksys::SystemTools::FileExists( unixArchetype.c_str() ) )
    {
    std::vector< std::string > tlist;

    tlist.push_back(unixArchetype);
    m_Groupings.push_back(tlist);
    }

  m_ScanTime.Modified();
}

void
ArchetypeSeriesFileNames
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Archetype: " << m_Archetype << std::endl;
  os << indent << "Number of groupings: "
     << const_cast< ArchetypeSeriesFileNames * >( this )->GetNumberOfGroupings() << std::endl;

  for ( unsigned int j = 0; j < const_cast< ArchetypeSeriesFileNames * >( this )->GetNumberOfGroupings(); j++ )
    {
    os << indent << "Grouping #" << j << std::endl;
    StringVectorType                 group = const_cast< ArchetypeSeriesFileNames * >( this )->GetFileNames(j);
    StringVectorType::const_iterator groupItr = group.begin();
    unsigned int                     i = 0;
    while ( groupItr != group.end() )
      {
      os << indent << indent << "FileNames[" << i << "]: " << *groupItr << std::endl;
      ++i;
      ++groupItr;
      }
    }
}
} //namespace ITK

#endif
