/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArchetypeSeriesFileNames.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkArchetypeSeriesFileNames_h
#define _itkArchetypeSeriesFileNames_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


#include "itkArchetypeSeriesFileNames.h"
#include "itkRegularExpressionSeriesFileNames.h"
#include <itksys/SystemTools.hxx>
#include <stdio.h>
#include <algorithm>

namespace itk
{

ArchetypeSeriesFileNames
::ArchetypeSeriesFileNames() :
  m_Archetype("")
{
}

void
ArchetypeSeriesFileNames
::SetArchetype( const std::string &archetype )
{
  if (archetype != m_Archetype)
    {
    m_Archetype = archetype;
    this->Modified();
    m_ArchetypeMTime.Modified();
    }
}

unsigned int
ArchetypeSeriesFileNames
::GetNumberOfGroupings()
{
  if (m_ScanTime < m_ArchetypeMTime)
    {
    this->Scan();
    }

  return m_Groupings.size();
}


const std::vector<std::string> &
ArchetypeSeriesFileNames
::GetFileNames(unsigned int group)
{
  if (m_ScanTime < m_ArchetypeMTime)
    {
    this->Scan();
    }

  if (group < m_Groupings.size())
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

  if (itksys::SystemTools::FileIsDirectory( unixArchetype.c_str() ))
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
  for( unsigned int j = 0; j < origFileName.length(); j++ )
    {
    char oneChar = origFileName[j];
    if( oneChar == '^' ||
        oneChar == '$' ||
        oneChar == '.' ||
        oneChar == '[' ||
        oneChar == ']' ||
        oneChar == '-' ||
        oneChar == '*' ||
        oneChar == '+' ||
        oneChar == '?' ||
        oneChar == '(' ||
        oneChar == ')' )
      {
      fileName += "\\";
      }
    fileName += oneChar;
    }
    
  // If there is no "/" in the name, the directory is not specified.
  // In that case, use the default ".".  This is necessary for the RegularExpressionSeriesFileNames.
  if (fileNamePath == "")
    {
    fileNamePath = ".";
    pathPrefix = "./";
    }
  else
    {
    pathPrefix = "";
    }

  std::vector < std::string > regExpFileNameVector;
  std::string regExpString = "([0-9]+)";
  std::vector < int > numGroupStart;
  std::vector < int > numGroupLength;
  int sIndex;
  std::string::iterator sit;
  for (sit = fileName.begin(); sit < fileName.end(); sit++)
    {
    // If the element is a number, find its starting index and length.
    if ((*sit) >= '0' && (*sit) <= '9')
      {
      sIndex = sit - fileName.begin();
      numGroupStart.push_back( sIndex );
      
      // Loop to one past the end of the group of numbers.
      while ((*sit) >= '0' && (*sit) <= '9' && sit != fileName.end() )
        {
        ++sit;
        }
      
      numGroupLength.push_back( (sit - fileName.begin()) - sIndex );
      }
    }




  // Create a set of regular expressions, one for each group of
  // numbers in m_FileName. We walk the regular expression groups
  // from right to left since numbers at the end of filenames are more
  // likely to be image numbers.
  // It is also necessary to walk backward so that the numGroupStart
  // indices remain correct since the length of numbers we are replacing may
  // be different from the length of regExpString.
  int i;
  for (i = (int)numGroupLength.size()-1 ; i >= 0; i--)
    {
    std::string regExpFileName = fileName;
    
    regExpFileName.replace(numGroupStart[i],numGroupLength[i],regExpString);
    // Include only filenames that exactly match this regular expression.  Don't
    // match filenames that have this string as a substring (ie. that have extra
    // prefixes or suffixes).
    regExpFileName = "^" + regExpFileName + "$";
    regExpFileNameVector.push_back( regExpFileName );
    }

  // Use a RegularExpressionSeriesFileNames to find the files to return
  std::vector<std::string> names;

  for (i = 0; i < (int) regExpFileNameVector.size(); i++)
    {
    itk::RegularExpressionSeriesFileNames::Pointer fit = itk::RegularExpressionSeriesFileNames::New();
    fit->SetDirectory( fileNamePath.c_str() );
    fit->SetRegularExpression( regExpFileNameVector[i].c_str() );
    fit->SetSubMatch(1);
    fit->NumericSortOn();
    names = fit->GetFileNames();

    std::vector<std::string>::iterator ait;
    ait = std::find(names.begin(), names.end(), pathPrefix + unixArchetype);

    // Accept the list if it contains the archetype and is not the
    // "trivial" list (containing only the archetype)
    if ( ait != names.end() && names.size() > 1)
      {
      m_Groupings.push_back(names);
      }
    }

  // If the group list is empty, create a single group containing the
  // archetype.
  if ( m_Groupings.size() == 0 && itksys::SystemTools::FileExists(unixArchetype.c_str()))
    {
    std::vector<std::string> tlist;

    tlist.push_back( unixArchetype );
    m_Groupings.push_back( tlist );
    }

  m_ScanTime.Modified();
}

void
ArchetypeSeriesFileNames
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Archetype: " << m_Archetype << std::endl;
  os << indent << "Number of groupings: " 
     << const_cast<ArchetypeSeriesFileNames*>(this)->GetNumberOfGroupings() << std::endl;

  for (unsigned int j = 0; j < const_cast<ArchetypeSeriesFileNames*>(this)->GetNumberOfGroupings(); j++)
    {
    os << indent << "Grouping #" << j << std::endl;
    std::vector<std::string> group = const_cast<ArchetypeSeriesFileNames*>(this)->GetFileNames(j);
    for (unsigned int i = 0; i < group.size(); i++)
      {
      os << indent << indent << "Filenames[" << i << "]: " << group[i] << std::endl;
      }
    }
}
} //namespace ITK

#endif
