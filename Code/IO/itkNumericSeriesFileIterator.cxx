/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericSeriesFileIterator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNumericSeriesFileIterator_h
#define _itkNumericSeriesFileIterator_h

#include <itkkwsys/SystemTools.hxx>
#include "itkNumericSeriesFileIterator.h"
//#include "itkIOCommon.h"
#include <stdio.h>

namespace itk
{

NumericSeriesFileIterator::NumericSeriesFileIterator() :
  m_StartIndex(1),m_EndIndex(10000),m_NumberOfFiles(10000),
  m_ThrowExceptionOnMissingFile(true)
{
}

NumericSeriesFileIterator::~NumericSeriesFileIterator()
{
}

void NumericSeriesFileIterator
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Series Format: " << m_SeriesFormat << "\n";
  os << indent << "Start Index: " << m_StartIndex << "\n";
  os << indent << "End Index: " << m_EndIndex << "\n";
  os << indent << "Number of files: " << m_NumberOfFiles << "\n";
  os << indent << "ThrowExceptionOnMissingFile: " << m_ThrowExceptionOnMissingFile << "\n";

}

const std::string& NumericSeriesFileIterator
::ProduceNextFileName(unsigned long idx)
{
  char filename[4096], seriesFormat[2048];      
  strcpy(seriesFormat, m_SeriesFormat.c_str());
  sprintf(filename, seriesFormat, idx);
  m_CurrentFileName = filename;

  return m_CurrentFileName;
}

const std::string& NumericSeriesFileIterator::Begin()
{
  if ( m_SeriesFormat == "" )
  {
    throw NumericSeriesException(__FILE__, __LINE__,
        "No series format defined!");
  }

  if ( m_WriteMode )
  {
    m_CurrentIndex = m_StartIndex;
    return this->ProduceNextFileName(m_StartIndex);
  }
  else //in read mode
  {
    // Find the first filename in the series
    unsigned long idx;
    for (idx=m_StartIndex; idx < m_EndIndex; idx++ )
    {
      this->ProduceNextFileName( idx );
      if ( ! itkkwsys::SystemTools::FileExists(m_CurrentFileName.c_str()) )
      {
        if ( m_ThrowExceptionOnMissingFile )
        {
          throw NumericSeriesException(__FILE__, __LINE__);
        }
      }
      else
      {
        break;
      }
    }

    if ( idx >= m_EndIndex )
    {
      throw NumericSeriesException(__FILE__, __LINE__, "No files in series");
    }

    m_CurrentIndex = idx;
    m_CurrentNumberOfFiles = 1;
    return m_CurrentFileName;
  }
}

const std::string& NumericSeriesFileIterator::operator++()
{
  if ( m_SeriesFormat == "" )
    {
    throw NumericSeriesException(__FILE__, __LINE__,
                                 "No series format defined!");
    }

  if ( m_WriteMode )
    {
    return this->ProduceNextFileName( ++m_CurrentIndex );
    }

  else //in read mode
    {
    // Find next filename in the series
    unsigned long idx;
    for ( idx=m_CurrentIndex; 
          idx < m_EndIndex && m_CurrentNumberOfFiles < m_NumberOfFiles; 
          idx++ )
      {
      this->ProduceNextFileName( idx );
      if ( ! itkkwsys::SystemTools::FileExists(m_CurrentFileName.c_str()) )
        {
        if ( m_ThrowExceptionOnMissingFile )
          {
          throw NumericSeriesException(__FILE__, __LINE__);
          }
        }
      else
        {
        break;
        }
      }

    if ( idx >= m_EndIndex || m_CurrentNumberOfFiles > m_NumberOfFiles )
      {
      m_CurrentFileName = "";
      }
    else
      {
      m_CurrentIndex = idx;
      m_CurrentNumberOfFiles++;
      }
    
    return m_CurrentFileName;
    }
}

const std::string& NumericSeriesFileIterator::operator--()
{
  if ( m_SeriesFormat == "" )
    {
    throw NumericSeriesException(__FILE__, __LINE__, 
                                 "No series format defined!");
    }

  if ( m_WriteMode )
    {
    if ( --m_CurrentIndex < m_StartIndex )
      {
      m_CurrentIndex = m_StartIndex;
      m_CurrentFileName = "";
      return m_CurrentFileName;
      }
    else
      {
      return this->ProduceNextFileName(m_CurrentIndex);
      }
    }

  else  //in read mode
    {
    // Find previous filename in the series
    unsigned long idx;
    for ( idx=m_CurrentIndex; 
          idx >= m_StartIndex; idx-- )
      {
      this->ProduceNextFileName( idx );
      if ( ! itkkwsys::SystemTools::FileExists(m_CurrentFileName.c_str()) )
        {
        if ( m_ThrowExceptionOnMissingFile )
          {
          throw NumericSeriesException(__FILE__, __LINE__);
          }
        }
      else
        {
        break;
        }
      }

    if ( idx < m_EndIndex )
      {
      m_CurrentIndex = m_StartIndex;
      m_CurrentFileName = "";
      }
    else
      {
      m_CurrentIndex = idx;
      m_CurrentNumberOfFiles--;
      }
    
    return m_CurrentFileName;
    }
}

} //namespace ITK

#endif
