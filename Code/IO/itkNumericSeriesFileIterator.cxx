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

#include "itkNumericSeriesFileIterator.h"

namespace itk
{

NumericSeriesFileIterator::NumericSeriesFileIterator() :
  m_StartIndex(1)
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

}

const std::string& NumericSeriesFileIterator::Begin()
{
  if ( m_WriteMode )
    {
    if ( m_SeriesFormat == "" )
      {
      itkExceptionMacro(<< "No series format defined!");
      m_CurrentFileName = "";
      }
    char filename[2048], seriesFormat[1024];      
    strcpy(seriesFormat,m_SeriesFormat.c_str());
    sprintf(filename,seriesFormat,m_StartIndex);
    m_CurrentFileName = filename;
    m_CurrentIndex = m_StartIndex;
    }
  else
    {
    }

  return m_CurrentFileName;
}

const std::string& NumericSeriesFileIterator::operator++()
{
  if ( m_WriteMode )
    {
    if ( m_SeriesFormat == "" )
      {
      itkExceptionMacro(<< "No series format defined!");
      m_CurrentFileName = "";
      }
    char filename[2048], seriesFormat[1024];      
    strcpy(seriesFormat,m_SeriesFormat.c_str());
    m_CurrentIndex++;
    sprintf(filename,seriesFormat,m_CurrentIndex);
    m_CurrentFileName = filename;
    }
  else
    {
    }

  return m_CurrentFileName;
}

const std::string& NumericSeriesFileIterator::operator--()
{
  if ( m_WriteMode )
    {
    if ( m_SeriesFormat == "" )
      {
      itkExceptionMacro(<< "No series format defined!");
      m_CurrentFileName = "";
      }
    if ( --m_CurrentIndex < m_StartIndex )
      {
      m_CurrentFileName = "";
      }
    else
      {
      char filename[2048], seriesFormat[1024];      
      strcpy(seriesFormat,m_SeriesFormat.c_str());
      sprintf(filename,seriesFormat,m_CurrentIndex);
      m_CurrentFileName = filename;
      }
    }
  else
    {
    }

  return m_CurrentFileName;
}

} //namespace ITK

#endif
