/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileIteratorBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFileIteratorBase_h
#define _itkFileIteratorBase_h

#include "itkFileIteratorBase.h"

namespace itk
{

FileIteratorBase::FileIteratorBase() :
  m_CurrentFileName("")
{
}

FileIteratorBase::~FileIteratorBase()
{
}

void FileIteratorBase::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Write Mode: ";
  if ( m_WriteMode ) os << "On\n";
  else os << "Off\n";
    
  os << indent << "Series Format: " << m_SeriesFormat << "\n";
  os << indent << "Current File Name: " << m_CurrentFileName << "\n";
}

} //namespace ITK

#endif
