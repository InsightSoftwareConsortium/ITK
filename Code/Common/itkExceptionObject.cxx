/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExceptionObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkExceptionObject.h"

namespace itk
{

void
ExceptionObject
::Print(std::ostream& os) const
{
  Indent indent;

  this->PrintHeader(os,0); 
  this->PrintSelf(os, indent.GetNextIndent());
  this->PrintTrailer(os,0);
}  
  
/**
 * Define a default print header for all objects.
 */
void 
ExceptionObject
::PrintHeader(std::ostream& os, Indent indent) const
{
  os << std::endl;
  os << indent << "itk::" << this->GetNameOfClass() << " (" << this << ")\n";
}


/**
 * Define a default print trailer for all objects.
 */
void 
ExceptionObject
::PrintTrailer(std::ostream& os, Indent indent) const
{
  os << indent << std::endl;
}

void
ExceptionObject
::PrintSelf(std::ostream& os, Indent indent) const
{
  if (! m_Location.empty()) 
    {
    os << indent << "Location: \"" << m_Location << "\" " << std::endl;
    }
  
  if (! m_File.empty()) 
    {
    os << indent << "File: " << m_File << std::endl;
    os << indent << "Line: " << m_Line << std::endl;
    }
  
  if (! m_Description.empty()) 
    {
    os << indent << "Description: " << m_Description << std::endl;  
    }
}

} // end namespace itk
