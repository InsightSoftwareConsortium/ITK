/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkExceptionObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkExceptionObject.h"

namespace itk
{
  
void ExceptionObject::Print(std::ostream& os) const
{
  os << "itk: Exception detected ";
  if (! m_Location.empty()) 
    {
    os << "in \"" << m_Location << "\" ";
    }
  
  os << "of type \"" << this->GetClassName() << "\" ";
  if (! m_Description.empty()) 
    {
    os << std::endl << "\t" << m_Description;  
    }
}

} // namespace itk
