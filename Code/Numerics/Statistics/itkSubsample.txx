/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSubsample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSubsample_txx
#define __itkSubsample_txx

#include "itkObject.h"

namespace itk{ 
  namespace Statistics{

template< class TSample >
Subsample< TSample >
::Subsample()
{
}

template< class TSample >
void
Subsample< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Sample: " << m_Sample << std::endl ;
  os << indent << "InstanceIdentifierHolder : " << &m_IdHolder << std::endl ;
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif







