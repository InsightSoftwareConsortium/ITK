/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGroupSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifndef __itkGroupSpatialObject_txx
#define __itkGroupSpatialObject_txx

#include "itkGroupSpatialObject.h" 

namespace itk  
{ 

/** Constructor */
template< unsigned int TDimension >
GroupSpatialObject< TDimension > 
::GroupSpatialObject()  
{ 
  m_Dimension = TDimension;
  strcpy(m_TypeName,"GroupSpatialObject");
  m_Property->SetRed(1); 
  m_Property->SetGreen(0); 
  m_Property->SetBlue(0); 
  m_Property->SetAlpha(1); 
  ComputeBoundingBox();
} 
 
/** Destructor */
template< unsigned int TDimension >
GroupSpatialObject< TDimension >  
::~GroupSpatialObject()
{ 
} 
 

/** Print the object */ 
template< unsigned int TDimension >
void  
GroupSpatialObject< TDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "GroupSpatialObject(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 
 
} // end namespace itk 

#endif // end __itkGroupSpatialObject_txx
