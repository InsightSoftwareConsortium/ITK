/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGroupSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGroupSpatialObject_txx
#define __itkGroupSpatialObject_txx

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkGroupSpatialObject.h" 

namespace itk  
{ 

/** Constructor */
template< unsigned int TDimension >
GroupSpatialObject< TDimension > 
::GroupSpatialObject()  
{ 
  this->SetDimension(TDimension);
  this->SetTypeName("GroupSpatialObject");
  this->GetProperty()->SetRed(1); 
  this->GetProperty()->SetGreen(0); 
  this->GetProperty()->SetBlue(0); 
  this->GetProperty()->SetAlpha(1); 
  this->ComputeBoundingBox();
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
