/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointBasedSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifndef __itkPointBasedSpatialObject_txx
#define __itkPointBasedSpatialObject_txx

#include "itkPointBasedSpatialObject.h" 

namespace itk  
{ 

/** Constructor */
template< unsigned int TDimension >
PointBasedSpatialObject< TDimension > 
::PointBasedSpatialObject() :
  SpatialObject<TDimension>()
{ 
  this->SetTypeName("PointBasedSpatialObject");
} 
 
/** Destructor */
template< unsigned int TDimension >
PointBasedSpatialObject< TDimension >  
::~PointBasedSpatialObject()
{ 
} 
 

/** Print the object */ 
template< unsigned int TDimension >
void  
PointBasedSpatialObject< TDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "PointBasedSpatialObject(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 
 
} // end namespace itk 

#endif // end __itkPointBasedSpatialObject_txx
