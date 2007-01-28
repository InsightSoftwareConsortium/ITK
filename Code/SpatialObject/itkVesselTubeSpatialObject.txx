/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVesselTubeSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVesselTubeSpatialObject_txx
#define __itkVesselTubeSpatialObject_txx

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkVesselTubeSpatialObject.h" 

namespace itk  
{ 

/** Constructor */
template< unsigned int TDimension >
VesselTubeSpatialObject< TDimension > 
::VesselTubeSpatialObject() : Superclass()
{ 
  this->m_ParentPoint = -1;
  this->SetDimension(TDimension);
  this->SetTypeName("VesselTubeSpatialObject");
} 
 
/** Destructor */
template< unsigned int TDimension >
VesselTubeSpatialObject< TDimension >  
::~VesselTubeSpatialObject()
{ 
} 

/** Print the object */ 
template< unsigned int TDimension >
void  
VesselTubeSpatialObject< TDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "VesselTubeSpatialObject(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent ); 
}

} // end namespace itk 

#endif // end __itkVesselTubeSpatialObject_txx
