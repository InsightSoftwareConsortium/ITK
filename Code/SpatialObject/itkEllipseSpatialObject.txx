/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipseSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
 
#include "itkEllipseSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int NDimensions , unsigned int PipelineDimension >
EllipseSpatialObject<NDimensions, PipelineDimension >
::EllipseSpatialObject()
{
  strcpy(m_TypeName,"EllipseSpatialObject");
  m_Radius.Fill(1.0);
  m_Dimension = NDimensions;
} 

/** Destructor */
template< unsigned int NDimensions , unsigned int PipelineDimension >
EllipseSpatialObject<NDimensions, PipelineDimension >
::~EllipseSpatialObject()  
{
  
}

/** Set all radii to the same radius value */
template< unsigned int NDimensions , unsigned int PipelineDimension >
void
EllipseSpatialObject<NDimensions, PipelineDimension >
::SetRadius(double radius)
{
  for(unsigned int i=0;i<NumberOfDimension;i++)
  {
    m_Radius[i]=radius;
  }
}

template< unsigned int NDimensions , unsigned int PipelineDimension >
void 
EllipseSpatialObject< NDimensions >
::PrintSelf( std::ostream& os, Indent indent ) const
{

  Superclass::PrintSelf(os, indent);
  os << "Radius: " << m_Radius << std::endl;

}



} // end namespace itk
