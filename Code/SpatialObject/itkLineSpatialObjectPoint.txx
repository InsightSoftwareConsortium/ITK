/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineSpatialObjectPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLineSpatialObjectPoint_txx
#define __itkLineSpatialObjectPoint_txx

#include "itkLineSpatialObjectPoint.h"

namespace itk 
{
 
/** Constructor */
template< unsigned int TPointDimension >
LineSpatialObjectPoint< TPointDimension >
::LineSpatialObjectPoint( void ) 
{ 
  m_ID = 0;
  m_NumDimensions = TPointDimension;
  for(unsigned int i=0;i<TPointDimension-1;i++)
  {
    VectorType normal;
    normal.Fill(0);
    m_NormalArray[i]=normal;
  }  
}

/** Destructor */
template< unsigned int TPointDimension >
LineSpatialObjectPoint< TPointDimension >
::~LineSpatialObjectPoint( void ) 
{
}

/** Get the specified normal */
template< unsigned int TPointDimension >
const typename LineSpatialObjectPoint< TPointDimension >::VectorType &
LineSpatialObjectPoint< TPointDimension >
::GetNormal(unsigned int index) const
{
  return m_NormalArray[index];
}

/** Print the object */
template< unsigned int TPointDimension >
void 
LineSpatialObjectPoint< TPointDimension >
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  Superclass::PrintSelf(os,indent);
  os << indent << "LineSpatialObjectPoint(" << this << ")" << std::endl; 
  for(unsigned int i=0;i<TPointDimension-1;i++)
  {
    os << indent <<   m_NormalArray[i] << std::endl;
  }  
}

/** Set the specified normal */
template< unsigned int TPointDimension >
void 
LineSpatialObjectPoint< TPointDimension >
::SetNormal(VectorType & normal , unsigned int index ) 
{
  m_NormalArray[index] = normal;
}

/** Copy a point to another */
template< unsigned int TPointDimension >
typename LineSpatialObjectPoint< TPointDimension >::Self & 
LineSpatialObjectPoint< TPointDimension >
::operator=(const LineSpatialObjectPoint & rhs) 
{
  m_ID = rhs.m_ID;
  m_NumDimensions = rhs.m_NumDimensions;
  m_X = rhs.m_X;
  return * this;
}

} // end namespace itk

#endif
