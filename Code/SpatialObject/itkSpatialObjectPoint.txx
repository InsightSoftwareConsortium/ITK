/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectPoint_txx
#define __itkSpatialObjectPoint_txx

#include "itkSpatialObjectPoint.h"

namespace itk 
{

/** Common construction */
template< unsigned int TPointDimension >
void 
SpatialObjectPoint< TPointDimension >
::CommonConstruction() 
{
  m_ID = 0;
}

/** Constructor */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >
::SpatialObjectPoint( void ) 
{ 
  CommonConstruction();
  m_NumDimensions = TPointDimension;
}

/** Destructor */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >
::~SpatialObjectPoint( void ) 
{
}

/** Get a reference to the point */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >::Self & 
SpatialObjectPoint< TPointDimension >
::GetReference( void )
{
  return *this;
}

/** Get a pointer to the point */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >::Pointer 
SpatialObjectPoint< TPointDimension >
::GetPointer( void )
{
  return this;
}
    
/** Return a const pointer to the point */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >::ConstPointer 
SpatialObjectPoint< TPointDimension >
::GetConstPointer( void )
{
  return this;
}

/** Set the Identification number of a point */
template< unsigned int TPointDimension >
void 
SpatialObjectPoint< TPointDimension >
::SetId( const unsigned int newID ) 
{
  m_ID = newID;
}

/** Get the Identification number of a point */
template< unsigned int TPointDimension >
unsigned int 
SpatialObjectPoint< TPointDimension >
::GetId( void ) 
{
  return m_ID;
}

/** Return the number of dimension of a point */
template< unsigned int TPointDimension >
unsigned short int 
SpatialObjectPoint< TPointDimension >
::GetNumDimensions( void ) const
{
  return m_NumDimensions;
}

/** Return the position of a point */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >::PointType 
SpatialObjectPoint< TPointDimension >
::GetPosition( void ) const
{
  return m_X;
}

/** Set the position : n-D case */
template< unsigned int TPointDimension >
void 
SpatialObjectPoint< TPointDimension >
::SetPosition( const PointType & newX ) 
{
  m_X = newX;
}

/** Set the position : 3D case */
template< unsigned int TPointDimension >
void 
SpatialObjectPoint< TPointDimension >
::SetPosition( const double x0, const double x1, const double x2 ) 
{
  m_X[0] = x0;
  m_X[1] = x1;
  m_X[2] = x2;
}

/** Set the position : 2D case */
template< unsigned int TPointDimension >
void 
SpatialObjectPoint< TPointDimension >
::SetPosition( const double x0, const double x1 ) 
{
  m_X[0] = x0;
  m_X[1] = x1;
}


/** Copy a point to another point */
template< unsigned int TPointDimension >
SpatialObjectPoint< TPointDimension >::Self & 
SpatialObjectPoint< TPointDimension >
::operator=(const SpatialObjectPoint & rhs) 
{
  m_ID = rhs.m_ID;
  m_NumDimensions = rhs.m_NumDimensions;
  m_X = rhs.m_X;
  return * this;
}

} // end namespace itk

#endif
