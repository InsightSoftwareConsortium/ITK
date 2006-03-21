/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContourSpatialObjectPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkContourSpatialObjectPoint_txx
#define __itkContourSpatialObjectPoint_txx

#include "itkContourSpatialObjectPoint.h"

namespace itk 
{

/** Constructor */
template< unsigned int TPointDimension >
ContourSpatialObjectPoint< TPointDimension >
::ContourSpatialObjectPoint( void ) 
{ 
  this->m_ID = 0;
  m_Normal.Fill(0);
  m_PickedPoint.Fill(0);
}

/** Destructor */
template< unsigned int TPointDimension >
ContourSpatialObjectPoint< TPointDimension >
::~ContourSpatialObjectPoint( void ) 
{
}

/** Set the picked point : N-D case */
template< unsigned int TPointDimension >
void 
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint( const PointType & point ) 
{
  m_PickedPoint = point;
}

/** Set the picked point : 2D case */
template< unsigned int TPointDimension >
void 
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint(const double pointx, const double pointy)
{
  m_PickedPoint[0] = pointx;
  m_PickedPoint[1] = pointy;
}

/** Set the picked point : 3D case */
template< unsigned int TPointDimension >
void 
ContourSpatialObjectPoint< TPointDimension >
::SetPickedPoint(const double pointx, const double pointy, const double pointz)
{
  m_PickedPoint[0] = pointx;
  m_PickedPoint[1] = pointy;
  m_PickedPoint[2] = pointz;
}

/** Get the normal at one point */
template< unsigned int TPointDimension >
const typename ContourSpatialObjectPoint< TPointDimension >::PointType &
ContourSpatialObjectPoint< TPointDimension >
::GetPickedPoint( void ) const
{
  return m_PickedPoint;
}

/** Set the normal : N-D case */
template< unsigned int TPointDimension >
void 
ContourSpatialObjectPoint< TPointDimension >
::SetNormal( const VectorType & normal ) 
{
  m_Normal = normal;
}

/** Set the normal : 2D case */
template< unsigned int TPointDimension >
void 
ContourSpatialObjectPoint< TPointDimension >
::SetNormal(const double normalx, const double normaly)
{
  m_Normal[0] = normalx;
  m_Normal[1] = normaly;
}

/** Set the normal : 3D case */
template< unsigned int TPointDimension >
void 
ContourSpatialObjectPoint< TPointDimension >
::SetNormal(const double normalx, const double normaly, const double normalz)
{
  m_Normal[0] = normalx;
  m_Normal[1] = normaly;
  m_Normal[2] = normalz;
}

/** Get the normal at one point */
template< unsigned int TPointDimension >
const typename ContourSpatialObjectPoint< TPointDimension >::VectorType &
ContourSpatialObjectPoint< TPointDimension >
::GetNormal( void ) const
{
  return m_Normal;
}

/** Print the object */
template< unsigned int TPointDimension >
void 
ContourSpatialObjectPoint< TPointDimension >
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  Superclass::PrintSelf(os,indent);
  os << indent << "ContourSpatialObjectPoint(" << this << ")" << std::endl; 
  os << indent << "Picked Point: ";
  os << indent <<  m_PickedPoint << std::endl;
  os << indent << "Normal: ";
  os << indent <<  m_Normal << std::endl;
}

/** Copy a surface point to another */
template< unsigned int TPointDimension >
typename ContourSpatialObjectPoint< TPointDimension >::Self & 
ContourSpatialObjectPoint< TPointDimension >
::operator=(const ContourSpatialObjectPoint & rhs) 
{
  this->m_ID = rhs.GetID();
  this->m_X = rhs.GetPosition();
  this->m_Normal = rhs.GetNormal();
  this->m_PickedPoint = rhs.GetPickedPoint();
  return * this;
}

} // end namespace itk

#endif
