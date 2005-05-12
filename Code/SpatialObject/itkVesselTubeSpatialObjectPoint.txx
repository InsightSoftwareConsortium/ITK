/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVesselTubeSpatialObjectPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVesselTubeSpatialObjectPoint_txx
#define __itkVesselTubeSpatialObjectPoint_txx

#include "itkVesselTubeSpatialObjectPoint.h"

namespace itk 
{

/** Constructor */
template< unsigned int TPointDimension >
VesselTubeSpatialObjectPoint< TPointDimension >
::VesselTubeSpatialObjectPoint( void ) : Superclass()
{ 
  m_Medialness = 0;
  m_Ridgeness = 0;
  m_Branchness = 0;
  m_Mark = false;
  m_Alpha1 = 0;
  m_Alpha2 = 0;
  m_Alpha3 = 0;
}

/** Destructor */
template< unsigned int TPointDimension >
VesselTubeSpatialObjectPoint< TPointDimension >
::~VesselTubeSpatialObjectPoint( void ) 
{
}

template< unsigned int TPointDimension >
float 
VesselTubeSpatialObjectPoint< TPointDimension >
::GetMedialness( void ) const 
{
  return m_Medialness;
}

template< unsigned int TPointDimension >
void 
VesselTubeSpatialObjectPoint< TPointDimension >
::SetMedialness( const float newMedialness ) 
{
  m_Medialness = newMedialness;
}

template< unsigned int TPointDimension >
float 
VesselTubeSpatialObjectPoint< TPointDimension >
::GetRidgeness( void ) const
{
  return m_Ridgeness;
}

template< unsigned int TPointDimension >
void 
VesselTubeSpatialObjectPoint< TPointDimension >
::SetRidgeness( const float newRidgeness ) 
{
  m_Ridgeness = newRidgeness;
}

template< unsigned int TPointDimension >
float 
VesselTubeSpatialObjectPoint< TPointDimension >
::GetBranchness( void ) const
{
  return m_Branchness;
}

template< unsigned int TPointDimension >
void 
VesselTubeSpatialObjectPoint< TPointDimension >
::SetBranchness( const float newBranchness ) 
{
  m_Branchness = newBranchness;
}

template< unsigned int TPointDimension >
bool 
VesselTubeSpatialObjectPoint< TPointDimension >
::GetMark( void ) const
{
  return m_Mark;
}

template< unsigned int TPointDimension >
void 
VesselTubeSpatialObjectPoint< TPointDimension >
::SetMark( const bool newMark ) 
{
  m_Mark = newMark;
}

template< unsigned int TPointDimension >
float 
VesselTubeSpatialObjectPoint< TPointDimension >
::GetAlpha1( void ) const
{
  return m_Alpha1;
}

template< unsigned int TPointDimension >
void 
VesselTubeSpatialObjectPoint< TPointDimension >
::SetAlpha1( const float newAlpha ) 
{
  m_Alpha1 = newAlpha;
}

template< unsigned int TPointDimension >
float 
VesselTubeSpatialObjectPoint< TPointDimension >
::GetAlpha2( void ) const
{
  return m_Alpha2;
}

template< unsigned int TPointDimension >
void 
VesselTubeSpatialObjectPoint< TPointDimension >
::SetAlpha2( const float newAlpha ) 
{
  m_Alpha2 = newAlpha;
}

template< unsigned int TPointDimension >
float 
VesselTubeSpatialObjectPoint< TPointDimension >
::GetAlpha3( void ) const
{
  return m_Alpha3;
}

template< unsigned int TPointDimension >
void 
VesselTubeSpatialObjectPoint< TPointDimension >
::SetAlpha3( const float newAlpha ) 
{
  m_Alpha3 = newAlpha;
}


template< unsigned int TPointDimension >
void
VesselTubeSpatialObjectPoint< TPointDimension >
::PrintSelf( std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Medialness: " << m_Medialness << std::endl;
  os << indent << "Ridgeness: " << m_Ridgeness << std::endl;
  os << indent << "Alpha1: " << m_Alpha1 << std::endl;
  os << indent << "Alpha2: " << m_Alpha2 << std::endl;
  os << indent << "Alpha3: " << m_Alpha3 << std::endl;
  os << indent << "Mark: " << m_Mark << std::endl;
}


template< unsigned int TPointDimension >
typename VesselTubeSpatialObjectPoint< TPointDimension >::Self & 
VesselTubeSpatialObjectPoint< TPointDimension >
::operator=(const VesselTubeSpatialObjectPoint & rhs) 
{
  this->m_ID = rhs.m_ID;
  this->m_R = rhs.m_R;
  m_Medialness = rhs.m_Medialness;
  m_Ridgeness = rhs.m_Ridgeness;
  m_Branchness = rhs.m_Branchness;
  m_Mark = rhs.m_Mark;
  this->m_NumDimensions = rhs.m_NumDimensions;
  this->m_X = rhs.m_X;
  this->m_T = rhs.m_T;
  this->m_Normal1 = rhs.m_Normal1;
  this->m_Normal2 = rhs.m_Normal2;
  m_Alpha1 = rhs.m_Alpha1;
  m_Alpha2 = rhs.m_Alpha2;
  m_Alpha3 = rhs.m_Alpha3;
  this->m_Color = rhs.m_Color;
  return * this;
}

} // end namespace itk

#endif
