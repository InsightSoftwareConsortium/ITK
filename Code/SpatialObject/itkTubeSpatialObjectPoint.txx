/*=========================================================================

Program: Insight Segmentation & Registration Toolkit
Module:itkTubeSpatialObjectPoint.txx
Language:C++
Date:$Date$
Version: $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

 This software is distributed WITHOUT ANY WARRANTY; without even 
 the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
 PURPOSE.See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTubeSpatialObjectPoint_txx
#define __itkTubeSpatialObjectPoint_txx

#include "itkTubeSpatialObjectPoint.h"

namespace itk 
{

/** Constructor */
template< unsigned int TPointDimension >
TubeSpatialObjectPoint< TPointDimension >
::TubeSpatialObjectPoint( void ) 
{ 
  m_NumDimensions = TPointDimension;
  //m_T = new VectorType(m_NumDimensions);
  //m_Normal1 = new VectorType(m_NumDimensions);
  //m_Normal2 = new VectorType(m_NumDimensions);
  m_T.Fill(0);
  m_Normal1.Fill(0);
  m_Normal2.Fill(0);
  m_R = 0;
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
TubeSpatialObjectPoint< TPointDimension >
::~TubeSpatialObjectPoint( void ) 
{
  //delete m_T;
  //delete m_Normal1;
  //delete m_Normal2;
}

/** Get the radius */
template< unsigned int TPointDimension >
float 
TubeSpatialObjectPoint< TPointDimension >
::GetRadius( void ) const 
{
  return m_R;
}

/** Set the radius */
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetRadius( const float newR ) 
{
  m_R = newR;
}

template< unsigned int TPointDimension >
float 
TubeSpatialObjectPoint< TPointDimension >
::GetMedialness( void ) const 
{
  return m_Medialness;
}

template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetMedialness( const float newMedialness ) 
{
  m_Medialness = newMedialness;
}

template< unsigned int TPointDimension >
float 
TubeSpatialObjectPoint< TPointDimension >
::GetRidgeness( void ) const
{
  return m_Ridgeness;
}

template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetRidgeness( const float newRidgeness ) 
{
  m_Ridgeness = newRidgeness;
}

template< unsigned int TPointDimension >
float 
TubeSpatialObjectPoint< TPointDimension >
::GetBranchness( void ) const
{
  return m_Branchness;
}

template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetBranchness( const float newBranchness ) 
{
  m_Branchness = newBranchness;
}

template< unsigned int TPointDimension >
bool 
TubeSpatialObjectPoint< TPointDimension >
::GetMark( void ) const
{
  return m_Mark;
}

template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetMark( const bool newMark ) 
{
  m_Mark = newMark;
}


template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::VectorType & 
TubeSpatialObjectPoint< TPointDimension >
::GetTangent( void ) const
{
  return m_T;
}

// n-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetTangent( const VectorType & newT ) 
{
  m_T = newT;
}

// 3-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetTangent( const double t0, const double t1, const double t2 ) 
{
  m_T[0] = t0;
  m_T[1] = t1;
  m_T[2] = t2;
}

// 2-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetTangent( const double t0, const double t1 ) 
{
  m_T[0] = t0;
  m_T[1] = t1;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetNormal1() const
{
  return m_Normal1;
}

/*template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetCV1() 
{
  int i;
  for(i=0; i<TPointDimension; i++)
    {
    m_CV1[i] = m_Normal1[i];
    }
  return m_CV1;
}*/

// n-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1( const CovariantVectorType & newV1 ) 
{
  m_Normal1 = newV1;
}

// n-D case
/*template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1( const CovariantVectorType & newV1 ) 
  {
  int i;
  for(i=0; i<TPointDimension; i++)
    {
    m_Normal1[i] = newV1[i];
    }
  }
*/

// 3-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1( const double v10, const double v11, const double v12 ) 
{
  m_Normal1[0] = v10;
  m_Normal1[1] = v11;
  m_Normal1[2] = v12;
}

// 2-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetNormal1( const double v10, const double v11 ) 
{
  m_Normal1[0] = v10;
  m_Normal1[1] = v11;
}

template< unsigned int TPointDimension >
const typename TubeSpatialObjectPoint< TPointDimension >::CovariantVectorType &
TubeSpatialObjectPoint< TPointDimension >
::GetNormal2() const
{
  return m_Normal2;
}


// n-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetNormal2( const CovariantVectorType & newV2 ) 
{
  m_Normal2 = newV2;
}

// 3-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetNormal2( const double v20, const double v21, const double v22 ) 
{
  m_Normal2[0] = v20;
  m_Normal2[1] = v21;
  m_Normal2[2] = v22;
}

// 2-D case
template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetNormal2( const double v20, const double v21 ) 
{
  m_Normal2[0] = v20;
  m_Normal2[1] = v21;
}

template< unsigned int TPointDimension >
float 
TubeSpatialObjectPoint< TPointDimension >
::GetAlpha1( void ) const
{
  return m_Alpha1;
}

template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetAlpha1( const float newAlpha ) 
{
  m_Alpha1 = newAlpha;
}

template< unsigned int TPointDimension >
float 
TubeSpatialObjectPoint< TPointDimension >
::GetAlpha2( void ) const
{
  return m_Alpha2;
}

template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetAlpha2( const float newAlpha ) 
{
  m_Alpha2 = newAlpha;
}

template< unsigned int TPointDimension >
float 
TubeSpatialObjectPoint< TPointDimension >
::GetAlpha3( void ) const
{
  return m_Alpha3;
}

template< unsigned int TPointDimension >
void 
TubeSpatialObjectPoint< TPointDimension >
::SetAlpha3( const float newAlpha ) 
{
  m_Alpha3 = newAlpha;
}


template< unsigned int TPointDimension >
void
TubeSpatialObjectPoint< TPointDimension >
::PrintSelf( std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "#Dims: " << m_NumDimensions << std::endl;
  os << indent << "R: " << m_R << std::endl;
  os << indent << "Medialness: " << m_Medialness << std::endl;
  os << indent << "Ridgeness: " << m_Ridgeness << std::endl;
  os << indent << "X: " << m_X << std::endl;
  os << indent << "T: " << m_T << std::endl;
  os << indent << "Normal1: " << m_Normal1 << std::endl;
  os << indent << "Normal2: " << m_Normal2 << std::endl;
  os << indent << "Alpha1: " << m_Alpha1 << std::endl;
  os << indent << "Alpha2: " << m_Alpha2 << std::endl;
  os << indent << "Alpha3: " << m_Alpha3 << std::endl;
  os << indent << "Mark: " << m_Mark << std::endl;
}


template< unsigned int TPointDimension >
typename TubeSpatialObjectPoint< TPointDimension >::Self & 
TubeSpatialObjectPoint< TPointDimension >
::operator=(const TubeSpatialObjectPoint & rhs) 
{
  m_ID = rhs.m_ID;
  m_R = rhs.m_R;
  m_Medialness = rhs.m_Medialness;
  m_Ridgeness = rhs.m_Ridgeness;
  m_Branchness = rhs.m_Branchness;
  m_Mark = rhs.m_Mark;
  m_NumDimensions = rhs.m_NumDimensions;
  m_X = rhs.m_X;
  m_T = rhs.m_T;
  m_Normal1 = rhs.m_Normal1;
  m_Normal2 = rhs.m_Normal2;
  m_Alpha1 = rhs.m_Alpha1;
  m_Alpha2 = rhs.m_Alpha2;
  m_Alpha3 = rhs.m_Alpha3;
  m_Color = rhs.m_Color;
  return * this;
}

} // end namespace itk

#endif
