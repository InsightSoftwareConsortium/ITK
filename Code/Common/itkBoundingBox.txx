/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoundingBox.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkBoundingBox_txx
#define _itkBoundingBox_txx
#include "itkBoundingBox.h"
#include "itkNumericTraits.h"
#include "itkPoint.h"

namespace itk
{

/**
 * Print out the bounding box.
 */
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
void
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os, indent);

  if ( m_Bounds )
    {
    os << indent << "Bounding Box: ( " ;
    for (unsigned int i=0; i<PointDimension; i++)
      {
      os << m_Bounds[2*i] << "," << m_Bounds[2*i+1] << " ";
      }
    os << " )" << std::endl;
    }
  else
    {
    os << indent << "Bounding Box not defined" << std::endl;
    }
}

/**
 * Access routine to set the points container.
 */
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
void
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::SetPoints(PointsContainer* points)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): setting Points container to " << points);
  if(m_PointsContainer.GetPointer() != points)
    {
    m_PointsContainer = points;
    this->Modified();
    }
}

/**
 * Access routine to get the points container.
 */
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >::PointsContainerPointer
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::GetPoints(void)
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning Points container of " << m_PointsContainer );

  return m_PointsContainer;
}

/******************************************************************************
 * PROTECTED METHOD DEFINITIONS
 *****************************************************************************/
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::BoundingBox():
  m_PointsContainer(NULL),
  m_Bounds(NULL)
{
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::~BoundingBox()
{
  if ( m_Bounds )
    {   
    delete [] m_Bounds;
    }
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
bool  
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::ComputeBoundingBox(void)
{
  if ( !m_PointsContainer )
    {
    return false;
    }

  if ( !m_Bounds || this->GetMTime() > m_BoundsMTime )
    {
    if ( !m_Bounds )
      {
      m_Bounds = new CoordRepType[2*PointDimension];
      }
    
    //iterate over points determining min/max
    //start by initializing the values
    for (unsigned int i=0; i < PointDimension; i++)
      {
      m_Bounds[2*i] = NumericTraits<CoordRepType>::max();
      m_Bounds[2*i+1] = NumericTraits<CoordRepType>::min();
      }
    
    //use a const iterator to grab the points and compute
    //the bounding box.
    Point< TCoordRep, VPointDimension>   point;
    for ( PointsContainerIterator ci = m_PointsContainer->Begin();
          ci != m_PointsContainer->End(); ++ci )
      {
      point = ci->Value();     //point value
    for (unsigned int i=0; i<PointDimension; i++)
      {
    if ( point[i] < m_Bounds[2*i] )
      {
      m_Bounds[2*i] = point[i];
      }
    if ( point[i] > m_Bounds[2*i+1] )
      {
      m_Bounds[2*i+1] = point[i];
      }
    }
      }//for all points in container

    m_BoundsMTime.Modified();
    }

  return true;
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>::CoordRepType* 
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::GetBoundingBox(CoordRepType bounds[PointDimension*2])
{
  if ( this->ComputeBoundingBox() )
    {
    for (unsigned int i=0; i<PointDimension; i++)
      {
      bounds[2*i] = m_Bounds[2*i];
      bounds[2*i+1] = m_Bounds[2*i+1];
      }
    return bounds;
    }
  else
    {
    return NULL;
    }
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>::CoordRepType*  
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::GetCenter(CoordRepType center[PointDimension])
{
  if ( this->ComputeBoundingBox() )
    {
    for (unsigned int i=0; i<PointDimension; i++)
      {
      center[i] = (m_Bounds[2*i] + m_Bounds[2*i+1]) / 2.0;
      }
    return center;
    }
  else
    {
    return NULL;
    }
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>::AccumulateType 
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::GetDiagonalLength2(void)
{
  NumericTraits<CoordRepType>::AccumulateType 
    dist2 = NumericTraits<CoordRepType>::Zero;

  if ( this->ComputeBoundingBox() )
    {
    for (unsigned int i=0; i<PointDimension; i++)
      {
      dist2 += (m_Bounds[2*i]-m_Bounds[2*i+1]) * (m_Bounds[2*i]-m_Bounds[2*i+1]);
      }
    }

  return dist2;
}

} // end namespace itk

#endif
