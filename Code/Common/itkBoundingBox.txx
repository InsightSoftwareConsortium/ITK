/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoundingBox.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Bounding Box: ( " ;
  for (unsigned int i=0; i<PointDimension; i++)
    {
    os << m_Bounds[2*i] << "," << m_Bounds[2*i+1] << " ";
    }
  os << " )" << std::endl;
}

/**
 * Access routine to set the points container.
 */
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
void
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::SetPoints(const PointsContainer* points)
{
  itkDebugMacro("setting Points container to " << points);
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
const typename BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >::PointsContainer *
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::GetPoints(void) const
{
  itkDebugMacro("returning Points container of " << m_PointsContainer );

  return m_PointsContainer.GetPointer();
}

/******************************************************************************
 * PROTECTED METHOD DEFINITIONS
 *****************************************************************************/
template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::BoundingBox():
  m_PointsContainer(NULL)
{
  m_Bounds.Fill( NumericTraits< CoordRepType >::Zero );
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
BoundingBox<TPointIdentifier , VPointDimension, TCoordRep, TPointsContainer >
::~BoundingBox()
{
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
bool  
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::ComputeBoundingBox(void)
{
  if ( !m_PointsContainer )
    {
    if ( this->GetMTime() > m_BoundsMTime )
      {
      m_Bounds.Fill( NumericTraits< CoordRepType >::Zero );
      m_BoundsMTime.Modified();
      }
    return false;
    }

  if ( this->GetMTime() > m_BoundsMTime )
    {
    //iterate over points determining min/max
    //start by initializing the values
    for (unsigned int i=0; i < PointDimension; i++)
      {
      m_Bounds[2*i  ] = NumericTraits<CoordRepType>::max();
      m_Bounds[2*i+1] = NumericTraits<CoordRepType>::min();
      }
    
    //use a const iterator to grab the points and compute
    //the bounding box.
    Point< TCoordRep, VPointDimension>   point;
    for ( PointsContainerConstIterator ci = m_PointsContainer->Begin();
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
typename BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>::PointType 
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::GetCenter(void)
  {
  this->ComputeBoundingBox();

  PointType center;
  for (unsigned int i=0; i<PointDimension; i++)
    {
    center[i] = (m_Bounds[2*i] + m_Bounds[2*i+1]) / 2.0;
    }

  return center;
  }

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
typename BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>::PointType 
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::GetMinimum(void)
  {
  this->ComputeBoundingBox();

  PointType minimum;
  for (unsigned int i=0; i<PointDimension; i++)
    {
    minimum[i] = m_Bounds[2*i];
    }

  return minimum;
  }

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
void
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::SetMinimum(const PointType & point)
  {
  for (unsigned int i=0; i<PointDimension; i++)
    {
    m_Bounds[2*i] = point[i];
    }

  m_BoundsMTime.Modified();
  }

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
typename BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>::PointType 
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::GetMaximum(void)
  {
  this->ComputeBoundingBox();

  PointType maximum;
  for (unsigned int i=0; i<PointDimension; i++)
    {
    maximum[i] = m_Bounds[2*i+1];
    }

  return maximum;
  }

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
void
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::SetMaximum(const PointType & point)
  {
  for (unsigned int i=0; i<PointDimension; i++)
    {
    m_Bounds[2*i+1] = point[i];
    }

  m_BoundsMTime.Modified();
  }

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
void
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::ConsiderPoint( const PointType & point )
{
  bool changed = false;
  for (unsigned int i=0; i<PointDimension; i++)
    {
    if ( point[i] < m_Bounds[2*i] )
      {
      m_Bounds[2*i] = point[i];
      changed = true;
      }
    if ( point[i] > m_Bounds[2*i+1] )
      {
      m_Bounds[2*i+1] = point[i];
      changed = true;
      }
    }

  if(changed)
    {
    m_BoundsMTime.Modified();
    }
}

template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
typename BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>::AccumulateType 
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::GetDiagonalLength2(void)
{
  typename NumericTraits<CoordRepType>::AccumulateType
    dist2 = NumericTraits<CoordRepType>::Zero;

  if ( this->ComputeBoundingBox() )
    {
    for (unsigned int i=0; i<PointDimension; i++)
      {
      dist2 += (m_Bounds[2*i]-m_Bounds[2*i+1]) * 
               (m_Bounds[2*i]-m_Bounds[2*i+1]);
      }
    }

  return dist2;
}



template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
bool
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::IsInside( const PointType & point )
{
  unsigned int j = 0; 
  unsigned int i = 0;
  while( i<PointDimension )
    {
    if( point[i] < m_Bounds[j++] )
      {
      return false;
      }
    if( point[i] > m_Bounds[j++] )
      {
      return false;
      }
    i++;
    }
  return true;
}






template <typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer>
unsigned long
BoundingBox<TPointIdentifier,VPointDimension,TCoordRep,TPointsContainer>
::GetMTime( void ) const
{
  unsigned long latestTime = Object::GetMTime(); 
  if( m_PointsContainer )
    {
    if( latestTime < m_PointsContainer->GetMTime() )
      {
      latestTime = m_PointsContainer->GetMTime();
      }
    }
  return latestTime;
}



} // end namespace itk

#endif
