/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBoundingBox_hxx
#define itkBoundingBox_hxx
#include "itkBoundingBox.h"

namespace itk
{
/**
 * Print out the bounding box.
 */
template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
void
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Bounding Box: ( ";
  for ( unsigned int i = 0; i < PointDimension; i++ )
    {
    os << m_Bounds[2 * i] << "," << m_Bounds[2 * i + 1] << " ";
    }
  os << " )" << std::endl;
}

/**
 * Access routine to set the points container.
 */
template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
void
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::SetPoints(const PointsContainer *points)
{
  itkDebugMacro("setting Points container to " << points);
  if ( m_PointsContainer.GetPointer() != points )
    {
    m_PointsContainer = points;
    this->Modified();
    }
}

/** Access routine to get the points container. */
template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
const typename BoundingBox< TPointIdentifier, VPointDimension, TCoordRep,
                            TPointsContainer >::PointsContainer *
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::GetPoints(void) const
{
  itkDebugMacro("returning Points container of " << m_PointsContainer);

  return m_PointsContainer.GetPointer();
}

/** Compute and get the corners of the bounding box */
template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
const typename BoundingBox< TPointIdentifier, VPointDimension, TCoordRep,
                            TPointsContainer >::PointsContainer *
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::GetCorners(void)
{
  m_CornersContainer->clear();

  PointType center = this->GetCenter();
  PointType radius;

  for ( unsigned int i = 0; i < VPointDimension; i++ )
    {
    radius[i] = m_Bounds[2 * i + 1] - center[i];
    }

  for ( unsigned int j = 0; j < std::pow(2.0, (double)VPointDimension); j++ )
    {
    PointType pnt;
    for ( unsigned int i = 0; i < VPointDimension; i++ )
      {
      pnt[i] = center[i] + std::pow( -1.0, ( (double)( j / ( int( std::pow(2.0, (double)i) ) ) ) ) )
               * radius[i];
      }

    // Push back is not defined so we insert at the end of the list
    m_CornersContainer->InsertElement(m_CornersContainer->Size(), pnt);
    }

  return m_CornersContainer.GetPointer();
}

/** */
template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::BoundingBox():m_PointsContainer(ITK_NULLPTR)
{
  m_Bounds.Fill(NumericTraits< CoordRepType >::ZeroValue());
  m_CornersContainer = PointsContainer::New();
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::~BoundingBox()
{}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
bool
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::ComputeBoundingBox(void) const
{
  if ( !m_PointsContainer )
    {
    if ( this->GetMTime() > m_BoundsMTime )
      {
      m_Bounds.Fill(NumericTraits< CoordRepType >::ZeroValue());
      m_BoundsMTime.Modified();
      }
    return false;
    }

  if ( this->GetMTime() > m_BoundsMTime )
    {
    //iterate over points determining min/max
    //start by initializing the values
    if ( m_PointsContainer->Size() < 1 )
      {
      m_Bounds.Fill(NumericTraits< CoordRepType >::ZeroValue());
      m_BoundsMTime.Modified();
      return false;
      }

    PointsContainerConstIterator        ci = m_PointsContainer->Begin();
    Point< TCoordRep, VPointDimension > point = ci->Value();      //point value
    for ( unsigned int i = 0; i < PointDimension; i++ )
      {
      m_Bounds[2 * i] = point[i];
      m_Bounds[2 * i + 1] = point[i];
      }
    ++ci;

    //use a const iterator to grab the points and compute
    //the bounding box.
    while ( ci != m_PointsContainer->End() )
      {
      point = ci->Value();     //point value
      for ( unsigned int i = 0; i < PointDimension; i++ )
        {
        if ( point[i] < m_Bounds[2 * i] )
          {
          m_Bounds[2 * i] = point[i];
          }
        if ( point[i] > m_Bounds[2 * i + 1] )
          {
          m_Bounds[2 * i + 1] = point[i];
          }
        }
      ++ci;
      } //for all points in container

    m_BoundsMTime.Modified();
    }

  return true;
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
typename BoundingBox< TPointIdentifier, VPointDimension, TCoordRep,
                      TPointsContainer >::PointType
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::GetCenter(void) const
{
  this->ComputeBoundingBox();

  PointType center;
  for ( unsigned int i = 0; i < PointDimension; i++ )
    {
    center[i] = ( m_Bounds[2 * i] + m_Bounds[2 * i + 1] ) / 2.0;
    }

  return center;
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
typename BoundingBox< TPointIdentifier, VPointDimension, TCoordRep,
                      TPointsContainer >::PointType
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::GetMinimum(void) const
{
  this->ComputeBoundingBox();

  PointType minimum;
  for ( unsigned int i = 0; i < PointDimension; i++ )
    {
    minimum[i] = m_Bounds[2 * i];
    }

  return minimum;
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
void
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::SetMinimum(const PointType & point)
{
  for ( unsigned int i = 0; i < PointDimension; i++ )
    {
    m_Bounds[2 * i] = point[i];
    }

  m_BoundsMTime.Modified();
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
typename BoundingBox< TPointIdentifier, VPointDimension, TCoordRep,
                      TPointsContainer >::PointType
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::GetMaximum(void) const
{
  this->ComputeBoundingBox();

  PointType maximum;
  for ( unsigned int i = 0; i < PointDimension; i++ )
    {
    maximum[i] = m_Bounds[2 * i + 1];
    }

  return maximum;
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
void
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::SetMaximum(const PointType & point)
{
  for ( unsigned int i = 0; i < PointDimension; i++ )
    {
    m_Bounds[2 * i + 1] = point[i];
    }

  m_BoundsMTime.Modified();
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
void
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::ConsiderPoint(const PointType & point)
{
  bool changed = false;

  for ( unsigned int i = 0; i < PointDimension; i++ )
    {
    if ( point[i] < m_Bounds[2 * i] )
      {
      m_Bounds[2 * i] = point[i];
      changed = true;
      }
    if ( point[i] > m_Bounds[2 * i + 1] )
      {
      m_Bounds[2 * i + 1] = point[i];
      changed = true;
      }
    }

  if ( changed )
    {
    m_BoundsMTime.Modified();
    }
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
typename BoundingBox< TPointIdentifier, VPointDimension, TCoordRep,
                      TPointsContainer >::AccumulateType
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::GetDiagonalLength2(void) const
{
  typename NumericTraits< CoordRepType >::AccumulateType
  dist2 = NumericTraits< CoordRepType >::ZeroValue();

  if ( this->ComputeBoundingBox() )
    {
    for ( unsigned int i = 0; i < PointDimension; i++ )
      {
      dist2 += ( m_Bounds[2 * i] - m_Bounds[2 * i + 1] )
               * ( m_Bounds[2 * i] - m_Bounds[2 * i + 1] );
      }
    }

  return dist2;
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
bool
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::IsInside(const PointType & point) const
{
  unsigned int j = 0;
  unsigned int i = 0;

  while ( i < PointDimension )
    {
    if ( point[i] < m_Bounds[j++] )
      {
      return false;
      }
    if ( point[i] > m_Bounds[j++] )
      {
      return false;
      }
    i++;
    }
  return true;
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
ModifiedTimeType
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::GetMTime(void) const
{
  ModifiedTimeType latestTime = Object::GetMTime();

  if ( m_PointsContainer )
    {
    if ( latestTime < m_PointsContainer->GetMTime() )
      {
      latestTime = m_PointsContainer->GetMTime();
      }
    }
  return latestTime;
}

template< typename TPointIdentifier, int VPointDimension,
          typename TCoordRep, typename TPointsContainer >
typename BoundingBox< TPointIdentifier, VPointDimension, TCoordRep,
                      TPointsContainer >::Pointer
BoundingBox< TPointIdentifier, VPointDimension, TCoordRep, TPointsContainer >
::DeepCopy(void) const
{
  Pointer clone = Self::New();

  // Connect the same points container into the clone
  clone->SetPoints(this->m_PointsContainer);

  // Copy the corners into the clone.
  clone->m_CornersContainer->clear();

  PointsContainerConstIterator itr = this->m_CornersContainer->Begin();
  PointsContainerConstIterator end = this->m_CornersContainer->End();

  clone->m_CornersContainer->Reserve( this->m_CornersContainer->Size() );
  PointsContainerIterator dest = clone->m_CornersContainer->Begin();

  while ( itr != end )
    {
    dest.Value() = itr.Value();
    ++itr;
    }

  // Copy the bounds into the clone
  for ( unsigned int i = 0; i < 2 * PointDimension; i++ )
    {
    clone->m_Bounds[i] = this->m_Bounds[i];
    }

  return clone;
}
} // end namespace itk

#endif
