/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoundingBox.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
      dist2 += (m_Bounds[2*i]-m_Bounds[2*i+1]) * 
               (m_Bounds[2*i]-m_Bounds[2*i+1]);
      }
    }

  return dist2;
}

} // end namespace itk

#endif
