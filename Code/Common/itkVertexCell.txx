/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVertexCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVertexCell_txx
#define _itkVertexCell_txx
#include "itkVertexCell.h"

namespace itk
{
 
/**
 * Standard CellInterface:
 */
template <typename TCellInterface>
void
VertexCell< TCellInterface >
::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership( new Self );
  cellPointer->SetPointIds(this->GetPointIds());
}



  
/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TCellInterface>
unsigned int
VertexCell< TCellInterface >
::GetDimension(void) const
{
  return Self::CellDimension;
}


/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TCellInterface>
unsigned int
VertexCell< TCellInterface >
::GetNumberOfPoints(void) const
{
  return Self::NumberOfPoints;
}  


/**
 * Standard CellInterface:
 * A vertex has no boundary entities of any dimension.
 */
template <typename TCellInterface>
VertexCell< TCellInterface >::CellFeatureCount
VertexCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int) const
{
  return 0;
}


/**
 * Standard CellInterface:
 * A vertex has no boundary entities.  Just return null.
 */
template <typename TCellInterface >
bool
VertexCell< TCellInterface >
::GetBoundaryFeature(int, CellFeatureIdentifier, CellAutoPointer & cellAPtr )
{
  cellAPtr.Reset();
  return false;
}


/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */
template <typename TCellInterface>
void
VertexCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);
  for(unsigned int i=0; i < Self::NumberOfPoints ; ++i)
    {
    m_PointIds[i] = *ii++;
    }
}


/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the range
 * of iterators [first, last) contains the correct number of points needed to
 * define the cell.  The position *last is NOT referenced, so it can safely
 * be one beyond the end of an array or other container.
 */
template <typename TCellInterface>
void
VertexCell< TCellInterface >
::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int localId=0;
  PointIdConstIterator ii(first);
  
  while(ii != last)
    {
    m_PointIds[localId++] = *ii++;
    }
}


/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */
template <typename TCellInterface>
void
VertexCell< TCellInterface >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
VertexCell< TCellInterface >::PointIdIterator
VertexCell< TCellInterface >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
VertexCell< TCellInterface >::PointIdConstIterator
VertexCell< TCellInterface >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
VertexCell< TCellInterface >::PointIdIterator
VertexCell< TCellInterface >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
VertexCell< TCellInterface >::PointIdConstIterator
VertexCell< TCellInterface >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Vertex-specific:
 * Set the identifier of the point defining the vertex.
 */
template <typename TCellInterface>
void
VertexCell< TCellInterface >
::SetPointId(PointIdentifier ptId)
{
  m_PointIds[0] = ptId;
}


/**
 * Vertex-specific:
 * Get the identifier of the point defining the vertex.
 */
template <typename TCellInterface>
VertexCell< TCellInterface >::PointIdentifier
VertexCell< TCellInterface >
::GetPointId(void)
{
  return m_PointIds[0];
}


} // end namespace itk

#endif
