/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVertexCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkVertexCell.h"

namespace itk
{
 
/**
 * Standard CellInterface:
 */
template <typename TPixelType, typename TCellTraits>
VertexCell< TPixelType , TCellTraits >::CellPointer
VertexCell< TPixelType , TCellTraits >
::MakeCopy(void)
{
  CellPointer newCell(Self::New());
  newCell->SetPointIds(this->GetPointIds());
  return newCell;
}

  
/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellTraits>
int
VertexCell< TPixelType , TCellTraits >
::GetDimension(void)
{
  return Self::CellDimension;
}


/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TPixelType, typename TCellTraits>
int
VertexCell< TPixelType , TCellTraits >
::GetNumberOfPoints(void)
{
  return Self::NumberOfPoints;
}  


/**
 * Standard CellInterface:
 * A vertex has no boundary entities of any dimension.
 */
template <typename TPixelType, typename TCellTraits>
VertexCell< TPixelType , TCellTraits >::CellFeatureCount
VertexCell< TPixelType , TCellTraits >
::GetNumberOfBoundaryFeatures(int)
{
  return 0;
}


/**
 * Standard CellInterface:
 * A vertex has no boundary entities.  Just return null.
 */
template <typename TPixelType, typename TCellTraits>
VertexCell< TPixelType , TCellTraits >::CellPointer
VertexCell< TPixelType , TCellTraits >
::GetBoundaryFeature(int, CellFeatureIdentifier)
{
  return CellPointer(NULL);
}


/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */
template <typename TPixelType, typename TCellTraits>
void
VertexCell< TPixelType , TCellTraits >
::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);
  for(int i=0; i < Self::NumberOfPoints ; ++i)
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
template <typename TPixelType, typename TCellTraits>
void
VertexCell< TPixelType , TCellTraits >
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
template <typename TPixelType, typename TCellTraits>
void
VertexCell< TPixelType , TCellTraits >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCellTraits>
VertexCell< TPixelType , TCellTraits >::PointIdIterator
VertexCell< TPixelType , TCellTraits >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TPixelType, typename TCellTraits>
VertexCell< TPixelType , TCellTraits >::PointIdConstIterator
VertexCell< TPixelType , TCellTraits >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCellTraits>
VertexCell< TPixelType , TCellTraits >::PointIdIterator
VertexCell< TPixelType , TCellTraits >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TPixelType, typename TCellTraits>
VertexCell< TPixelType , TCellTraits >::PointIdConstIterator
VertexCell< TPixelType , TCellTraits >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Vertex-specific:
 * Set the identifier of the point defining the vertex.
 */
template <typename TPixelType, typename TCellTraits>
void
VertexCell< TPixelType , TCellTraits >
::SetPointId(PointIdentifier ptId)
{
  m_PointIds[0] = ptId;
}


/**
 * Vertex-specific:
 * Get the identifier of the point defining the vertex.
 */
template <typename TPixelType, typename TCellTraits>
VertexCell< TPixelType , TCellTraits >::PointIdentifier
VertexCell< TPixelType , TCellTraits >
::GetPointId(void)
{
  return m_PointIds[0];
}


} // end namespace itk
