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
// #include "itkVertexCell.h"

namespace itk
{
 
/**
 * Standard CellInterface:
 */
template <typename TPixelType, typename TCelltype>
VertexCell< TPixelType , TCelltype >::Cell::Pointer
VertexCell< TPixelType , TCelltype >
::MakeCopy(void)
{
  Cell::Pointer newCell(Self::New());
  newCell->SetPointIds(this->GetPointIds());
  return newCell;
}

  
/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellType>
int
VertexCell< TPixelType , TCellType >
::GetDimension(void)
{
  return Self::CellDimension;
}


/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TPixelType, typename TCelltype>
int
VertexCell< TPixelType , TCelltype >
::GetNumberOfPoints(void)
{
  return Self::NumberOfPoints;
}  


/**
 * Standard CellInterface:
 * A vertex has no boundary entities of any dimension.
 */
template <typename TPixelType, typename TCellType>
VertexCell< TPixelType , TCellType >::CellFeatureCount
VertexCell< TPixelType , TCellType >
::GetNumberOfBoundaryFeatures(int)
{
  return 0;
}


/**
 * Standard CellInterface:
 * A vertex has no boundary entities.  Just return null.
 */
template <typename TPixelType, typename TCellType>
VertexCell< TPixelType , TCellType >::Cell::Pointer
VertexCell< TPixelType , TCellType >
::GetBoundaryFeature(int, CellFeatureIdentifier)
{
  return Cell::Pointer(NULL);
}


/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */
template <typename TPixelType, typename TCelltype>
void
VertexCell< TPixelType , TCelltype >
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
template <typename TPixelType, typename TCelltype>
void
VertexCell< TPixelType , TCelltype >
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
template <typename TPixelType, typename TCelltype>
void
VertexCell< TPixelType , TCelltype >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCelltype>
VertexCell< TPixelType , TCelltype >::PointIdIterator
VertexCell< TPixelType , TCelltype >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TPixelType, typename TCelltype>
VertexCell< TPixelType , TCelltype >::PointIdConstIterator
VertexCell< TPixelType , TCelltype >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCelltype>
VertexCell< TPixelType , TCelltype >::PointIdIterator
VertexCell< TPixelType , TCelltype >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TPixelType, typename TCelltype>
VertexCell< TPixelType , TCelltype >::PointIdConstIterator
VertexCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Vertex-specific:
 * Set the identifier of the point defining the vertex.
 */
template <typename TPixelType, typename TCellType>
void
VertexCell< TPixelType , TCellType >
::SetPointId(PointIdentifier ptId)
{
  m_PointIds[0] = ptId;
}


/**
 * Vertex-specific:
 * Get the identifier of the point defining the vertex.
 */
template <typename TPixelType, typename TCellType>
VertexCell< TPixelType , TCellType >::PointIdentifier
VertexCell< TPixelType , TCellType >
::GetPointId(void)
{
  return m_PointIds[0];
}


} // namespace itk
