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
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellType>
int
VertexCell< TPixelType , TCellType >
::GetCellDimension(void)
{
  return CellDimension;
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
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TCellType>
void
VertexCell< TPixelType , TCellType >
::SetCellPoints(const PointIdentifier *ptList)
{
  for(int i=0; i < NumberOfPoints ; ++i)
    m_PointIds[i] = ptList[i];
}


/**
 * Standard CellInterface:
 * Use this to set all the points in the cell.  It is assumed that the
 * range [first, last) is exactly the size needed for this cell type.
 * The position *last is NOT referenced, so it can safely be one beyond
 * the end of an array.
 */
template <typename TPixelType, typename TCellType>
void
VertexCell< TPixelType , TCellType >
::SetCellPoints(const PointIdentifier* first, const PointIdentifier* last)
{
  int localId=0;
  const PointIdentifier *ii = first;
  
  while(ii != last)
    m_PointIds[localId++] = *ii++;
}


/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */
template <typename TPixelType, typename TCellType>
void
VertexCell< TPixelType , TCellType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCelltype>
VertexCell< TPixelType , TCelltype >::PointIterator
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
VertexCell< TPixelType , TCelltype >::PointConstIterator
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
VertexCell< TPixelType , TCelltype >::PointIterator
VertexCell< TPixelType , TCelltype >
::PointIdsEnd(void)
{
  return &m_PointIds[NumberOfPoints];
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TPixelType, typename TCelltype>
VertexCell< TPixelType , TCelltype >::PointConstIterator
VertexCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[NumberOfPoints];
}


/**
 * Vertex-specific:
 * Get the identifier of the point defining the vertex.
 */
template <typename TPixelType, typename TCellType>
VertexCell< TPixelType , TCellType >::PointIdentifier
VertexCell< TPixelType , TCellType >
::GetCellPoint(void)
{
  return m_PointIds[0];
}


} // namespace itk
