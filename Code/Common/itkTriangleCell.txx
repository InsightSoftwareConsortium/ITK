/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkTriangleCell.h"

namespace itk
{

/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellType>
int
TriangleCell< TPixelType , TCellType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TCellType>
TriangleCell< TPixelType , TCellType >::CellFeatureCount
TriangleCell< TPixelType , TCellType >
::GetNumberOfBoundaryFeatures(int dimension)
{
  switch (dimension)
    {
    case 0: return GetNumberOfVertices();
    case 1: return GetNumberOfEdges();
    default: return 0;
    }
}


/**
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template <typename TPixelType, typename TCellType>
TriangleCell< TPixelType , TCellType >::Cell::Pointer
TriangleCell< TPixelType , TCellType >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId)
{
  switch (dimension)
    {
    case 0: return Cell::Pointer(GetCellVertex(featureId));
    case 1: return Cell::Pointer(GetCellEdge(featureId));
    default: return Cell::Pointer(NULL);
    }
}


/**
 * Standard CellInterface:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TCellType>
void
TriangleCell< TPixelType , TCellType >
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
TriangleCell< TPixelType , TCellType >
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
TriangleCell< TPixelType , TCellType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCelltype>
TriangleCell< TPixelType , TCelltype >::PointIterator
TriangleCell< TPixelType , TCelltype >
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
TriangleCell< TPixelType , TCelltype >::PointConstIterator
TriangleCell< TPixelType , TCelltype >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCelltype>
TriangleCell< TPixelType , TCelltype >::PointIterator
TriangleCell< TPixelType , TCelltype >
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
TriangleCell< TPixelType , TCelltype >::PointConstIterator
TriangleCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[NumberOfPoints];
}


/**
 * Triangle-specific:
 * Get the number of vertices defining the triangle.
 */
template <typename TPixelType, typename TCellType>
TriangleCell< TPixelType , TCellType >::CellFeatureCount
TriangleCell< TPixelType , TCellType >
::GetNumberOfVertices(void)
{
  return NumberOfVertices;
}


/**
 * Triangle-specific:
 * Get the number of edges defined for the triangle.
 */
template <typename TPixelType, typename TCellType>
TriangleCell< TPixelType , TCellType >::CellFeatureCount
TriangleCell< TPixelType , TCellType >
::GetNumberOfEdges(void)
{
  return NumberOfEdges;
}


/**
 * Triangle-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TPixelType, typename TCellType>
TriangleCell< TPixelType , TCellType >::Vertex::Pointer
TriangleCell< TPixelType , TCellType >
::GetCellVertex(CellFeatureIdentifier vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;
}


/**
 * Triangle-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template <typename TPixelType, typename TCellType>
TriangleCell< TPixelType , TCellType >::Edge::Pointer
TriangleCell< TPixelType , TCellType >
::GetCellEdge(CellFeatureIdentifier edgeId)
{
  Edge::Pointer edge(Edge::New());
  
  for(int i=0; i < Edge::NumberOfPoints; ++i)
    edge->SetCellPoint(i, m_PointIds[ m_Edges[edgeId][i] ]);
  
  return edge;
}


/**
 * The triangle's topology data: Edges
 */
template <typename TPixelType, typename TCellType>
const int
TriangleCell< TPixelType , TCellType >
::m_Edges[3][2] = { {0,1}, {1,2}, {2,0} };

} // namespace itk
