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

/**
 *
 */
template <typename TPixelType, typename TCellType>
itkTriangleCell< TPixelType , TCellType >::Pointer
itkTriangleCell< TPixelType , TCellType >
::New(void)
{
  return new Self;
}


/**
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellType>
int
itkTriangleCell< TPixelType , TCellType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TCellType>
itkTriangleCell< TPixelType , TCellType >::CellFeatureCount
itkTriangleCell< TPixelType , TCellType >
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
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 */
template <typename TPixelType, typename TCellType>
itkTriangleCell< TPixelType , TCellType >::Cell::Pointer
itkTriangleCell< TPixelType , TCellType >
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
 * Standard itkCell API:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TCellType>
void
itkTriangleCell< TPixelType , TCellType >
::SetCellPoints(const PointIdentifier *ptList)
{
  for(int i=0; i < NumberOfPoints ; ++i)
    m_PointIds[i] = ptList[i];
}


/**
 * Standard itkCell API:
 * Use this to set all the points in the cell.  It is assumed that the
 * range [first, last) is exactly the size needed for this cell type.
 * The position *last is NOT referenced, so it can safely be one beyond
 * the end of an array.
 */
template <typename TPixelType, typename TCellType>
void
itkTriangleCell< TPixelType , TCellType >
::SetCellPoints(const PointIdentifier* first, const PointIdentifier* last)
{
  int localId=0;
  const PointIdentifier *ii = first;
  
  while(ii != last)
    m_PointIds[localId++] = *ii++;
}


/**
 * Use this to set an individual point identifier in the cell.
 */
template <typename TPixelType, typename TCellType>
void
itkTriangleCell< TPixelType , TCellType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Provide iterator begin and end for the point identifier array.  These
 * are just pointers to the beginning and one past the end.
 */
template <typename TPixelType, typename TCelltype>
itkTriangleCell< TPixelType , TCelltype >::PointIterator
itkTriangleCell< TPixelType , TCelltype >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
itkTriangleCell< TPixelType , TCelltype >::PointConstIterator
itkTriangleCell< TPixelType , TCelltype >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
itkTriangleCell< TPixelType , TCelltype >::PointIterator
itkTriangleCell< TPixelType , TCelltype >
::PointIdsEnd(void)
{
  return &m_PointIds[NumberOfPoints];
}

template <typename TPixelType, typename TCelltype>
itkTriangleCell< TPixelType , TCelltype >::PointConstIterator
itkTriangleCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[NumberOfPoints];
}


/**
 * Triangle-specific:
 * Get the number of vertices defining the triangle.
 */
template <typename TPixelType, typename TCellType>
itkTriangleCell< TPixelType , TCellType >::CellFeatureCount
itkTriangleCell< TPixelType , TCellType >
::GetNumberOfVertices(void)
{
  return NumberOfVertices;
}


/**
 * Triangle-specific:
 * Get the number of edges defined for the triangle.
 */
template <typename TPixelType, typename TCellType>
itkTriangleCell< TPixelType , TCellType >::CellFeatureCount
itkTriangleCell< TPixelType , TCellType >
::GetNumberOfEdges(void)
{
  return NumberOfEdges;
}


/**
 * Triangle-specific:
 * Get the vertex specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCellType>
itkTriangleCell< TPixelType , TCellType >::Vertex::Pointer
itkTriangleCell< TPixelType , TCellType >
::GetCellVertex(CellFeatureIdentifier vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;
}


/**
 * Triangle-specific:
 * Get the edge specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCellType>
itkTriangleCell< TPixelType , TCellType >::Edge::Pointer
itkTriangleCell< TPixelType , TCellType >
::GetCellEdge(CellFeatureIdentifier edgeId)
{
  Edge::Pointer edge(Edge::New());
  
  for(int i=0; i < Edge::NumberOfPoints; ++i)
    edge->SetCellPoint(i, m_PointIds[ m_Edges[edgeId][i] ]);
  
  return edge;
}


/**
 * Define the triangle's topology data.
 */
template <typename TPixelType, typename TCellType>
const int
itkTriangleCell< TPixelType , TCellType >
::m_Edges[3][2] = { {0,1}, {1,2}, {2,0} };


/**
 * Object factory for the boundary version of this cell type.
 */
template <typename TPixelType, typename TCellType>
itkTriangleBoundary< TPixelType , TCellType >::Pointer
itkTriangleBoundary< TPixelType , TCellType >
::New(void)
{
  return new Self;
}

