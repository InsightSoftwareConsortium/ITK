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
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::Pointer
itkTriangleCell< TPixelType , TMeshType >
::New(void)
{
  return new Self;
}


/**
 *
 */
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::CellFeatureID
itkTriangleCell< TPixelType , TMeshType >
::GetNumberOfBoundaryEntities(void)
{
  return 3;
}


/**
 * Standard itkCell API:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TMeshType>
void
itkTriangleCell< TPixelType , TMeshType >
::SetCellPoints(PointIdentifier *ptList)
{
  m_Points[0] = ptList[0];
  m_Points[1] = ptList[1];
  m_Points[2] = ptList[2];
}


/**
 * Triangle-specific:
 * Get the number of vertices defining the triangle.
 */
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::CellFeatureID
itkTriangleCell< TPixelType , TMeshType >
::GetNumberOfVertices(void)
{
  return 3;
}


/**
 * Triangle-specific:
 * Get the number of edges defined for the triangle.
 */
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::CellFeatureID
itkTriangleCell< TPixelType , TMeshType >
::GetNumberOfEdges(void)
{
  return 3;
}


/**
 * Triangle-specific:
 * Get the vertex specified by the given cell feature ID.
 */
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::Vertex::Pointer
itkTriangleCell< TPixelType , TMeshType >
::GetCellVertex(CellFeatureID vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_Points[vertexId]);
  
  return vert;
}


/**
 * Triangle-specific:
 * Get the edge specified by the given cell feature ID.
 */
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::Edge::Pointer
itkTriangleCell< TPixelType , TMeshType >
::GetCellEdge(CellFeatureID edgeId)
{
  CellFeatureID v1 = edgeId;
  CellFeatureID v2 = (edgeId < 2) ? (edgeId+1) : 0;
  
  Edge::Pointer edge(Edge::New());
  edge->SetCellPoint(0, m_Points[v1]);
  edge->SetCellPoint(1, m_Points[v2]);  
  
  return edge;
}

