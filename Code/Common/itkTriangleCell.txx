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
 * Get the number of boundary entities of the given dimension.
 */
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::CellFeatureCount
itkTriangleCell< TPixelType , TMeshType >
::GetNumberOfBoundaryEntities(int dimension)
{
  switch (dimension)
    {
    case 0: return GetNumberOfVertices();
    case 1: return GetNumberOfEdges();
    default: return 0;
    }
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
  for(int i=0; i < NumberOfPoints ; ++i)
    m_PointIds[i] = ptList[i];
}


/**
 * Triangle-specific:
 * Get the number of vertices defining the triangle.
 */
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::CellFeatureCount
itkTriangleCell< TPixelType , TMeshType >
::GetNumberOfVertices(void)
{
  return NumberOfVertices;
}


/**
 * Triangle-specific:
 * Get the number of edges defined for the triangle.
 */
template <typename TPixelType, typename TMeshType>
itkTriangleCell< TPixelType , TMeshType >::CellFeatureCount
itkTriangleCell< TPixelType , TMeshType >
::GetNumberOfEdges(void)
{
  return NumberOfEdges;
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
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
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
  Edge::Pointer edge(Edge::New());
  
  edge->SetCellPoint(0, m_PointIds[ m_Edges[edgeId][0] ]);
  edge->SetCellPoint(1, m_PointIds[ m_Edges[edgeId][1] ]);
  
  return edge;
}


/**
 * Define the triangle's topology data.
 */
template <typename TPixelType, typename TMeshType>
const int
itkTriangleCell< TPixelType , TMeshType >
::m_Edges[3][2] = { {0,1}, {1,2}, {2,0} };

