/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadrilateralCell.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkQuadrilateralCell.h"

/**
 *
 */
template <typename TPixelType, typename TMeshType>
itkQuadrilateralCell< TPixelType , TMeshType >::Pointer
itkQuadrilateralCell< TPixelType , TMeshType >
::New(void)
{
  return new Self;
}


/**
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TMeshType>
int
itkQuadrilateralCell< TPixelType , TMeshType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TMeshType>
itkQuadrilateralCell< TPixelType , TMeshType >::CellFeatureCount
itkQuadrilateralCell< TPixelType , TMeshType >
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
template <typename TPixelType, typename TMeshType>
itkQuadrilateralCell< TPixelType , TMeshType >::Cell::Pointer
itkQuadrilateralCell< TPixelType , TMeshType >
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
template <typename TPixelType, typename TMeshType>
void
itkQuadrilateralCell< TPixelType , TMeshType >
::SetCellPoints(PointIdentifier *ptList)
{
  for(int i=0; i < NumberOfPoints ; ++i)
    m_PointIds[i] = ptList[i];
}


/**
 * Quadrilateral-specific:
 * Get the number of vertices defining the quadrilateral.
 */
template <typename TPixelType, typename TMeshType>
itkQuadrilateralCell< TPixelType , TMeshType >::CellFeatureCount
itkQuadrilateralCell< TPixelType , TMeshType >
::GetNumberOfVertices(void)
{
  return NumberOfVertices;
}


/**
 * Quadrilateral-specific:
 * Get the number of edges defined for the quadrilateral.
 */
template <typename TPixelType, typename TMeshType>
itkQuadrilateralCell< TPixelType , TMeshType >::CellFeatureCount
itkQuadrilateralCell< TPixelType , TMeshType >
::GetNumberOfEdges(void)
{
  return NumberOfEdges;
}


/**
 * Quadrilateral-specific:
 * Get the vertex specified by the given cell feature Id.
 */
template <typename TPixelType, typename TMeshType>
itkQuadrilateralCell< TPixelType , TMeshType >::Vertex::Pointer
itkQuadrilateralCell< TPixelType , TMeshType >
::GetCellVertex(CellFeatureIdentifier vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;
}


/**
 * Quadrilateral-specific:
 * Get the edge specified by the given cell feature Id.
 */
template <typename TPixelType, typename TMeshType>
itkQuadrilateralCell< TPixelType , TMeshType >::Edge::Pointer
itkQuadrilateralCell< TPixelType , TMeshType >
::GetCellEdge(CellFeatureIdentifier edgeId)
{
  Edge::Pointer edge(Edge::New());
  
  edge->SetCellPoint(0, m_PointIds[ m_Edges[edgeId][0] ]);
  edge->SetCellPoint(1, m_PointIds[ m_Edges[edgeId][1] ]);
  
  return edge;
}


/**
 * Define the quadrilateral's topology data.
 */
template <typename TPixelType, typename TMeshType>
const int
itkQuadrilateralCell< TPixelType , TMeshType >
::m_Edges[4][2] = { {0,1}, {1,2}, {2,3}, {3,0} };
