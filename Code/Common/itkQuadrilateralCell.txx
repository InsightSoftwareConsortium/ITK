/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadrilateralCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkQuadrilateralCell.h"

namespace itk
{

/**
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellType>
int
QuadrilateralCell< TPixelType , TCellType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TCellType>
QuadrilateralCell< TPixelType , TCellType >::CellFeatureCount
QuadrilateralCell< TPixelType , TCellType >
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
QuadrilateralCell< TPixelType , TCellType >::Cell::Pointer
QuadrilateralCell< TPixelType , TCellType >
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
QuadrilateralCell< TPixelType , TCellType >
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
QuadrilateralCell< TPixelType , TCellType >
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
QuadrilateralCell< TPixelType , TCellType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Provide iterator begin and end for the point identifier array.  These
 * are just pointers to the beginning and one past the end.
 */
template <typename TPixelType, typename TCelltype>
QuadrilateralCell< TPixelType , TCelltype >::PointIterator
QuadrilateralCell< TPixelType , TCelltype >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
QuadrilateralCell< TPixelType , TCelltype >::PointConstIterator
QuadrilateralCell< TPixelType , TCelltype >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
QuadrilateralCell< TPixelType , TCelltype >::PointIterator
QuadrilateralCell< TPixelType , TCelltype >
::PointIdsEnd(void)
{
  return &m_PointIds[NumberOfPoints];
}

template <typename TPixelType, typename TCelltype>
QuadrilateralCell< TPixelType , TCelltype >::PointConstIterator
QuadrilateralCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[NumberOfPoints];
}


/**
 * Quadrilateral-specific:
 * Get the number of vertices defining the quadrilateral.
 */
template <typename TPixelType, typename TCellType>
QuadrilateralCell< TPixelType , TCellType >::CellFeatureCount
QuadrilateralCell< TPixelType , TCellType >
::GetNumberOfVertices(void)
{
  return NumberOfVertices;
}


/**
 * Quadrilateral-specific:
 * Get the number of edges defined for the quadrilateral.
 */
template <typename TPixelType, typename TCellType>
QuadrilateralCell< TPixelType , TCellType >::CellFeatureCount
QuadrilateralCell< TPixelType , TCellType >
::GetNumberOfEdges(void)
{
  return NumberOfEdges;
}


/**
 * Quadrilateral-specific:
 * Get the vertex specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCellType>
QuadrilateralCell< TPixelType , TCellType >::Vertex::Pointer
QuadrilateralCell< TPixelType , TCellType >
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
template <typename TPixelType, typename TCellType>
QuadrilateralCell< TPixelType , TCellType >::Edge::Pointer
QuadrilateralCell< TPixelType , TCellType >
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
template <typename TPixelType, typename TCellType>
const int
QuadrilateralCell< TPixelType , TCellType >
::m_Edges[4][2] = { {0,1}, {1,2}, {2,3}, {3,0} };


/**
 * Object factory for the boundary version of this cell type.
 */
template <typename TPixelType, typename TCellType>
QuadrilateralBoundary< TPixelType , TCellType >::Pointer
QuadrilateralBoundary< TPixelType , TCellType >
::New(void)
{
  return new Self;
}

} // namespace itk
