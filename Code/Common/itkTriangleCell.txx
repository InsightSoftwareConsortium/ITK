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
 */
template <typename TPixelType, typename TCelltype>
TriangleCell< TPixelType , TCelltype >::Cell::Pointer
TriangleCell< TPixelType , TCelltype >
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
TriangleCell< TPixelType , TCellType >
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
TriangleCell< TPixelType , TCelltype >
::GetNumberOfPoints(void)
{
  return Self::NumberOfPoints;
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
    case 0: return Cell::Pointer(GetVertex(featureId));
    case 1: return Cell::Pointer(GetEdge(featureId));
    default: return Cell::Pointer(NULL);
    }
}


/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */
template <typename TPixelType, typename TCelltype>
void
TriangleCell< TPixelType , TCelltype >
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
TriangleCell< TPixelType , TCelltype >
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
TriangleCell< TPixelType , TCelltype >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCelltype>
TriangleCell< TPixelType , TCelltype >::PointIdIterator
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
TriangleCell< TPixelType , TCelltype >::PointIdConstIterator
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
TriangleCell< TPixelType , TCelltype >::PointIdIterator
TriangleCell< TPixelType , TCelltype >
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
TriangleCell< TPixelType , TCelltype >::PointIdConstIterator
TriangleCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints];
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
  return Self::NumberOfVertices;
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
  return Self::NumberOfEdges;
}

/**
 * Triangle-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TPixelType, typename TCellType>
TriangleCell< TPixelType , TCellType >::Vertex::Pointer
TriangleCell< TPixelType , TCellType >
::GetVertex(CellFeatureIdentifier vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetPointId(0, m_PointIds[vertexId]);
  
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
::GetEdge(CellFeatureIdentifier edgeId)
{
  Edge::Pointer edge(Edge::New());
  
  for(int i=0; i < Edge::NumberOfPoints; ++i)
    {
    edge->SetPointId(i, m_PointIds[ m_Edges[edgeId][i] ]);
    }
  
  return edge;
}


/**
 * The triangle's topology data: Edges
 */
template <typename TPixelType, typename TCellType>
const int
TriangleCell< TPixelType , TCellType >
::m_Edges[3][2] = { {0,1}, {1,2}, {2,0} };

} // end namespace itk
