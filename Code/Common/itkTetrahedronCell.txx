/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTetrahedronCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkTetrahedronCell.h"


/**
 *
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::Pointer
itkTetrahedronCell< TPixelType , TCellType >
::New(void)
{
  return new Self;
}


/**
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellType>
int
itkTetrahedronCell< TPixelType , TCellType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::CellFeatureCount
itkTetrahedronCell< TPixelType , TCellType >
::GetNumberOfBoundaryFeatures(int dimension)
{
  switch (dimension)
    {
    case 0: return GetNumberOfVertices();
    case 1: return GetNumberOfEdges();
    case 2: return GetNumberOfFaces();
    default: return 0;
    }
}


/**
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::Cell::Pointer
itkTetrahedronCell< TPixelType , TCellType >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId)
{
  switch (dimension)
    {
    case 0: return Cell::Pointer(GetCellVertex(featureId));
    case 1: return Cell::Pointer(GetCellEdge(featureId));
    case 2: return Cell::Pointer(GetCellFace(featureId));
    default: return Cell::Pointer(NULL);
    }
}


/**
 * Standard itkCell API:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TCellType>
void
itkTetrahedronCell< TPixelType , TCellType >
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
itkTetrahedronCell< TPixelType , TCellType >
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
itkTetrahedronCell< TPixelType , TCellType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Provide iterator begin and end for the point identifier array.  These
 * are just pointers to the beginning and one past the end.
 */
template <typename TPixelType, typename TCelltype>
itkTetrahedronCell< TPixelType , TCelltype >::PointIterator
itkTetrahedronCell< TPixelType , TCelltype >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
itkTetrahedronCell< TPixelType , TCelltype >::PointConstIterator
itkTetrahedronCell< TPixelType , TCelltype >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
itkTetrahedronCell< TPixelType , TCelltype >::PointIterator
itkTetrahedronCell< TPixelType , TCelltype >
::PointIdsEnd(void)
{
  return &m_PointIds[NumberOfPoints];
}

template <typename TPixelType, typename TCelltype>
itkTetrahedronCell< TPixelType , TCelltype >::PointConstIterator
itkTetrahedronCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[NumberOfPoints];
}


/**
 * Tetrahedron-specific:
 * Get the number of vertices defining the tetrahedron.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::CellFeatureCount
itkTetrahedronCell< TPixelType , TCellType >
::GetNumberOfVertices(void)
{
  return NumberOfVertices;
}


/**
 * Tetrahedron-specific:
 * Get the number of edges defined for the tetrahedron.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::CellFeatureCount
itkTetrahedronCell< TPixelType , TCellType >
::GetNumberOfEdges(void)
{
  return NumberOfEdges;
}


/**
 * Tetrahedron-specific:
 * Get the number of faces defined for the tetrahedron.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::CellFeatureCount
itkTetrahedronCell< TPixelType , TCellType >
::GetNumberOfFaces(void)
{
  return NumberOfFaces;
}


/**
 * Tetrahedron-specific:
 * Get the vertex specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::Vertex::Pointer
itkTetrahedronCell< TPixelType , TCellType >
::GetCellVertex(CellFeatureIdentifier vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;
}


/**
 * Tetrahedron-specific:
 * Get the edge specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::Edge::Pointer
itkTetrahedronCell< TPixelType , TCellType >
::GetCellEdge(CellFeatureIdentifier edgeId)
{
  Edge::Pointer edge(Edge::New());

  for(int i=0; i < Edge::NumberOfPoints; ++i)
    edge->SetCellPoint(i, m_PointIds[ m_Edges[edgeId][i] ]);
  
  return edge;
}


/**
 * Tetrahedron-specific:
 * Get the face specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronCell< TPixelType , TCellType >::Face::Pointer
itkTetrahedronCell< TPixelType , TCellType >
::GetCellFace(CellFeatureIdentifier faceId)
{
  Face::Pointer face(Face::New());
  
  for(int i=0; i < Face::NumberOfPoints; ++i)
    face->SetCellPoint(i, m_PointIds[ m_Faces[faceId][i] ]);
  
  return face;
}


/**
 * Define the tetrahedron's topology data.
 */
template <typename TPixelType, typename TCellType>
const int
itkTetrahedronCell< TPixelType , TCellType >
::m_Edges[6][2] = { {0,1}, {1,2}, {2,0}, {0,3}, {1,3}, {2,3} };

template <typename TPixelType, typename TCellType>
const int
itkTetrahedronCell< TPixelType , TCellType >
::m_Faces[4][3] = { {0,1,3}, {1,2,3}, {2,0,3}, {0,2,1} };


/**
 * Object factory for the boundary version of this cell type.
 */
template <typename TPixelType, typename TCellType>
itkTetrahedronBoundary< TPixelType , TCellType >::Pointer
itkTetrahedronBoundary< TPixelType , TCellType >
::New(void)
{
  return new Self;
}

