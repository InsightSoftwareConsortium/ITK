/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHexahedronCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkHexahedronCell.h"

namespace itk
{

/**
 *
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::Pointer
HexahedronCell< TPixelType , TCelltype >
::New(void)
{
  return new Self;
}


/**
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCelltype>
int
HexahedronCell< TPixelType , TCelltype >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::CellFeatureCount
HexahedronCell< TPixelType , TCelltype >
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
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::Cell::Pointer
HexahedronCell< TPixelType , TCelltype >
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
template <typename TPixelType, typename TCelltype>
void
HexahedronCell< TPixelType , TCelltype >
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
template <typename TPixelType, typename TCelltype>
void
HexahedronCell< TPixelType , TCelltype >
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
template <typename TPixelType, typename TCelltype>
void
HexahedronCell< TPixelType , TCelltype >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Provide iterator begin and end for the point identifier array.  These
 * are just pointers to the beginning and one past the end.
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::PointIterator
HexahedronCell< TPixelType , TCelltype >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::PointConstIterator
HexahedronCell< TPixelType , TCelltype >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::PointIterator
HexahedronCell< TPixelType , TCelltype >
::PointIdsEnd(void)
{
  return &m_PointIds[NumberOfPoints];
}

template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::PointConstIterator
HexahedronCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[NumberOfPoints];
}


/**
 * Hexahedron-specific:
 * Get the number of vertices defining the hexahedron.
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::CellFeatureCount
HexahedronCell< TPixelType , TCelltype >
::GetNumberOfVertices(void)
{
  return NumberOfVertices;
}


/**
 * Hexahedron-specific:
 * Get the number of edges defined for the hexahedron.
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::CellFeatureCount
HexahedronCell< TPixelType , TCelltype >
::GetNumberOfEdges(void)
{
  return NumberOfEdges;
}


/**
 * Hexahedron-specific:
 * Get the number of faces defined for the hexahedron.
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::CellFeatureCount
HexahedronCell< TPixelType , TCelltype >
::GetNumberOfFaces(void)
{
  return NumberOfFaces;
}


/**
 * Hexahedron-specific:
 * Get the vertex specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::Vertex::Pointer
HexahedronCell< TPixelType , TCelltype >
::GetCellVertex(CellFeatureIdentifier vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;
}


/**
 * Hexahedron-specific:
 * Get the edge specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::Edge::Pointer
HexahedronCell< TPixelType , TCelltype >
::GetCellEdge(CellFeatureIdentifier edgeId)
{
  Edge::Pointer edge(Edge::New());

  for(int i=0; i < Edge::NumberOfPoints; ++i)
    edge->SetCellPoint(i, m_PointIds[ m_Edges[edgeId][i] ]);
  
  return edge;
}


/**
 * Hexahedron-specific:
 * Get the face specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCelltype>
HexahedronCell< TPixelType , TCelltype >::Face::Pointer
HexahedronCell< TPixelType , TCelltype >
::GetCellFace(CellFeatureIdentifier faceId)
{
  Face::Pointer face(Face::New());
  
  for(int i=0; i < Face::NumberOfPoints; ++i)
    face->SetCellPoint(i, m_PointIds[ m_Faces[faceId][i] ]);
  
  return face;
}


/**
 * Define the hexahedron's topology data.
 */
template <typename TPixelType, typename TCelltype>
const int
HexahedronCell< TPixelType , TCelltype >
::m_Edges[12][2] = { {0,1}, {1,2}, {3,2}, {0,3}, 
                     {4,5}, {5,6}, {7,6}, {4,7},
                     {0,4}, {1,5}, {3,7}, {2,6} };

template <typename TPixelType, typename TCelltype>
const int
HexahedronCell< TPixelType , TCelltype >
::m_Faces[6][4] = { {0,4,7,3}, {1,2,6,5},
                    {0,1,5,4}, {3,7,6,2},
                    {0,3,2,1}, {4,5,6,7} };

/**
 * Object factory for the boundary version of this cell type.
 */
template <typename TPixelType, typename TCellType>
HexahedronBoundary< TPixelType , TCellType >::Pointer
HexahedronBoundary< TPixelType , TCellType >
::New(void)
{
  return new Self;
}

} // namespace itk
