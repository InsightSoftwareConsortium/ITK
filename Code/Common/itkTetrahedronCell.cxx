/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTetrahedronCell.cxx
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
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::Pointer
itkTetrahedronCell< TPixelType , TMeshType >
::New(void)
{
  return new Self;
}


/**
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TMeshType>
int
itkTetrahedronCell< TPixelType , TMeshType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::CellFeatureCount
itkTetrahedronCell< TPixelType , TMeshType >
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
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::Cell::Pointer
itkTetrahedronCell< TPixelType , TMeshType >
::GetBoundaryFeature(int dimension, CellFeatureId featureId, Mesh* mesh)
{
  switch (dimension)
    {
    case 0: return Cell::Pointer(GetCellVertex(featureId, mesh));
    case 1: return Cell::Pointer(GetCellEdge(featureId, mesh));
    case 2: return Cell::Pointer(GetCellFace(featureId, mesh));
    default: return Cell::Pointer(NULL);
    }
}


/**
 * Standard itkCell API:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TMeshType>
void
itkTetrahedronCell< TPixelType , TMeshType >
::SetCellPoints(PointIdentifier *ptList)
{
  for(int i=0; i < NumberOfPoints ; ++i)
    m_PointIds[i] = ptList[i];
}


/**
 * Tetrahedron-specific:
 * Get the number of vertices defining the tetrahedron.
 */
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::CellFeatureCount
itkTetrahedronCell< TPixelType , TMeshType >
::GetNumberOfVertices(void)
{
  return NumberOfVertices;
}


/**
 * Tetrahedron-specific:
 * Get the number of edges defined for the tetrahedron.
 */
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::CellFeatureCount
itkTetrahedronCell< TPixelType , TMeshType >
::GetNumberOfEdges(void)
{
  return NumberOfEdges;
}


/**
 * Tetrahedron-specific:
 * Get the number of faces defined for the tetrahedron.
 */
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::CellFeatureCount
itkTetrahedronCell< TPixelType , TMeshType >
::GetNumberOfFaces(void)
{
  return NumberOfFaces;
}


/**
 * Tetrahedron-specific:
 * Get the vertex specified by the given cell feature Id.
 */
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::Vertex::Pointer
itkTetrahedronCell< TPixelType , TMeshType >
::GetCellVertex(CellFeatureId vertexId, Mesh*)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;
}


/**
 * Tetrahedron-specific:
 * Get the edge specified by the given cell feature Id.
 */
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::Edge::Pointer
itkTetrahedronCell< TPixelType , TMeshType >
::GetCellEdge(CellFeatureId edgeId, Mesh*)
{
  Edge::Pointer edge(Edge::New());

  edge->SetCellPoint(0, m_PointIds[ m_Edges[edgeId][0] ]);
  edge->SetCellPoint(1, m_PointIds[ m_Edges[edgeId][1] ]);
  
  return edge;
}


/**
 * Tetrahedron-specific:
 * Get the face specified by the given cell feature Id.
 */
template <typename TPixelType, typename TMeshType>
itkTetrahedronCell< TPixelType , TMeshType >::Face::Pointer
itkTetrahedronCell< TPixelType , TMeshType >
::GetCellFace(CellFeatureId faceId, Mesh*)
{
  Face::Pointer face(Face::New());
  
  face->SetCellPoint(0, m_PointIds[ m_Faces[faceId][0] ]);
  face->SetCellPoint(1, m_PointIds[ m_Faces[faceId][1] ]);
  face->SetCellPoint(2, m_PointIds[ m_Faces[faceId][2] ]);  
  
  return face;
}


/**
 * Define the tetrahedron's topology data.
 */
template <typename TPixelType, typename TMeshType>
const int
itkTetrahedronCell< TPixelType , TMeshType >
::m_Edges[6][2] = { {0,1}, {1,2}, {2,0}, {0,3}, {1,3}, {2,3} };

template <typename TPixelType, typename TMeshType>
const int
itkTetrahedronCell< TPixelType , TMeshType >
::m_Faces[4][3] = { {0,1,3}, {1,2,3}, {2,0,3}, {0,2,1} };
