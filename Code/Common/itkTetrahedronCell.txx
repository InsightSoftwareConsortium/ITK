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
#ifndef _itkTetrahedronCell_txx
#define _itkTetrahedronCell_txx
// #include "itkTetrahedronCell.h"

namespace itk
{

/**
 * Standard CellInterface:
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::CellPointer
TetrahedronCell< TPixelType , TCellTraits >
::MakeCopy(void)
{
  CellPointer newCell(Self::New());
  newCell->SetPointIds(this->GetPointIds());
  return newCell;
}


/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellTraits>
int
TetrahedronCell< TPixelType , TCellTraits >
::GetDimension(void)
{
  return Self::CellDimension;
}


/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TPixelType, typename TCellTraits>
int
TetrahedronCell< TPixelType , TCellTraits >
::GetNumberOfPoints(void)
{
  return Self::NumberOfPoints;
}  


/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::CellFeatureCount
TetrahedronCell< TPixelType , TCellTraits >
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
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::CellPointer
TetrahedronCell< TPixelType , TCellTraits >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId)
{
  switch (dimension)
    {
    case 0: return CellPointer(GetVertex(featureId));
    case 1: return CellPointer(GetEdge(featureId));
    case 2: return CellPointer(GetFace(featureId));
    default: return CellPointer(NULL);
    }
}


/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */
template <typename TPixelType, typename TCellTraits>
void
TetrahedronCell< TPixelType , TCellTraits >
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
template <typename TPixelType, typename TCellTraits>
void
TetrahedronCell< TPixelType , TCellTraits >
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
template <typename TPixelType, typename TCellTraits>
void
TetrahedronCell< TPixelType , TCellTraits >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::PointIdIterator
TetrahedronCell< TPixelType , TCellTraits >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::PointIdConstIterator
TetrahedronCell< TPixelType , TCellTraits >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::PointIdIterator
TetrahedronCell< TPixelType , TCellTraits >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::PointIdConstIterator
TetrahedronCell< TPixelType , TCellTraits >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Tetrahedron-specific:
 * Get the number of vertices defining the tetrahedron.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::CellFeatureCount
TetrahedronCell< TPixelType , TCellTraits >
::GetNumberOfVertices(void)
{
  return Self::NumberOfVertices;
}


/**
 * Tetrahedron-specific:
 * Get the number of edges defined for the tetrahedron.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::CellFeatureCount
TetrahedronCell< TPixelType , TCellTraits >
::GetNumberOfEdges(void)
{
  return Self::NumberOfEdges;
}


/**
 * Tetrahedron-specific:
 * Get the number of faces defined for the tetrahedron.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::CellFeatureCount
TetrahedronCell< TPixelType , TCellTraits >
::GetNumberOfFaces(void)
{
  return Self::NumberOfFaces;
}


/**
 * Tetrahedron-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::VertexPointer
TetrahedronCell< TPixelType , TCellTraits >
::GetVertex(CellFeatureIdentifier vertexId)
{
  VertexPointer vert(Vertex::New());
  vert->SetPointId(0, m_PointIds[vertexId]);
  
  return vert;
}


/**
 * Tetrahedron-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::EdgePointer
TetrahedronCell< TPixelType , TCellTraits >
::GetEdge(CellFeatureIdentifier edgeId)
{
  EdgePointer edge(Edge::New());

  for(int i=0; i < Edge::NumberOfPoints; ++i)
    {
    edge->SetPointId(i, m_PointIds[ m_Edges[edgeId][i] ]);
    }
  
  return edge;
}


/**
 * Tetrahedron-specific:
 * Get the face specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfFaces()-1.
 */
template <typename TPixelType, typename TCellTraits>
TetrahedronCell< TPixelType , TCellTraits >::FacePointer
TetrahedronCell< TPixelType , TCellTraits >
::GetFace(CellFeatureIdentifier faceId)
{
  FacePointer face(Face::New());
  
  for(int i=0; i < Face::NumberOfPoints; ++i)
    {
    face->SetPointId(i, m_PointIds[ m_Faces[faceId][i] ]);
    }
  
  return face;
}


/**
 * The tetrahedron's topology data: Edges
 */
template <typename TPixelType, typename TCellTraits>
const int
TetrahedronCell< TPixelType , TCellTraits >
::m_Edges[6][2] = { {0,1}, {1,2}, {2,0}, {0,3}, {1,3}, {2,3} };

/**
 * The tetrahedron's topology data: Faces
 */
template <typename TPixelType, typename TCellTraits>
const int
TetrahedronCell< TPixelType , TCellTraits >
::m_Faces[4][3] = { {0,1,3}, {1,2,3}, {2,0,3}, {0,2,1} };


} // end namespace itk

#endif
