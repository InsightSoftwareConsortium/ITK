/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHexahedronCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkHexahedronCell_txx
#define _itkHexahedronCell_txx
#include "itkHexahedronCell.h"

namespace itk
{

/**
 * Standard CellInterface:
 */
template <typename TCellInterface >
void
HexahedronCell< TCellInterface >
::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership( new Self );
  cellPointer->SetPointIds(this->GetPointIds());
}

/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TCellInterface>
unsigned int
HexahedronCell< TCellInterface >
::GetDimension(void) const
{
  return Self::CellDimension;
}

/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TCellInterface>
unsigned int
HexahedronCell< TCellInterface >
::GetNumberOfPoints(void) const
{
  return Self::NumberOfPoints;
}  

/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TCellInterface>
HexahedronCell< TCellInterface >::CellFeatureCount
HexahedronCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int dimension) const
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
template <typename TCellInterface>
bool
HexahedronCell< TCellInterface >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId, 
                                    CellAutoPointer & cellPointer )
{
  switch (dimension)
    {
    case 0: 
      {
      VertexAutoPointer vertexPointer;
      if( this->GetVertex(featureId,vertexPointer) )
        {
        TransferAutoPointer(cellPointer,vertexPointer);
        return true;
        }
      else
        {
        cellPointer.Reset();
        return false;
        }
      break;
      }
    case 1: 
      {
      EdgeAutoPointer edgePointer;
      if( this->GetEdge(featureId,edgePointer) )
        {
        TransferAutoPointer(cellPointer,edgePointer);
        return true;
        }
      else
        {
        cellPointer.Reset();
        return false;
        }
      break;
      }
    case 2: 
      {
      FaceAutoPointer facePointer;
      if( this->GetFace(featureId,facePointer) )
        {
        TransferAutoPointer(cellPointer,facePointer);
        return true;
        }
      else
        {
        cellPointer.Reset();
        return false;
        }
      break;
      }
    default: 
      {
      cellPointer.Reset();
      return false;
      }
    }
    return false;
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */
template <typename TCellInterface>
void
HexahedronCell< TCellInterface >
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
template <typename TCellInterface>
void
HexahedronCell< TCellInterface >
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
template <typename TCellInterface>
void
HexahedronCell< TCellInterface >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}

/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
HexahedronCell< TCellInterface >::PointIdIterator
HexahedronCell< TCellInterface >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
HexahedronCell< TCellInterface >::PointIdConstIterator
HexahedronCell< TCellInterface >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
HexahedronCell< TCellInterface >::PointIdIterator
HexahedronCell< TCellInterface >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints];
}

/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
HexahedronCell< TCellInterface >::PointIdConstIterator
HexahedronCell< TCellInterface >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints];
}

/**
 * Hexahedron-specific:
 * Get the number of vertices defining the hexahedron.
 */
template <typename TCellInterface>
HexahedronCell< TCellInterface >::CellFeatureCount
HexahedronCell< TCellInterface >
::GetNumberOfVertices(void) const
{
  return Self::NumberOfVertices;
}

/**
 * Hexahedron-specific:
 * Get the number of edges defined for the hexahedron.
 */
template <typename TCellInterface>
HexahedronCell< TCellInterface >::CellFeatureCount
HexahedronCell< TCellInterface >
::GetNumberOfEdges(void) const
{
  return Self::NumberOfEdges;
}

/**
 * Hexahedron-specific:
 * Get the number of faces defined for the hexahedron.
 */
template <typename TCellInterface>
HexahedronCell< TCellInterface >::CellFeatureCount
HexahedronCell< TCellInterface >
::GetNumberOfFaces(void) const
{
  return Self::NumberOfFaces;
}

/**
 * Hexahedron-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TCellInterface>
bool
HexahedronCell< TCellInterface >
::GetVertex(CellFeatureIdentifier vertexId,VertexAutoPointer & vertexPointer )
{
  VertexType * vert = new VertexType;
  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership( vert );
  return true;  
}

/**
 * Hexahedron-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template <typename TCellInterface>
bool
HexahedronCell< TCellInterface >
::GetEdge(CellFeatureIdentifier edgeId, EdgeAutoPointer & edgePointer )
{
  EdgeType * edge = new EdgeType;
  for(int i=0; i < EdgeType::NumberOfPoints; ++i)
    {
    edge->SetPointId(i, m_PointIds[ m_Edges[edgeId][i] ]);
    }
  edgePointer.TakeOwnership( edge ); 
  return true;
}


/**
 * Hexahedron-specific:
 * Get the face specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfFaces()-1.
 */
template <typename TCellInterface>
bool
HexahedronCell< TCellInterface >
::GetFace(CellFeatureIdentifier faceId, FaceAutoPointer & facePointer )
{
  FaceType * face = new FaceType;
  for(unsigned int i=0; i < FaceType::NumberOfPoints; ++i)
    {
    face->SetPointId(i, m_PointIds[ m_Faces[faceId][i] ]);
    }
  facePointer.TakeOwnership( face ); 
  return true;
}

} // end namespace itk

#endif
