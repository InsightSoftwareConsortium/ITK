/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkTriangleCell_txx
#define _itkTriangleCell_txx
#include "itkTriangleCell.h"

namespace itk
{

/**
 * Standard CellInterface:
 */
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::CellPointer
TriangleCell< TPixelType , TCellTraits >
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
TriangleCell< TPixelType , TCellTraits >
::GetDimension(void) const
{
  return Self::CellDimension;
}


/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TPixelType, typename TCellTraits>
int
TriangleCell< TPixelType , TCellTraits >
::GetNumberOfPoints(void) const
{
  return Self::NumberOfPoints;
}  


/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::CellFeatureCount
TriangleCell< TPixelType , TCellTraits >
::GetNumberOfBoundaryFeatures(int dimension) const
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
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::CellPointer
TriangleCell< TPixelType , TCellTraits >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId)
{
  switch (dimension)
    {
    case 0: return CellPointer(GetVertex(featureId));
    case 1: return CellPointer(GetEdge(featureId));
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
TriangleCell< TPixelType , TCellTraits >
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
TriangleCell< TPixelType , TCellTraits >
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
TriangleCell< TPixelType , TCellTraits >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::PointIdIterator
TriangleCell< TPixelType , TCellTraits >
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
TriangleCell< TPixelType , TCellTraits >::PointIdConstIterator
TriangleCell< TPixelType , TCellTraits >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::PointIdIterator
TriangleCell< TPixelType , TCellTraits >
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
TriangleCell< TPixelType , TCellTraits >::PointIdConstIterator
TriangleCell< TPixelType , TCellTraits >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Triangle-specific:
 * Get the number of vertices defining the triangle.
 */
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::CellFeatureCount
TriangleCell< TPixelType , TCellTraits >
::GetNumberOfVertices(void)
{
  return Self::NumberOfVertices;
}


/**
 * Triangle-specific:
 * Get the number of edges defined for the triangle.
 */
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::CellFeatureCount
TriangleCell< TPixelType , TCellTraits >
::GetNumberOfEdges(void)
{
  return Self::NumberOfEdges;
}

/**
 * Triangle-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::VertexPointer
TriangleCell< TPixelType , TCellTraits >
::GetVertex(CellFeatureIdentifier vertexId)
{
  VertexPointer vert(Vertex::New());
  vert->SetPointId(0, m_PointIds[vertexId]);
  
  return vert;
}

/**
 * Triangle-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template <typename TPixelType, typename TCellTraits>
TriangleCell< TPixelType , TCellTraits >::EdgePointer
TriangleCell< TPixelType , TCellTraits >
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
 * The triangle's topology data: Edges
 */
template <typename TPixelType, typename TCellTraits>
const int
TriangleCell< TPixelType , TCellTraits >
::m_Edges[3][2] = { {0,1}, {1,2}, {2,0} };

} // end namespace itk

#endif
