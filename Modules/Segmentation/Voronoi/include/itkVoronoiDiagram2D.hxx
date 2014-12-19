/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVoronoiDiagram2D_hxx
#define itkVoronoiDiagram2D_hxx
#include "itkVoronoiDiagram2D.h"

#include <algorithm>
#include "vnl/vnl_sample.h"

namespace itk
{

template< typename TCoordRepType >
VoronoiDiagram2D< TCoordRepType >
::VoronoiDiagram2D()
{
  m_NumberOfSeeds = 0;
}


template< typename TCoordRepType >
VoronoiDiagram2D< TCoordRepType >
::~VoronoiDiagram2D()
{
}


template< typename TCoordRepType >
void
VoronoiDiagram2D< TCoordRepType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number Of Seeds: "
     << m_NumberOfSeeds << std::endl;
}


/* Set the seed points, specify the number of seeds as "num". */
template< typename TCoordRepType >
void
VoronoiDiagram2D< TCoordRepType >
::SetSeeds(int num,  SeedsIterator begin)
{
  m_Seeds.clear();
  SeedsIterator ii(begin);
  for ( int i = 0; i < num; ++i )
    {
    m_Seeds.push_back(*ii++);
    }
  m_NumberOfSeeds = num;
}


/* Set the rectangle that encloses the Voronoi Diagram. */
template< typename TCoordRepType >
void
VoronoiDiagram2D< TCoordRepType >
::SetBoundary(PointType vorsize)
{
  m_VoronoiBoundary[0] = vorsize[0];
  m_VoronoiBoundary[1] = vorsize[1];
}


template< typename TCoordRepType >
void
VoronoiDiagram2D< TCoordRepType >
::SetOrigin(PointType vorsize)
{
  m_VoronoiBoundaryOrigin[0] = vorsize[0];
  m_VoronoiBoundaryOrigin[1] = vorsize[1];
}


template< typename TCoordRepType >
void
VoronoiDiagram2D< TCoordRepType >
::GetPoint(int pId, PointType *answer)
{
  *answer = this->m_PointsContainer->ElementAt(pId);
}


template< typename TCoordRepType >
void
VoronoiDiagram2D< TCoordRepType >
::GetCellId(CellIdentifier cellId, CellAutoPointer & cellPtr)
{
  cellPtr.TakeNoOwnership(m_VoronoiRegions[cellId]);
}


template< typename TCoordRepType >
typename VoronoiDiagram2D< TCoordRepType >::EdgeInfo
VoronoiDiagram2D< TCoordRepType >
::GetSeedsIDAroundEdge(VoronoiEdge *task)
{
  EdgeInfo answer;

  answer[0] = m_LineList[task->m_LineID][0];
  answer[1] = m_LineList[task->m_LineID][1];
  return ( answer );
}


template< typename TCoordRepType >
typename VoronoiDiagram2D< TCoordRepType >::VoronoiEdgeIterator
VoronoiDiagram2D< TCoordRepType >
::EdgeBegin()
{
  return m_EdgeList.begin();
}


template< typename TCoordRepType >
typename VoronoiDiagram2D< TCoordRepType >::VoronoiEdgeIterator
VoronoiDiagram2D< TCoordRepType >
::EdgeEnd()
{
  return m_EdgeList.end();
}


template< typename TCoordRepType >
typename VoronoiDiagram2D< TCoordRepType >::NeighborIdIterator
VoronoiDiagram2D< TCoordRepType >
::NeighborIdsBegin(int seeds)
{
  return m_CellNeighborsID[seeds].begin();
}


template< typename TCoordRepType >
typename VoronoiDiagram2D< TCoordRepType >::NeighborIdIterator
VoronoiDiagram2D< TCoordRepType >
::NeighborIdsEnd(int seeds)
{
  return m_CellNeighborsID[seeds].end();
}


template< typename TCoordRepType >
typename VoronoiDiagram2D< TCoordRepType >::VertexIterator
VoronoiDiagram2D< TCoordRepType >
::VertexBegin()
{
  return this->m_PointsContainer->Begin();
}


template< typename TCoordRepType >
typename VoronoiDiagram2D< TCoordRepType >::VertexIterator
VoronoiDiagram2D< TCoordRepType >
::VertexEnd()
{
  return this->m_PointsContainer->End();
}


template< typename TCoordRepType >
typename VoronoiDiagram2D< TCoordRepType >::PointType
VoronoiDiagram2D< TCoordRepType >
::GetSeed(int SeedID)
{
  PointType answer;

  answer[0] = m_Seeds[SeedID][0];
  answer[1] = m_Seeds[SeedID][1];
  return answer;
}


template< typename TCoordRepType >
void
VoronoiDiagram2D< TCoordRepType >
::Reset()
{
  m_VoronoiRegions.clear();
  m_VoronoiRegions.resize(m_NumberOfSeeds);
  m_CellNeighborsID.resize(m_NumberOfSeeds);

  for ( unsigned int i = 0; i < m_NumberOfSeeds; i++ )
    {
    m_VoronoiRegions[i] = new PolygonCellType;
    m_CellNeighborsID[i].clear();
    }
}


template< typename TCoordRepType >
void
VoronoiDiagram2D< TCoordRepType >
::InsertCells()
{
  genericCellPointer cellPtr;

  for ( unsigned int i = 0; i < m_NumberOfSeeds; i++ )
    {
    cellPtr.TakeOwnership(m_VoronoiRegions[i]);
    this->SetCell(i, cellPtr);
    }
}

} // end namespace itk

#endif
