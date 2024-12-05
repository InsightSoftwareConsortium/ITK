/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include <algorithm>

namespace itk
{

template <typename TCoordinate>
VoronoiDiagram2D<TCoordinate>::VoronoiDiagram2D()
{
  m_NumberOfSeeds = 0;
}

template <typename TCoordinate>
void
VoronoiDiagram2D<TCoordinate>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number Of Seeds: " << m_NumberOfSeeds << std::endl;
}


/* Set the seed points, specify the number of seeds as "num". */
template <typename TCoordinate>
void
VoronoiDiagram2D<TCoordinate>::SetSeeds(int num, SeedsIterator begin)
{
  m_Seeds.clear();
  auto ii(begin);
  for (int i = 0; i < num; ++i)
  {
    m_Seeds.push_back(*ii++);
  }
  m_NumberOfSeeds = num;
}


/* Set the rectangle that encloses the Voronoi Diagram. */
template <typename TCoordinate>
void
VoronoiDiagram2D<TCoordinate>::SetBoundary(PointType vorsize)
{
  m_VoronoiBoundary[0] = vorsize[0];
  m_VoronoiBoundary[1] = vorsize[1];
}


template <typename TCoordinate>
void
VoronoiDiagram2D<TCoordinate>::SetOrigin(PointType vorsize)
{
  m_VoronoiBoundaryOrigin[0] = vorsize[0];
  m_VoronoiBoundaryOrigin[1] = vorsize[1];
}


template <typename TCoordinate>
void
VoronoiDiagram2D<TCoordinate>::GetPoint(int pId, PointType * answer)
{
  *answer = this->m_PointsContainer->ElementAt(pId);
}


template <typename TCoordinate>
void
VoronoiDiagram2D<TCoordinate>::GetCellId(CellIdentifier cellId, CellAutoPointer & cellPtr)
{
  cellPtr.TakeNoOwnership(m_VoronoiRegions[cellId]);
}


template <typename TCoordinate>
auto
VoronoiDiagram2D<TCoordinate>::GetSeedsIDAroundEdge(VoronoiEdge * task) -> EdgeInfo
{
  EdgeInfo answer;

  answer[0] = m_LineList[task->m_LineID][0];
  answer[1] = m_LineList[task->m_LineID][1];
  return (answer);
}


template <typename TCoordinate>
auto
VoronoiDiagram2D<TCoordinate>::EdgeBegin() -> VoronoiEdgeIterator
{
  return m_EdgeList.begin();
}


template <typename TCoordinate>
auto
VoronoiDiagram2D<TCoordinate>::EdgeEnd() -> VoronoiEdgeIterator
{
  return m_EdgeList.end();
}


template <typename TCoordinate>
auto
VoronoiDiagram2D<TCoordinate>::NeighborIdsBegin(int seeds) -> NeighborIdIterator
{
  return m_CellNeighborsID[seeds].begin();
}


template <typename TCoordinate>
auto
VoronoiDiagram2D<TCoordinate>::NeighborIdsEnd(int seeds) -> NeighborIdIterator
{
  return m_CellNeighborsID[seeds].end();
}


template <typename TCoordinate>
auto
VoronoiDiagram2D<TCoordinate>::VertexBegin() -> VertexIterator
{
  return this->m_PointsContainer->Begin();
}


template <typename TCoordinate>
auto
VoronoiDiagram2D<TCoordinate>::VertexEnd() -> VertexIterator
{
  return this->m_PointsContainer->End();
}


template <typename TCoordinate>
auto
VoronoiDiagram2D<TCoordinate>::GetSeed(int SeedID) -> PointType
{
  PointType answer;

  answer[0] = m_Seeds[SeedID][0];
  answer[1] = m_Seeds[SeedID][1];
  return answer;
}


template <typename TCoordinate>
void
VoronoiDiagram2D<TCoordinate>::Reset()
{
  m_VoronoiRegions.clear();
  m_VoronoiRegions.resize(m_NumberOfSeeds);
  m_CellNeighborsID.resize(m_NumberOfSeeds);

  for (unsigned int i = 0; i < m_NumberOfSeeds; ++i)
  {
    m_VoronoiRegions[i] = new PolygonCellType;
    m_CellNeighborsID[i].clear();
  }
}


template <typename TCoordinate>
void
VoronoiDiagram2D<TCoordinate>::InsertCells()
{
  genericCellPointer cellPtr;

  for (unsigned int i = 0; i < m_NumberOfSeeds; ++i)
  {
    cellPtr.TakeOwnership(m_VoronoiRegions[i]);
    this->SetCell(i, cellPtr);
  }
}

} // end namespace itk

#endif
