/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiDiagram2D.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVoronoiDiagram2D_txx
#define _itkVoronoiDiagram2D_txx
#include "itkVoronoiDiagram2D.h"


#include <algorithm>
#include "vnl/vnl_sample.h"

namespace itk{

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::
VoronoiDiagram2D()
{
  m_NumberOfSeeds = 0;
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::
~VoronoiDiagram2D()
{
}

template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number Of Seeds: " 
     << m_NumberOfSeeds << std::endl;
}


/* Set the seed points, specify the number of seeds as "num". */
template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
SetSeeds(int num,  SeedsIterator begin)
{
  m_Seeds.clear();
  SeedsIterator ii(begin);
  for(int i = 0; i < num; ++i){
    m_Seeds.push_back(*ii++);
  }
  m_NumberOfSeeds = num;
}

/* Set the rectangle that encloses the Voronoi Diagram. */
template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
SetBoundary(PointType vorsize)
{
  m_VoronoiBoundary[0] = vorsize[0];
  m_VoronoiBoundary[1] = vorsize[1];
}

template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
SetOrigin(PointType vorsize)
{
  m_VoronoiBoundaryOrigin[0] = vorsize[0];
  m_VoronoiBoundaryOrigin[0] = vorsize[1];
}


template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
GetPoint(int pId, PointType *answer)
{ 
  (*answer)[0] = f_VertexList[pId][0];
  (*answer)[1] = f_VertexList[pId][1];
}
 

template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
GetCellId(CellIdentifier cellId, CellAutoPointer & cellPtr )
{
  cellPtr.TakeNoOwnership( m_VoronoiRegions[cellId] );
}

template <typename TCoordRepType>
typename VoronoiDiagram2D<TCoordRepType>::EdgeInfo 
VoronoiDiagram2D<TCoordRepType>::
GetSeedsIDAroundEdge(VoronoiEdge *task)
{
  EdgeInfo answer;
  answer[0]=f_LineList[task->m_LineID][0];
  answer[1]=f_LineList[task->m_LineID][1];
  return (answer);
}


template <typename TCoordRepType>
typename VoronoiDiagram2D<TCoordRepType>::VoronoiEdgeIterator 
VoronoiDiagram2D<TCoordRepType>::
EdgeBegin(void)
{
  return f_EdgeList.begin();
}

template <typename TCoordRepType>
typename VoronoiDiagram2D<TCoordRepType>::VoronoiEdgeIterator 
VoronoiDiagram2D<TCoordRepType>::
EdgeEnd(void)
{
  return f_EdgeList.end();
}

template <typename TCoordRepType>
typename VoronoiDiagram2D<TCoordRepType>::NeighborIdIterator 
VoronoiDiagram2D<TCoordRepType>::
NeighborIdsBegin(int seeds)
{
  return m_CellNeighborsID[seeds].begin();
}

template <typename TCoordRepType>
typename VoronoiDiagram2D<TCoordRepType>::NeighborIdIterator 
VoronoiDiagram2D<TCoordRepType>::
NeighborIdsEnd(int seeds)
{
  return m_CellNeighborsID[seeds].end();
}

template <typename TCoordRepType>
typename VoronoiDiagram2D<TCoordRepType>::VertexIterator 
VoronoiDiagram2D<TCoordRepType>::
VertexBegin(void)
{
  return f_VertexList.begin();
}

template <typename TCoordRepType>
typename VoronoiDiagram2D<TCoordRepType>::VertexIterator 
VoronoiDiagram2D<TCoordRepType>::
VertexEnd(void)
{
  return f_VertexList.end();
}

template <typename TCoordRepType>
typename VoronoiDiagram2D<TCoordRepType>::PointType 
VoronoiDiagram2D<TCoordRepType>::
GetSeed(int SeedID)
{
  PointType answer;
  answer[0]=m_Seeds[SeedID][0];
  answer[1]=m_Seeds[SeedID][1];
  return answer;
}

template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
Reset(void)
{
  m_VoronoiRegions.clear();
  m_VoronoiRegions.resize(m_NumberOfSeeds);
  m_CellNeighborsID.resize(m_NumberOfSeeds);

  for(unsigned int i = 0; i < m_NumberOfSeeds; i++)
    {
    m_VoronoiRegions[i] = new CellType;
    m_CellNeighborsID[i].clear();
    }  
}

template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
InsertCells(void)
{
  genericCellPointer cellPtr;
  for(unsigned int i = 0; i < m_NumberOfSeeds; i++)
    {
    cellPtr.TakeOwnership( m_VoronoiRegions[i] );
    this->SetCell(i, cellPtr );
    }  
}

}//end namespace

#endif
