/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiDiagram2D.txx
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
#ifndef _itkVoronoiDiagram2D_txx
#define _itkVoronoiDiagram2D_txx


#include <algorithm>
#include "vnl/vnl_sample.h"

namespace itk{

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::
VoronoiDiagram2D()
{
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::
~VoronoiDiagram2D()
{
}

/* set the seed points, specify the number of seeds as "num" */
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

/* set the rectangle that enclosing the Voronoi Diagram. */
template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
SetBoundary(PointType vorsize)
{
  m_VorBoundary[0] = vorsize[0];
  m_VorBoundary[1] = vorsize[1];
}

template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
SetOrigin(PointType vorsize)
{
  m_VorBoundaryOrigin[0] = vorsize[0];
  m_VorBoundaryOrigin[0] = vorsize[1];
}


template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
GetPointId(int pId, PointType *answer)
{ 
  (*answer)[0] = f_VertList[pId][0];
  (*answer)[1] = f_VertList[pId][1];
}
 

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::CellPointer
VoronoiDiagram2D<TCoordRepType>::
GetCellId(CellIdentifier cellId)
{
  return(VDregions[cellId]);
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::EdgeInfo 
VoronoiDiagram2D<TCoordRepType>::
GetSeedsIDAroundEdge(VorEdge *task)
{
  EdgeInfo answer;
  answer[0]=f_LineList[task->m_LineID][0];
  answer[1]=f_LineList[task->m_LineID][1];
  return (answer);
}


template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::VorEdgeIterator 
VoronoiDiagram2D<TCoordRepType>::
EdgeBegin(void)
{
  return f_EdgeList.begin();
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::VorEdgeIterator 
VoronoiDiagram2D<TCoordRepType>::
EdgeEnd(void)
{
  return f_EdgeList.end();
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::NeighborIdIterator 
VoronoiDiagram2D<TCoordRepType>::
NeighborIdsBegin(int seeds)
{
  return m_CellNeighborsID[seeds].begin();
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::NeighborIdIterator 
VoronoiDiagram2D<TCoordRepType>::
NeighborIdsEnd(int seeds)
{
  return m_CellNeighborsID[seeds].end();
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::VertexIterator 
VoronoiDiagram2D<TCoordRepType>::
VertexBegin(void){
  return f_VertList.begin();
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::VertexIterator 
VoronoiDiagram2D<TCoordRepType>::
VertexEnd(void){
  return f_VertList.end();
}

template <typename TCoordRepType>
VoronoiDiagram2D<TCoordRepType>::PointType 
VoronoiDiagram2D<TCoordRepType>::
getSeed(int SeedID){
  PointType answer;
  answer[0]=m_Seeds[SeedID][0];
  answer[1]=m_Seeds[SeedID][1];
  return answer;
}

template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
Reset(void){
  VDregions.clear();
  VDregions.resize(m_NumberOfSeeds);
  m_CellNeighborsID.resize(m_NumberOfSeeds);

  for(unsigned int i = 0; i < m_NumberOfSeeds; i++){
    VDregions[i] = Cell::New();
	m_CellNeighborsID[i].clear();
  }  
}

template <typename TCoordRepType>
void
VoronoiDiagram2D<TCoordRepType>::
InsertCells(void){
  for(unsigned int i = 0; i < m_NumberOfSeeds; i++){
    this->SetCell(i, VDregions[i]);
  }  
}

}//end namespace

#endif
