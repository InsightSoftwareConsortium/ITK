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
#ifndef __itkQuadEdgeMeshBoundaryEdgesMeshFunction_hxx
#define __itkQuadEdgeMeshBoundaryEdgesMeshFunction_hxx

#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.h"
#include "itkQuadEdgeMesh.h"  // Just to mark the dependance towards this class.

namespace itk
{
template< typename TMesh >
typename QuadEdgeMeshBoundaryEdgesMeshFunction< TMesh >::OutputType
QuadEdgeMeshBoundaryEdgesMeshFunction< TMesh >::Evaluate(const InputType & mesh)
const
{
  // Push on a list all the non internal edges:
  typedef typename MeshType::CellsContainerConstIterator
  CellsContainerConstIterator;
  EdgeListType boundaryList;

  CellsContainerConstIterator cellIterator = mesh.GetEdgeCells()->Begin();
  CellsContainerConstIterator cellEnd      = mesh.GetEdgeCells()->End();

  for (; cellIterator != cellEnd; ++cellIterator )
    {
    if ( EdgeCellType * cell =
           dynamic_cast< EdgeCellType * >( cellIterator.Value() ) )
      {
      QEPrimal *edge = cell->GetQEGeom();
      if ( !edge->IsInternal() )
        {
        boundaryList.push_front(edge);
        }
      }
    }

  OutputType ResultList = new EdgeListType;
  while ( !boundaryList.empty() )
    {
    // Pop the first edge of list and make sure it has no face
    // on it's left [because we want to follow the boundary with
    // GeometricalQuadEdge::Lnext()]:
    QEPrimal *bdryEdge = boundaryList.front();
    boundaryList.pop_front();
    if ( bdryEdge->IsLeftSet() )
      {
      bdryEdge = bdryEdge->GetSym();
      }
    if ( bdryEdge->IsLeftSet() )
      {
      itkWarningMacro("Entry edge has not face adjacency.");
      delete ResultList;
      return ( (OutputType)0 );
      }

    // Store this edge as representative of it's Lnext() ring i.e.
    // representative of the boundary:
    ResultList->push_back(bdryEdge);

    // Follow, with Lnext(), the boundary while removing edges
    // from boundary list:
    typename QEPrimal::IteratorGeom it = bdryEdge->BeginGeomLnext();
    for (; it != bdryEdge->EndGeomLnext(); it++ )
      {
      // Only one of the following will be effective (but we have
      // no way to know which one):
      boundaryList.remove( it.Value() );
      boundaryList.remove( it.Value()->GetSym() );
      }
    }

  return ResultList;
}
}

#endif
