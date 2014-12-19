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
#ifndef itkQuadEdgeMeshBoundaryEdgesMeshFunction_hxx
#define itkQuadEdgeMeshBoundaryEdgesMeshFunction_hxx

#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.h"
#include "itkQuadEdgeMesh.h"  // Just to mark the dependence towards this class.

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
  std::set< QEPrimal* > boundaryList;

  CellsContainerConstIterator cellIterator = mesh.GetEdgeCells()->Begin();
  CellsContainerConstIterator cellEnd      = mesh.GetEdgeCells()->End();

  while( cellIterator != cellEnd )
    {
    if ( EdgeCellType * cell =
           dynamic_cast< EdgeCellType * >( cellIterator.Value() ) )
      {
      QEPrimal *edge = cell->GetQEGeom();
      if ( !edge->IsInternal() )
        {
        boundaryList.insert(edge);
        }
      }
    ++cellIterator;
    }

  OutputType ResultList = new EdgeListType;
  while ( !boundaryList.empty() )
    {
    // Pop the first edge of list and make sure it has no face
    // on it's left [because we want to follow the boundary with
    // GeometricalQuadEdge::Lnext()]:
    typename std::set< QEPrimal* >::iterator b = boundaryList.begin();
    QEPrimal *bdryEdge = *b;
    boundaryList.erase( b );
    if ( bdryEdge->IsLeftSet() )
      {
      bdryEdge = bdryEdge->GetSym();
      }
    if ( bdryEdge->IsLeftSet() )
      {
      itkWarningMacro("Entry edge has not face adjacency.");
      delete ResultList;
      return ( (OutputType)ITK_NULLPTR );
      }

    // Store this edge as representative of it's Lnext() ring i.e.
    // representative of the boundary:
    ResultList->push_back(bdryEdge);

    // Follow, with Lnext(), the boundary while removing edges
    // from boundary list:
    typename QEPrimal::IteratorGeom bIt   = bdryEdge->BeginGeomLnext();
    typename QEPrimal::IteratorGeom bEnd  = bdryEdge->EndGeomLnext();

    while( bIt != bEnd )
      {
      // Only one of the following will be effective (but we have
      // no way to know which one):
      b = boundaryList.find( bIt.Value() );

      if( b != boundaryList.end() )
        {
        boundaryList.erase( b );
        }

      b = boundaryList.find( bIt.Value()->GetSym() );

      if( b != boundaryList.end() )
        {
        boundaryList.erase( b );
        }

      ++bIt;
      }
    }

  return ResultList;
}
}

#endif
