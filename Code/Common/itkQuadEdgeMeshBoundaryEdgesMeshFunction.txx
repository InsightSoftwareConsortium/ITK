/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshBoundaryEdgesMeshFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshBoundaryEdgesMeshFunction_txx
#define __itkQuadEdgeMeshBoundaryEdgesMeshFunction_txx

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
