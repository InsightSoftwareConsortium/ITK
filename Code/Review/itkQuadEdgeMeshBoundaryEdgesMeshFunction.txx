// -------------------------------------------------------------------------
// itkQuadEdgeMeshBoundaryEdgesMeshFunction.txx
// $Revision: 1.1 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) gouaillard@creatis.insa-lyon.fr
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
#ifndef __itkQuadEdgeMeshBoundaryEdgesMeshFunction_txx
#define __itkQuadEdgeMeshBoundaryEdgesMeshFunction_txx

#include "itkQuadEdgeMesh.h"  // Just to mark the dependance towards this class.

namespace itk
{

template< typename TMesh >
  typename QuadEdgeMeshBoundaryEdgesMeshFunction< TMesh >::OutputType
  QuadEdgeMeshBoundaryEdgesMeshFunction< TMesh >::
  Evaluate( const InputType& mesh )
  const
{
   // Push on a list all the non internal edges:
   typedef typename MeshType::CellsContainerConstIterator
                                             CellsContainerConstIterator; 
   EdgeListType boundaryList;
   CellsContainerConstIterator cellIterator = mesh.GetCells()->Begin();
   CellsContainerConstIterator cellEnd      = mesh.GetCells()->End();
   while( cellIterator != cellEnd )
     {
     if( QEPrimal* edge = dynamic_cast< QEPrimal* >( cellIterator.Value()) )
       {
       if( !edge->IsInternal( ) )
         {
         boundaryList.push_front( edge );
         }
       }
       ++cellIterator;
     } 

   OutputType ResultList = new EdgeListType;
   while( ! boundaryList.empty( ) )
     {
     // Pop the first edge of list and make sure it has no face
     // on it's left [because we want to follow the boundary with
     // GeometricalQuadEdge::Lnext()]:
     QEPrimal* bdryEdge = boundaryList.front( );
     boundaryList.pop_front( );
     if( bdryEdge->IsLeftSet( ) )
       {
       bdryEdge = bdryEdge->GetSym( );
       }
     if( bdryEdge->IsLeftSet( ) )
       {
       itkWarningMacro( "Entry edge has not face adjacency." );
       delete ResultList;
       return( (OutputType)0 );
       }

     // Store this edge as representative of it's Lnext() ring i.e.
     // representative of the boundary:
     ResultList->push_back( bdryEdge );

     // Follow, with Lnext(), the boundary while removing edges
     // from boundary list:
     typename QEPrimal::IteratorGeom it = bdryEdge->BeginGeomLnext( );
     for( ; it != bdryEdge->EndGeomLnext( ); it++ )
       {
       // Only one of the following will be effective (but we have
       // no way to know which one):
       boundaryList.remove( it.Value( ) );
       boundaryList.remove( it.Value( )->GetSym( ) );
       } 
     } 

   return ResultList;
}

} 

#endif 


