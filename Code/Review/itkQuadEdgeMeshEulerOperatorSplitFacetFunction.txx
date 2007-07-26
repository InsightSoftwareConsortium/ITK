// -------------------------------------------------------------------------
// itkQuadEdgeMeshEulerOperatorSplitFacetFunction.txx
// $Revision: 1.1 $
// $Author: hanfei $
// $Name:  $
// $Date: 2007-07-26 06:30:26 $
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
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
#ifndef __itkQuadEdgeMeshEulerOperatorSplitFacetFunction_txx
#define __itkQuadEdgeMeshEulerOperatorSplitFacetFunction_txx

namespace itk
{

template < class TMesh, class TQEType >
typename QuadEdgeMeshEulerOperatorSplitFacetFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorSplitFacetFunction< TMesh, TQEType >::
Evaluate( QEType* h, QEType* g )
{
  //
  //  g->Dest() ---<----- X                    destPid  --------- X
  //         /     g       \                         / \           \
  //        /               \                   dLnext  \           \
  //       /                 \                     /     \           \
  //      /                   \                   v       ^           \
  //     /      g->Left()      \                 /         \           \
  //    X           =           X               X        newEdge        X
  //     \      h->Left()      /                 \           \         /
  //      \                   /                   \           \       ^
  //       \                 /                     \           \     /
  //        \               /                       \           \ oLnext
  //         \       h     /                         \           \ /
  //          X ----->--- h->Dest()                   X --------- orgPid
  //

  if( !this->m_Mesh )
    {
    itkDebugMacro( "No mesh present." );
    return( (QEType*) 0 );
    }

  if( h->GetLeft( ) != g->GetLeft( ) )
    {
    itkDebugMacro( "The edges are not around the same face." );
    return( (QEType*) 0 );
    }

  if( h == g )
    {
    itkDebugMacro( "Provided edges should be different." );
    return( (QEType*) 0 );
    }
    
  if (  ( h->GetLnext( ) == g )
     || ( g->GetLnext( ) == h ) )
    {
    itkDebugMacro( "Provided edges should NOT be consecutive." );
    return( (QEType*) 0 );
    }   
   
  typedef typename MeshType::VertexRefType   VertexRefType;
  typedef typename MeshType::EdgeCellType    EdgeCellType;

  this->m_Mesh->DeleteFace( h->GetLeft( ) );
  VertexRefType orgPid  = h->GetDestination( );
  VertexRefType destPid = g->GetDestination( );

  // Create an new isolated edge and set it's geometry:
  EdgeCellType* newEdge = new EdgeCellType;
  QEType* newEdgeGeom = newEdge->GetQEGeom( );
   
  // see the code of e.g. AddFace
  newEdgeGeom->SetOrigin(  orgPid );
  newEdgeGeom->SetDestination( destPid );

  // Insert newEdge at Org
  QEType* oLnext = h->GetLnext( );
  oLnext->InsertAfterNextBorderEdgeWithUnsetLeft( newEdgeGeom );
  // Insert newEdge at Dest
  QEType* dLnext = g->GetLnext( );
  dLnext->InsertAfterNextBorderEdgeWithUnsetLeft( newEdgeGeom->GetSym( ) );

  // Add the new edge to the container
  this->m_Mesh->PushOnContainer( newEdge );

  // Build two new faces
  this->m_Mesh->AddFace( h );
  this->m_Mesh->AddFace( g );
  this->m_Mesh->Modified( );
  return( newEdgeGeom );
}

}

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitFacetFunction.txx
