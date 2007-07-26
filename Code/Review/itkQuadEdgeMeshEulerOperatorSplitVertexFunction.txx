// -------------------------------------------------------------------------
// itkQuadEdgeMeshEulerOperatorSplitVertexFunction.txx
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
#ifndef __itkQuadEdgeMeshEulerOperatorSplitVertexFunction_txx
#define __itkQuadEdgeMeshEulerOperatorSplitVertexFunction_txx

namespace itk
{

template < class TMesh, class TQEType >
  typename QuadEdgeMeshEulerOperatorSplitVertexFunction< TMesh, TQEType >::OutputType
  QuadEdgeMeshEulerOperatorSplitVertexFunction< TMesh, TQEType >::
  Evaluate( QEType* h, QEType* g )
{
  if ( ( h == (QEType*)(0) ) || ( g == (QEType*)(0) ) )
    {
    itkDebugMacro( "One or more argument(s) is(are) null." );
    return( (QEType*) 0 );
    }

  if ( h == g )
    {
    itkDebugMacro( "The two half-edges are the same. No antenna allowed." );
    return( (QEType*) 0 );
    }

  if( h->GetDestination( ) != g->GetDestination( ) )
    {
    itkDebugMacro( "The two half-edges must be incident to the same vertex." );
    return( (QEType*) 0 );
    }

  if( !this->m_Mesh )
    {
    itkDebugMacro( "No mesh present." );
    return( (QEType*) 0 );
    }

  // delete the faces
  this->m_Mesh->DeleteFace( h->GetRight( ) );
  this->m_Mesh->DeleteFace( g->GetRight( ) );

  // splice to create a new point and disconnect the rings
  this->m_NewPoint = this->m_Mesh->Splice( h->GetSym( ), g->GetSym( ) );

  // then add a new edge
  QEType* ReturnedEdge = this->m_Mesh->AddEdge( g->GetDestination( ),
                                                h->GetDestination( ) );

  // Build two new faces
  this->m_Mesh->AddFace( h->GetSym( ) );
  this->m_Mesh->AddFace( g->GetSym( ) );
  this->m_Mesh->Modified( );

  return( ReturnedEdge );

}

} // namespace itk

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitVertexFunction.txx
