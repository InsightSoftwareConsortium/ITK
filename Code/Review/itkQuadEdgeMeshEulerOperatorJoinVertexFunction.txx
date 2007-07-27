// -------------------------------------------------------------------------
// itkQuadEdgeMeshEulerOperatorJoinVertexFunction.txx
// $Revision: 1.2 $
// $Author: hanfei $
// $Name:  $
// $Date: 2007-07-27 06:34:01 $
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
#ifndef __itkQuadEdgeMeshEulerOperatorJoinVertexFunction_txx
#define __itkQuadEdgeMeshEulerOperatorJoinVertexFunction_txx

#include "itkQuadEdgeMeshZipMeshFunction.h"

namespace itk
{

//--------------------------------------------------------------------------
template < class TMesh, class TQEType >
typename QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::
Evaluate( QEType* e )
{
  
  if( !this->m_Mesh )
    {
    itkDebugMacro( "No mesh present." );
    return( (QEType*) 0 );
    }

  if( e->IsIsolated( ) && e->GetSym( )->IsIsolated( ) )
    {
    // We could shrink the edge to a point,
    // But we consider this case to be degenerated.
    itkDebugMacro( "Argument edge isolated." );
    return( (QEType*) 0 );
    }
   
  // First case: pathological
  if( e->IsIsolated( ) || e->GetSym( )->IsIsolated( ) )
    {
    // One the endpoints (and only one) of the incoming edge is isolated.
    // Instead of "shrinking" the edge it suffice to delete it. Note that
    // this also avoids trouble since the definition of leftZip or riteZip
    // would be improper in this case (in fact leftZip or riteZip would
    // in fact be e or e->GetSym( )...
    //
    // When e is adjacent to a face, we must retrieve the edge [ a, b ]
    // in order to rebuild the face after edge deletion. If the left face
    // of e is set then the right face is also set... since it is the same
    // face ! Here is the situation:
    //
    //        b----a----X
    //        |    |    |
    //        |    e    |
    //        |    |    |
    //        |         |
    //        X----X----X
    //
    // We are not yet sure of the orientation of e and which endpoint 
    // of e is attached in a. Find it out:
    QEType* rebuildEdge;
    if( e->IsIsolated( ) )
      {
      rebuildEdge = e->GetSym( )->GetOprev( );
      this->m_OldPointID = e->GetOrigin( );
      }
    else
      {
      rebuildEdge = e->GetOprev( );
      this->m_OldPointID = e->GetSym( )->GetOrigin( );
      }
      
    bool e_leftset = e->IsLeftSet( );
    this->m_Mesh->LightWeightDeleteEdge( e );
    if( e_leftset )
      {
      this->m_Mesh->AddFace( rebuildEdge );
      }
      
    // this case has no symetric case in SPlitVertex
    // i.e. it is impossible to reconstruct such a pathological
    // case using SplitVertex. Thus the return value is
    // of less interest.
    // We return an edge whose dest is a, whichever.
    return( rebuildEdge );
    }


  // General case
  bool wasLeftFace     = e->IsLeftSet( );
  bool wasRiteFace     = e->IsRightSet( );
  bool wasLeftTriangle = e->IsLnextOfTriangle( );
  bool wasRiteTriangle = e->GetSym( )->IsLnextOfTriangle( );
  PointIdentifier NewDest = e->GetDestination( );
  PointIdentifier NewOrg  = e->GetOrigin( );
  QEType* leftZip = e->GetLnext( );
  QEType* riteZip = e->GetOprev( );

  //
  //                   \   |   /
  //                     \  |  / 
  //                      \ | /
  //     ------------------ b ------------- Y
  //                   ___/ |               |  
  //      _<_leftZip__/     |               |
  //     /                  ^               |
  //    X      left         e     rite      |
  //     \____________      |               |
  //                  \___  |               |
  //                      \ |               |
  //     ------------------ a --riteZip->-- Y
  //                      / | \
  //                     /  |  \
  //                    /   |   \
  //
  this->m_Mesh->LightWeightDeleteEdge( e );
  this->m_OldPointID = this->m_Mesh->Splice( leftZip, riteZip );

  //
  //                            |      /       __Y 
  //                            |     /     __/  |  
  //       __<_leftZip___       |    /   __/     |
  //      /              \      |   / __/        |
  //     /                \__   |  / /           |
  //    X      NOFACE      __ [a = b]    NOFACE  |
  //     \                /  / / | \ \__         |
  //      \______________/ _/ /  |  \   \__      |
  //                    __/  /   |   \  riteZip  |
  //                 __/    /    |    \       \__|
  //                /      /     |     \         Y
  // 
  // When the Lnext and/or the Rnext ring of the argument edge was originaly
  // the one[s] of a triangle, the above edge deletion created the odd
  // situation of having two different edges adjacent to the same two
  // vertices (which is quite a bad thing). This is was is depicted on 
  // the above ascii-graph, the original left face was a triangle and
  // the resulting situation has two different edges adjacent to the
  // two vertices X and a. In order to clean up things, we can call the
  // Zip(MeshFunction) algorithm which handles this case.
  // Once things are back to normal, we recreate faces when they were
  // originaly present.
  //

  typedef itk::QuadEdgeMeshZipMeshFunction< MeshType, QEType > Zip;
  if( wasLeftTriangle )
    {
    typename Zip::Pointer zip = Zip::New( );
    zip->SetInput( this->m_Mesh );
    if( QEType::m_NoPoint != zip->Evaluate( leftZip ) )
      {
      itkDebugMacro( "Zip must return NoPoint (left)." );
      return( (QEType*) 0 );
      }
    }
  else
    {
    if( wasLeftFace )
      {
      this->m_Mesh->AddFace( leftZip );
      }
    }

  if( wasRiteTriangle )
    {
    NewOrg = riteZip->GetDestination( );
    typename Zip::Pointer zip = Zip::New( );
    zip->SetInput( this->m_Mesh );
    if( QEType::m_NoPoint != zip->Evaluate( riteZip ) )
      {
      itkDebugMacro( "Zip must return NoPoint (right)." );
      return( (QEType*) 0 );
      }
    }
  else
    {
    NewOrg = riteZip->GetLprev( )->GetOrigin( );
    if( wasRiteFace )
      {
      this->m_Mesh->AddFace( riteZip );
      }
    }

  return( this->m_Mesh->FindEdge( NewOrg, NewDest ) );

}

} // namespace itkQE

#endif

// eof - itkQuadEdgeMeshEulerOperatorJoinVertexFunction.txx
