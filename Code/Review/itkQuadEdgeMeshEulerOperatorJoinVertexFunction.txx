/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorJoinVertexFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorJoinVertexFunction_txx
#define __itkQuadEdgeMeshEulerOperatorJoinVertexFunction_txx

#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h"
#include "itkQuadEdgeMeshZipMeshFunction.h"

namespace itk
{

//--------------------------------------------------------------------------
template < class TMesh, class TQEType >
typename QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::
Evaluate( QEType* e )
{
  if( !e )
    {
    itkDebugMacro( "Input is not an edge." );
    return( (QEType*) 0 );
    }

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

  size_t number_common_vertices = CommonVertexNeighboor( e );
  if( number_common_vertices > 2 )
    {
    itkDebugMacro( "The 2 vertices have more than 2 common neighboor vertices.");
    return( (QEType*) 0 );
    }
  if( number_common_vertices == 2 )
  {
    if( IsTetraedron( e ) )
      {
      itkDebugMacro( "It forms a tetraedron." );
      return( (QEType*) 0 );
      }
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

  // case where the edge belongs to an isolated triangle
  if( e->IsAtBorder( ) )
    {
    if( wasLeftTriangle && wasRiteTriangle )
      {
      this->m_Mesh->LightWeightDeleteEdge( e->GetLnext( )->GetLnext( ) );
      this->m_Mesh->LightWeightDeleteEdge( e->GetLnext( ) );
      this->m_Mesh->LightWeightDeleteEdge( e );
      return( (QEType*) 0 );
      }
    }


  //
  //                    \   |   /                //
  //                     \  |  /                 //
  //                      \ | /                  //
  //     ------------------ b ------------- Y    //
  //                   ___/ |               |    //
  //      _<_leftZip__/     |               |    //
  //     /                  ^               |    //
  //    X      left         e     rite      |    //
  //     \____________      |               |    //
  //                  \___  |               |    //
  //                      \ |               |    //
  //     ------------------ a --riteZip->-- Y    //
  //                      / | \                  //
  //                     /  |  \                 //
  //                    /   |   \                //
  //
  this->m_Mesh->LightWeightDeleteEdge( e );
  this->m_OldPointID = this->m_Mesh->Splice( leftZip, riteZip );
 
  //
  //                            |      /       __Y  //
  //                            |     /     __/  |  //
  //       __<_leftZip___       |    /   __/     |  //
  //      /              \      |   / __/        |  //
  //     /                \__   |  / /           |  //
  //    X      NOFACE      __ [a = b]    NOFACE  |  //
  //     \                /  / / | \ \__         |  //
  //      \______________/ _/ /  |  \   \__      |  //
  //                    __/  /   |   \  riteZip  |  //
  //                 __/    /    |    \       \__|  //
  //                /      /     |     \         Y  //
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

  // NewOrg = riteZip->GetOprev( )->GetDestination( );
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
  
  OutputType result = this->m_Mesh->FindEdge( NewOrg, NewDest );
  if( !result)
    {
    result = this->m_Mesh->FindEdge( NewDest )->GetSym( );
    } 
  return( result );

}

//--------------------------------------------------------------------------
template < class TMesh, class TQEType >
bool
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::
IsTetraedron( QEType* e )
{
  QEType* e_sym = e->GetSym();

  if( ( e->GetOrder() == 3 ) &&
      ( e_sym->GetOrder() == 3 ) &&
      ( e->GetLprev()->GetOrder() == 3 ) &&
      ( e_sym->GetLprev()->GetOrder() == 3 ) )
    {
    bool left_triangle = e->IsLnextOfTriangle( );
    bool right_triangle = e_sym->IsLnextOfTriangle( );
  
    if( left_triangle && right_triangle )
      {
      CellIdentifier
        id_left_right_triangle = e->GetLprev()->GetRight();
      CellIdentifier
        id_left_left_triangle = e->GetLnext()->GetRight();
      CellIdentifier
        id_right_left_triangle = e_sym->GetLnext()->GetRight();
      CellIdentifier
        id_right_right_triangle = e_sym->GetLprev()->GetRight();

      if( ( id_left_right_triangle == id_right_left_triangle ) &&
          ( id_left_left_triangle == id_right_right_triangle ) )
        {
          return true;
        }
      }
    }

  return false;
}

//--------------------------------------------------------------------------
template < class TMesh, class TQEType >
size_t
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::
CommonVertexNeighboor( QEType* e )
{
///NOTE  arnaud gelas: these first tests can not be applied in the
///  case of mesh with boundaries (so I commented out for the time being)
// 
//   bool isLeftTriangle = e->IsLnextOfTriangle( );
//   bool isRiteTriangle = e_sym->IsLnextOfTriangle( );
// 
//   //easy case
//   if( isLeftTriangle && isRiteTriangle )
//     {
//     if( e->GetOrder( ) <= 3 && e_sym->GetOrder( ) <= 3 )
//       return( true );
//     if( e->GetOnext( )->GetSym( )->GetOrder( ) <= 3 )
//       return( true );
//     if( e->GetOprev( )->GetSym( )->GetOrder( ) <= 3 )
//       return( true );
//     }
// 
//   // general case

///NOTE arnaud gelas: I replace the previous code by this one which is cleaner
  QEType* qe = e;
  QEType* e_it  = qe->GetOnext( );

  typedef std::list< PointIdentifier > PointIdentifierList;
  PointIdentifierList dir_list;
  PointIdentifierList sym_list;
  PointIdentifierList intersection_list;

  PointIdentifier id;
  do
  {
    id = e_it->GetDestination();
    dir_list.push_back( id );
    e_it = e_it->GetOnext();
  } while( e_it != qe );

  qe = qe->GetSym();
  e_it = qe;

  do
  {
    id = e_it->GetDestination();
    sym_list.push_back( id );
    e_it = e_it->GetOnext();
  } while( e_it != qe );

  dir_list.sort();
  sym_list.sort();

  std::set_intersection( dir_list.begin(), dir_list.end(),
    sym_list.begin(), sym_list.end(),
    std::back_inserter( intersection_list ) );

  return intersection_list.size();

//   unsigned int counter = 0;
//   QEType* e_it = e->GetOnext( );
//   
//   while( e_it != e )
//     {
//     QEType* e_sym_it = e_sym->GetOnext( );
//     while( e_sym_it != e_sym )
//       {
//       if(  e_it->GetDestination() == e_sym_it->GetDestination() )
//         counter++;
//       e_sym_it = e_sym_it->GetOnext( );
//       }
//     e_it = e_it->GetOnext( );
//     }
//   return ( counter > 2 );
}

} // namespace itkQE

#endif

// eof - itkQuadEdgeMeshEulerOperatorJoinVertexFunction.txx
