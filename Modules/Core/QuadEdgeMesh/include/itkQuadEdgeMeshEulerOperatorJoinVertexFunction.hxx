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
#ifndef itkQuadEdgeMeshEulerOperatorJoinVertexFunction_hxx
#define itkQuadEdgeMeshEulerOperatorJoinVertexFunction_hxx

#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h"
#include "itkQuadEdgeMeshZipMeshFunction.h"

#include <list>
#include <algorithm>

namespace itk
{
template< typename TMesh, typename TQEType >
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::QuadEdgeMeshEulerOperatorJoinVertexFunction():Superclass(),
  m_OldPointID(0), m_EdgeStatus(STANDARD_CONFIG)
{}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
void
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "m_OldPointID: " << m_OldPointID << std::endl;
  os << indent << "m_EdgeStatus: ";

  switch ( m_EdgeStatus )
    {
    default:
    case STANDARD_CONFIG:
      os << "STANDARD_CONFIG" << std::endl;
      break;
    case QUADEDGE_ISOLATED:
      os << "QUADEDGE_ISOLATED" << std::endl;
      break;
    case FACE_ISOLATED:
      os << "FACE_ISOLATED" << std::endl;
      break;
    case EDGE_NULL:
      os << "EDGE_NULL" << std::endl;
      break;
    case MESH_NULL:
      os << "MESH_NULL" << std::endl;
      break;
    case EDGE_ISOLATED:
      os << "EDGE_ISOLATED" << std::endl;
      break;
    case TOO_MANY_COMMON_VERTICES:
      os << "TOO_MANY_COMMON_VERTICES" << std::endl;
      break;
    case TETRAHEDRON_CONFIG:
      os << "TETRAHEDRON_CONFIG" << std::endl;
      break;
    case SAMOSA_CONFIG:
      os << "SAMOSA_CONFIG" << std::endl;
      break;
    case EYE_CONFIG:
      os << "EYE_CONFIG" << std::endl;
      break;
    case EDGE_JOINING_DIFFERENT_BORDERS:
      os << "EDGE_JOINING_DIFFERENT_BORDERS" << std::endl;
      break;
    }
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
typename QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::Evaluate(QEType *e)
{
  std::stack< QEType * > edges_to_be_deleted;

  m_EdgeStatus = CheckStatus(e, edges_to_be_deleted);

  switch ( m_EdgeStatus )
    {
    default:
    case STANDARD_CONFIG:
      return Process(e);

    // ******************************************************************
    // Isolated quad edge
    case QUADEDGE_ISOLATED:
      return ProcessIsolatedQuadEdge(e);

    // ******************************************************************
    // Isolated face
    case FACE_ISOLATED:
      return ProcessIsolatedFace(e, edges_to_be_deleted);

    // ******************************************************************
    // e == 0
    case EDGE_NULL:
    // m_Mesh == 0
    case MESH_NULL:
    // e->IsIsolated() && e_sym->IsIsolated()
    case EDGE_ISOLATED:
    // more than 2 common vertices in 0-ring of org and dest respectively
    case TOO_MANY_COMMON_VERTICES:
    // Tetrahedron case
    case TETRAHEDRON_CONFIG:
    // Samosa case
    case SAMOSA_CONFIG:
    // Eye case
    case EYE_CONFIG:
      return ( (QEType *)ITK_NULLPTR );
    case EDGE_JOINING_DIFFERENT_BORDERS:
      return ( (QEType *)ITK_NULLPTR );
    }
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
TQEType *
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::Process(QEType *e)
{
  QEType *e_sym = e->GetSym();

  // General case
  bool wasLeftFace     = e->IsLeftSet();
  bool wasRiteFace     = e->IsRightSet();
  bool wasLeftTriangle = e->IsLnextOfTriangle();
  bool wasRiteTriangle = e_sym->IsLnextOfTriangle();

  PointIdentifier NewDest = e->GetDestination();
  PointIdentifier NewOrg  = e->GetOrigin();
  QEType *        leftZip = e->GetLnext();
  QEType *        riteZip = e->GetOprev();

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
  this->m_Mesh->LightWeightDeleteEdge(e);
  this->m_OldPointID = this->m_Mesh->Splice(leftZip, riteZip);

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
  // When the Lnext and/or the Rnext ring of the argument edge was originally
  // the one[s] of a triangle, the above edge deletion created the odd
  // situation of having two different edges adjacent to the same two
  // vertices (which is quite a bad thing). This is was is depicted on
  // the above ascii-graph, the original left face was a triangle and
  // the resulting situation has two different edges adjacent to the
  // two vertices X and a. In order to clean up things, we can call the
  // Zip(MeshFunction) algorithm which handles this case.
  // Once things are back to normal, we recreate faces when they were
  // originally present.
  //

  typedef QuadEdgeMeshZipMeshFunction< MeshType, QEType > Zip;
  if ( wasLeftTriangle )
    {
    typename Zip::Pointer zip = Zip::New();
    zip->SetInput(this->m_Mesh);
    if ( QEType::m_NoPoint != zip->Evaluate(leftZip) )
      {
      itkDebugMacro("Zip must return NoPoint (left).");
      return ( (QEType *)ITK_NULLPTR );
      }
    }
  else
    {
    if ( wasLeftFace )
      {
      this->m_Mesh->AddFace(leftZip);
      }
    }

  // NewOrg = riteZip->GetOprev( )->GetDestination( );
  if ( wasRiteTriangle )
    {
    NewOrg = riteZip->GetDestination();
    typename Zip::Pointer zip = Zip::New();
    zip->SetInput(this->m_Mesh);
    if ( QEType::m_NoPoint != zip->Evaluate(riteZip) )
      {
      itkDebugMacro("Zip must return NoPoint (right).");
      return ( (QEType *)ITK_NULLPTR );
      }
    }
  else
    {
    NewOrg = riteZip->GetLprev()->GetOrigin();
    if ( wasRiteFace )
      {
      this->m_Mesh->AddFace(riteZip);
      }
    }

  OutputType result = this->m_Mesh->FindEdge(NewOrg, NewDest);
  if ( !result )
    {
    result = this->m_Mesh->FindEdge(NewDest)->GetSym();
    }
  return ( result );
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
TQEType *
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::ProcessIsolatedQuadEdge(QEType *e)
{
  QEType *temp = ( e->IsIsolated() == true ) ? e->GetSym() : e;
  QEType *rebuildEdge = temp->GetOprev();

  m_OldPointID = temp->GetSym()->GetOrigin();

  bool e_leftset = e->IsLeftSet();
  this->m_Mesh->LightWeightDeleteEdge(e);
  if ( e_leftset )
    {
    this->m_Mesh->AddFace(rebuildEdge);
    }

  // this case has no symetric case in SPlitVertex
  // i.e. it is impossible to reconstruct such a pathological
  // case using SplitVertex. Thus the return value is
  // of less interest.
  // We return an edge whose dest is a, whichever.
  return ( rebuildEdge );
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
TQEType *
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::ProcessIsolatedFace(
  QEType *e,
  std::stack< QEType * > &
  EdgesToBeDeleted)
{
  PointIdentifier org = e->GetOrigin();
  PointIdentifier dest = e->GetDestination();

  // delete all elements
  while ( !EdgesToBeDeleted.empty() )
    {
    this->m_Mesh->LightWeightDeleteEdge( EdgesToBeDeleted.top() );
    EdgesToBeDeleted.pop();
    }

  // it now retuns one edge from NewDest or NewOrg if there are any
  // else ITK_NULLPTR
  QEType *temp = this->m_Mesh->FindEdge(dest);
  if ( temp != ITK_NULLPTR )
    {
    return temp;
    }
  else
    {
    return this->m_Mesh->FindEdge(org);
    }
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
bool
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::IsFaceIsolated(QEType *e,
                                                                              const bool & iWasLeftFace,
                                                                              std::stack< TQEType * > & oToBeDeleted)
{
  bool    border;
  QEType *e_sym = e->GetSym();

  // turn around the face (left or right one) while edges are on the border
  // and push them into a stack (which will be used to delete properly all
  // elements )
  QEType *temp = ( iWasLeftFace == true ) ? e : e_sym;
  QEType *e_it = temp;

  oToBeDeleted.push(e_it);
  e_it = e_it->GetLnext();

  do
    {
    oToBeDeleted.push(e_it);
    border = e_it->IsAtBorder();
    e_it = e_it->GetLnext();
    }
  while ( ( e_it != temp ) && border );

  return border;
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
typename QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType
                                                      >::EdgeStatusType
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::CheckStatus(QEType *e,
                                                                           std::stack< TQEType * > & oToBeDeleted)
{
#ifndef NDEBUG
  if ( !e )
    {
    itkDebugMacro("Input is not an edge.");
    return EDGE_NULL;
    }

  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return MESH_NULL;
    }
#endif

  QEType *e_sym = e->GetSym();

  bool IsEdgeIsolated = e->IsIsolated();
  bool IsSymEdgeIsolated = e_sym->IsIsolated();
  if ( IsEdgeIsolated || IsSymEdgeIsolated )
    {
    if ( IsEdgeIsolated && IsSymEdgeIsolated )
      {
      // We could shrink the edge to a point,
      // But we consider this case to be degenerated.
      itkDebugMacro("Argument edge isolated.");
      return EDGE_ISOLATED;
      }

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
    // of e is attached in a.
    return QUADEDGE_ISOLATED;
    }

  PointIdentifier number_common_vertices = CommonVertexNeighboor(e);
  if ( number_common_vertices > 2 )
    {
    itkDebugMacro("The 2 vertices have more than 2 common neighboor vertices.");
    return TOO_MANY_COMMON_VERTICES;
    }

  if ( number_common_vertices == 2 )
    {
    if ( IsTetrahedron(e) )
      {
      itkDebugMacro("It forms a tetrahedron.");
      return TETRAHEDRON_CONFIG;
      }
    }

  // General case
  bool wasLeftFace     = e->IsLeftSet();
  bool wasRiteFace     = e->IsRightSet();

  if ( wasLeftFace && wasRiteFace )
    {
    if ( IsSamosa(e) )
      {
      itkDebugMacro("SAMOSA_CONFIG.");
      return SAMOSA_CONFIG;
      }
    if ( IsEye(e) )
      {
      itkDebugMacro("EYE_CONFIG.");
      return EYE_CONFIG;
      }
    if ( IsEdgeLinkingTwoDifferentBorders(e) )
      {
      itkDebugMacro("EDGE_JOINING_DIFFERENT_BORDERS.");
      return EDGE_JOINING_DIFFERENT_BORDERS;
      }
    }
  else
    {
    if ( wasLeftFace || wasRiteFace )
      {
      if ( IsFaceIsolated(e, wasLeftFace, oToBeDeleted) )
        {
        itkDebugMacro("FACE_ISOLATED.");
        return FACE_ISOLATED;
        }
      }
    }

  return STANDARD_CONFIG;
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
bool
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::IsTetrahedron(QEType *e)
{
  if ( e->GetOrder() == 3 )
    {
    QEType *e_sym = e->GetSym();
    if ( e_sym->GetOrder() == 3 )
      {
      if ( e->GetLprev()->GetOrder() == 3 )
        {
        if ( e_sym->GetLprev()->GetOrder() == 3 )
          {
          bool left_triangle = e->IsLnextOfTriangle();
          bool right_triangle = e_sym->IsLnextOfTriangle();

          if ( left_triangle && right_triangle )
            {
            CellIdentifier id_left_right_triangle;
            if ( e->GetLprev()->IsRightSet() )
              {
              id_left_right_triangle = e->GetLprev()->GetRight();
              }
            else
              {
              return false;
              }

            CellIdentifier id_left_left_triangle;
            if ( e->GetLnext()->IsRightSet() )
              {
              id_left_left_triangle = e->GetLnext()->GetRight();
              }
            else
              {
              return false;
              }

            CellIdentifier id_right_left_triangle;
            if ( e_sym->GetLnext()->IsRightSet() )
              {
              id_right_left_triangle = e_sym->GetLnext()->GetRight();
              }
            else
              {
              return false;
              }

            CellIdentifier id_right_right_triangle;
            if ( e_sym->GetLprev()->IsRightSet() )
              {
              id_right_right_triangle = e_sym->GetLprev()->GetRight();
              }
            else
              {
              return false;
              }

            if ( ( id_left_right_triangle == id_right_left_triangle )
                 && ( id_left_left_triangle == id_right_right_triangle ) )
              {
              return true;
              }
            }
          }
        }
      }
    }

  return false;
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
bool
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::IsSamosa(QEType *e)
{
  return ( ( e->GetOrder() == 2 ) &&  ( e->GetSym()->GetOrder() == 2 ) );
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
bool
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::IsEye(QEType *e)
{
  bool OriginOrderIsTwo = ( e->GetOrder() == 2 );
  bool DestinationOrderIsTwo = ( e->GetSym()->GetOrder() == 2 );

  return ( ( OriginOrderIsTwo && !DestinationOrderIsTwo )
           || ( !OriginOrderIsTwo && DestinationOrderIsTwo ) );
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
typename QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::PointIdentifier
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::CommonVertexNeighboor(QEType *e)
{
  QEType *qe = e;
  QEType *e_it  = qe->GetOnext();

  typedef std::list< PointIdentifier > PointIdentifierList;
  PointIdentifierList dir_list;
  PointIdentifierList sym_list;
  PointIdentifierList intersection_list;

  PointIdentifier id;
  do
    {
    id = e_it->GetDestination();
    dir_list.push_back(id);
    e_it = e_it->GetOnext();
    }
  while ( e_it != qe );

  qe = qe->GetSym();
  e_it = qe;

  do
    {
    id = e_it->GetDestination();
    sym_list.push_back(id);
    e_it = e_it->GetOnext();
    }
  while ( e_it != qe );

  dir_list.sort();
  sym_list.sort();

  std::set_intersection( dir_list.begin(), dir_list.end(),
                         sym_list.begin(), sym_list.end(),
                         std::back_inserter(intersection_list) );

  return static_cast< PointIdentifier >( intersection_list.size() );
}

//--------------------------------------------------------------------------
template< typename TMesh, typename TQEType >
bool
QuadEdgeMeshEulerOperatorJoinVertexFunction< TMesh, TQEType >::IsEdgeLinkingTwoDifferentBorders(QEType *e)
{
  QEType *t = e;
  QEType *e_it = t;
  bool    org_border;

  do
    {
    org_border = e_it->IsAtBorder();
    e_it = e_it->GetOnext();
    }
  while ( ( e_it != t ) && ( !org_border ) );
  if ( !org_border )
    {
    return false;
    }
  else
    {
    t = e->GetSym();
    e_it = t;
    bool dest_border;
    do
      {
      dest_border = e_it->IsAtBorder();
      e_it = e_it->GetOnext();
      }
    while ( ( e_it != t ) && ( !dest_border ) );

    if ( !dest_border )
      {
      return false;
      }
    else
      {
      return true;
      }
    }
}
} // namespace itkQE

#endif

// eof - itkQuadEdgeMeshEulerOperatorJoinVertexFunction.hxx
