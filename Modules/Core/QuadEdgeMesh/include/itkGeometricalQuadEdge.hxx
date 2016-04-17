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
#ifndef itkGeometricalQuadEdge_hxx
#define itkGeometricalQuadEdge_hxx
#include "itkGeometricalQuadEdge.h"
#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include "vcl_limits.h"
#endif
#include <limits>
#include <iostream>

namespace itk
{
/**
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
const typename GeometricalQuadEdge< TVRef, TFRef,
                                    TPrimalData, TDualData, PrimalDual >::OriginRefType
GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::m_NoPoint =
  std::numeric_limits< OriginRefType >::max();

/**
 *   Constructor
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >
::GeometricalQuadEdge() :
  m_Origin(m_NoPoint),
  m_Data(),
  m_DataSet(false),
  m_LineCellIdent(0)
{
}

/**
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::SetLnextRingWithSameLeftFace(
  const DualOriginRefType faceGeom,
  int maxSize)
{
#ifndef NDEBUG
  if ( !this->IsLnextSharingSameFace(maxSize) )
    {
    itkQEDebugMacro("Lnext() edges do NOT share the same Left().");
    return ( false );
    }
#endif

  IteratorGeom it = this->BeginGeomLnext();

  while ( maxSize && ( it != this->EndGeomLnext() ) )
    {
    it.Value()->SetLeft(faceGeom);
    it++;
    maxSize--;
    }

  return ( true );
}

/**
 * \brief Check wether the Lnext() ring of "this" edge is exactly of
 *        size three AND if those three edges all share the same Left().
 * @return Returns true when the Lnext() ring is the one of a triangle.
 *         Returns false otherwise.
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::IsLnextOfTriangle()
{
  return ( this->IsLnextSharingSameFace(3) );
}

/**
 * \brief Check wether the incoming argument is in the Onext() ring
 *        of "this" edge or not.
 * @param b The edge to test.
 * @return Returns true when "this" edge and the incoming argument are
 *         in the same Onext() ring. Returns false otherwise.
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::IsInOnextRing(Self *b)
{
  for ( IteratorGeom it  = this->BeginGeomOnext();
        it != this->EndGeomOnext();
        it++ )
    {
    if ( b == it.Value() )
      {
      return true;
      }
    }
  return false;
}

/**
 * \brief Check wether the incoming argument is in the Lnext() ring
 *        of "this" edge or not.
 * @param b The edge to test.
 * @return Returns true when "this" edge and the incoming argument are
 *         in the same Lnext() ring. Returns false otherwise.
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::IsInLnextRing(Self *b)
{
  for ( IteratorGeom it  = this->BeginGeomLnext();
        it != this->EndGeomLnext();
        it++ )
    {
    if ( b == it.Value() )
      {
      return true;
      }
    }
  return false;
}

/**
 * \brief Check wether edge's Origin is internal to the mesh (as opposed
 *        to being on the boundary) by looking if all the edges in the
 *        Onext() ring have a face set on both their Left() and Right()
 *        side.
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool
GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >
::IsOriginInternal() const
{
  ConstIteratorGeom it = this->BeginGeomOnext();

  while ( it != this->EndGeomOnext() )
    {
    typedef typename ConstIteratorGeom::QuadEdgeType QuadEdgeType;
    const QuadEdgeType *value = it.Value();
    if ( !value->IsInternal() ) { return false; }
    ++it;
    }
  return true;
}

/**
 * \brief Consider the first few edges in Lnext() ring of "this" edge.
 *         Check wether those edges all share the same Left().
 * @param  maxSize Looks at most maxSize edges in the Lnext() ring.
 * @return Returns true when the Lnext() ring share THE same
 *         Left() faces. Return false otherwise.
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::IsLnextSharingSameFace(int maxSize)
{
  IteratorGeom it = this->BeginGeomLnext();

  while ( maxSize && ( it != this->EndGeomLnext() ) )
    {
    // The condition isn't complicated: if left faces aren't set,
    // continue, if just one is set return false, if both are set
    // check if the face is the same
    bool facesAreNotSet = !this->IsLeftSet() && !it.Value()->IsLeftSet();
    bool facesAreTheSame = this->GetLeft() == it.Value()->GetLeft();
    bool facesAreSet = this->IsLeftSet() && it.Value()->IsLeftSet();
    //
    // FIXME: This boolean expression can be simplified.
    // ALEX : what about the version below ?
    //
    // if ( this->IsLeftSet() )         // one left set
    // {
    //     if (it.Value()->IsLeftSet()) // two left set
    //     {
    //         if( !(this->GetLeft() == it.Value()->GetLeft()) )
    //          {
    //              return( false );    // not same face
    //           }
    //      }
    //      else                        // only one set
    //      {
    //          return( false );
    //       }
    // }
    // else // one not set
    // {
    //     if(it.Value()->IsLeftSet()) // only one set
    //     {
    //         return( false );
    //     }
    // }
    //
    if ( !( facesAreNotSet || ( facesAreSet && facesAreTheSame ) ) )
      {
      return ( false );
      }
    it++;
    maxSize--;
    }

  if ( it != this->EndGeomLnext() )
    {
    // The Lnext ring is bigger than the caller expected
    return ( false );
    }
  return ( true );
}

/**
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
typename GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::Self *
GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::GetNextBorderEdgeWithUnsetLeft(Self *edgeTest)
{
  // Definition: an edge is said to be a boundary edge when it is adjacent to
  // noface i.e. when at least one of the faces edge->GetLeft() or
  // edge->GetRight() is unset.  Definition: an point is said to be a boundary
  // point when at least one of the edges of it's Onext() ring is a boundary
  // edge.
  //
  // Assume "this" edge belongs to a triangulation (i.e. it belongs to a QEMesh
  // which represents a 2-manifold) which possesses a boundary.  Assume "this"
  // edge instance is a boundary edge. Let us denote by P the point which is
  // the origin of "this" edge i.e. P is this->Origin().  By definition P is a
  // boundary point.  Then AT LEAST two [see the note below] edges of the
  // Onext() ring of P [which all have the point P as Origin()] are themselves
  // boundary edges. And among those boundary edges AT LEAST one has it's
  // Left() face unset.  By iterating over the Onext() ring (which defines a
  // local ordering on edges) this method searches for the first edge whose
  // Left() face is unset AND which is encountered AFTER edgeTest.
  //
  // @param edgeTest When present, this edge will be considered as
  //        the entry edge in the Onext() ring. When absent it shall
  //        be defaulted to "this" edge. (see the warning below).
  // @return When "this" edge is a boundary edge, return the first
  //         edge in "this" Onext() ring whose Left() face is unset
  //         AND located after edgeTest.
  //         When "this" edge is NOT a boundary edge the 0 is
  //         returned.
  // @warning When the Mesh possessing "this" edge is a 2-manifold
  //          then result of this method is unique in the sense that
  //          it is independent from the edgeTest parameter.
  //          But when the Mesh is not 2-manifold (this state can
  //          happen at intermediary stages of the building process,
  //          or during "surgical" operations on the Mesh, and
  //          even though the Mesh represents a triangulation)
  //          the result of this method is not unique in the sense
  //          that the result depends on the edgeTest parameter.
  //          Let us illusatre this dependence by considering a
  //          Mesh (which is a triangulation) which is not a 2-manifold.
  //          Assume the point P (the origin of "this" edge i.e.
  //          P = this->Originv()) is TWICE on the border i.e. it
  //          is adjacent twice to noface. We can consider the situation
  //          of the following diagram, which depicts some Onext()
  //          ring around point P:
  //
  //                       \         /                               //
  //                        \   *   /                                //
  //                        i3     b2              counter-clockwise //
  //                  *       \   /   NO FACE      Onext() order.    //
  //                           \ /                                   //
  //                 ----b4-----P----b1------                        //
  //                           /|\                                   //
  //               NO FACE    / | \                                  //
  //                         /  |  \    *  <------ a * indicates the //
  //                        /   |   \         the presence of a face //
  //                       /    |    \                               //
  //                     b5    i6     i7                             //
  //                     /   *  |  *   \                             //
  //                    /       |       \                            //
  //
  //          On this example, and if we assume the Onext() oder is
  //          represented counter-clockwise, the edges are ordered as
  //          follows:
  //             b1, b2, i3, b4, b5, i6, i7
  //          (when arbitrarily starting at edge b1).
  //          We have four Boundary edges labeled b1, b2, b4, b5 and
  //          we have three internal edges (i.e. non boundary edges)
  //          labeled i3, i6 and i7.
  //          Depending on edgeTest, the result of this method
  //          will NOT return the same edge:
  //            - when edgeTest == b5 (or i6 or i7 or b1) then the edge
  //              b1 will be returned,
  //            - when edgeTest == b2 (or i3 or b4) then the edge
  //              b4 will be returned,
  //          Eventually, when edgeTest is absent, the result shall
  //          depend on the position of "this" in the Onext() ring().
  //

  // Be sure the Onext ring isn't already full
  if ( this->IsOriginInternal() )
    {
    itkQEDebugMacro("Internal point.");
    return ( ITK_NULLPTR );
    }

  // Update reference
  edgeTest = ( !edgeTest ) ? this : edgeTest;

  // On efficiency purposes
  if ( edgeTest->IsIsolated() )
    {
    return ( edgeTest );
    }

  // Ok, no more special cases
  IteratorGeom it   = edgeTest->BeginGeomOnext();
  IteratorGeom end  = edgeTest->EndGeomOnext();

  while ( it != end )
    {
    if ( !it.Value()->IsLeftSet() )
      {
      return ( it.Value() );
      }
    it++;
    }

  // No border edge found
  itkQEDebugMacro("Unfound border edge.");
  return ( ITK_NULLPTR );
}

/**
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::InsertAfterNextBorderEdgeWithUnsetLeft(
  Self *isol,
  Self *hint)
{
  // When the geometry of isol is set it must match the
  // one of "this" Origin(). If the geometry is not set, we assume
  // that both Origin are the same, regardless their actual value.
  // Note: The purpose of this test is to avoid introducing some
  //       incoherence in the geometry at Origin().
  // The things should go this way:
  // 1/ when the geometry of "this" Origin is not set, then be paranoid
  //    and suspect the situation is already snafu:
  //    1a/ if all edges of "this" Onext ring have an unset Origin()
  //        (the situation is coherent), then proceed (Result=0)
  //        whatever the value of isol.Origin() might be.
  //    1b/ if one of the edges of "this" Onext ring has an Origin() set,
  //        then we deduce that there is already some geometrical
  //        incoherence at this->Origin() and exit this method (Result=1).
  // 2/ Then when we didn't exit at stage 1, consider isol.Origin():
  //    2a/ when isol.Origin() is absent proceed (result=0),
  //    2b/ when isol.Origin() is present and Origin == isol.OriginSet then
  //        proceed (result=0),
  //    2c/ when isol.Origin() is present and Origin != isol.OriginSet then
  //        exit (result=1).
  //
  // Here is what is implemented:
  // +-----------+----------------+--------------------------+--------+
  // | OriginSet | isol.OriginSet | Origin == isol.OriginSet | Result |
  // +-----------+----------------+--------------------------+--------+
  // |      0    |         0      |           0              |    0   |
  // |      0    |         0      |           1              |    0   |
  // |      0    |         1      |           0              |    1   |
  // |      0    |         1      |           1              |    1   |
  // +-----------+----------------+--------------------------+--------+
  // |      1    |         0      |           0              |    1   |
  // |      1    |         0      |           1              |    1   |
  // |      1    |         1      |           0              |    1   |
  // |      1    |         1      |           1              |    0   |
  // +-----------+----------------+--------------------------+--------+
  //
  if ( !(   !( IsOriginSet() || isol->IsOriginSet() )
            || ( IsOriginSet()
                 && isol->IsOriginSet()
                 && ( m_Origin == isol->m_Origin ) )
            )
       )
    {
    itkQEDebugMacro("Isolated Origin() differs from this Origin.");
    return ( false );
    }

  // Find out if this point has some room left for edge insertion:
  Self *edgeAfter = this->GetNextBorderEdgeWithUnsetLeft(hint);
  if ( !edgeAfter )
    {
    itkQEDebugMacro("This point is yet surrounded by faces.");
    return ( false );
    }

  // Normally, an edge was found
  edgeAfter->Splice(isol);
  return ( true );
}

/**
 */
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::ReorderOnextRingBeforeAddFace(
  Self *second)
{
  // Assume "this->Originv()" is a boundary point P that is thrice adjacent
  // to noface and consider the given situation is the one depicted by
  // the following diagram where:
  //   - P is "this->Originv()" instance,
  //   - the * (star) indicates the presence of a face,
  //   - b1, b2, b3, b4, b5, b6 denote boundary edges,
  //   - p denotes some generic point,
  //   - A and B denote some specific points we want to discuss,
  //   - the Onext() ring order is represented counter-clockwise
  //     [which is coherent with the definition of edge->GetRigth()]
  //     i.e. the ordering of the edges is:
  //          b1, b2, b3, b4, b5, b6, b1...
  //
  //                    p       N       p
  //                   / \      O      / \                            //
  //                  /   \           /   \                           //
  //                 /     \    F    /     \       counter-clockwise  //
  //                /      b3   A   b2      \      Onext() ring order //
  //               /         \  C  /         \                        //
  //              /     *     \ E /     *     \                       //
  //             /             \ /             \                      //
  //             A------b4------P------b1-------B                     //
  //                           / \                                    //
  //                          /   \                                   //
  //             NO FACE     /     \      NO FACE                     //
  //                        /       \                                 //
  //                      b5         b6                               //
  //                      /     *     \                               //
  //                     /             \                              //
  //                    p---------------p                             //
  //
  // At P this Mesh doesn't represent a 2-manifold (since we are thrice
  // on the boundary). Nevertheless such a situation could arise in
  // intermediary stages (e.g. when building the Mesh, or during
  // surgical changes on the Mesh).
  //    Now, assume we are asked to build the triangle [P, A, B]. Note
  // that this request is not absurd since the current situation at
  // P isn't the one of a 2-manifold: hence when building the current
  // Onext() ring of P, we had not enough information to decide
  // wheter b4.Onext() should be b5 or b1. It is ONLY when we are
  // required to build the triangle [P, A, B] that we have the
  // additional information that b4.Onext() is indeed b1.
  //    When we are required to build triangle [P, A, B], we hence
  // need to change the Onext() ring order at P, i.e. we need to deal
  // with the triangle [P, b5, b6] which currently prevents
  // b4.Onext() to be b1. In other terms, when considering the
  // additional information that b4.Onext() is b1, and before
  // building the triangle [P, A, B], we need to reorder
  // the Onext() ring of P from it's current state
  //    b1, b2, b3, b4, b5, b6, b1...
  // to an order coherent with the [P, A, B] request, i.e.
  //     b1, b2, b5, b6, b3, b4, b1...
  //
  // In order to establish the "proper" Onext() ring at P we use
  // two Splice operations. The algorithm goes:
  //   - first disconnect the piece of the surface containing the edge
  //     [PB] (it would be the same process if we chose [PA]) from
  //     the Onext() ring at P.
  //   - second, re-integrate the disconnected piece at the desired
  //     location i.e. side by side with [PA] (respectively [PB] if
  //     we chose [PA] at first stage).
  // By "piece of surface containing the edge [PB]" we mean [all]
  // the triangle[s] starting at [PB] in the Onext() order and
  // having a left face set.
  //
  // We can illustrate this process on bit more general diagram than
  // the last case (where the "piece of surface containing the edge
  // [PB]" is constituted by two triangles) and when using
  // the arguments of this method (i.e. [PA] = this and [PB] = second).
  // The initial stage is the following (we note first=this=[PA] and
  // second=[PB]) where the Onext() ring order is:
  //     first, b2, b3, second, b5, bsplice, b7, first...
  //
  //                    p       N       A                            //
  //                   / \      O      / \                           //
  //                  /   \           /   \                          //
  //                 /     \    F    /     \     counter-clockwise   //
  //                /      b2   A  first    \    Onext() ring order  //
  //               /         \  C  /         \                       //
  //              /     *     \ E /     *     \                      //
  //             /             \ /             \                     //
  //            p-------b3------P------b7-------p                    //
  //                           /|\                                   //
  //                          / | \                                  //
  //          NO FACE        /  |  \      NO FACE                    //
  //                        /   |   \                                //
  //                  second   b5   bsplice                          //
  //                      /  *  |  *  \                              //
  //                     /      |      \                             //
  //                    B-------p-------p                            //
  //
  // The first stage, implemented as
  //     second->Oprev()->Splice( bsplice ),
  // yields the following diagram:
  //
  //                    p       N       A                            //
  //                   / \      O      / \                           //
  //                  /   \     F     /   \                          //
  //                 /     \    A    /     \      counter-clockwise  //
  //                /      b2   C  first    \     Onext() ring order //
  //               /         \  E  /         \                       //
  //              /     *     \   /     *     \                      //
  //             /             \ /             \                     //
  //            p-------b3------P------b7-------p                    //
  //                                                                 //
  //                         NO FACE                                 //
  //                                                                 //
  //                           /|\                                   //
  //                          / | \                                  //
  //                         /  |  \                                 //
  //                        /   |   \                                //
  //                  second   b5   bsplice                          //
  //                      /  *  |  *  \                              //
  //                     /      |      \                             //
  //                    B-------p-------p                            //
  //
  // and the second stage, implemented as
  //      first->Splice( bsplice ),
  // yields the following diagram:
  //
  //                                    A                            //
  //         B__        NO FACE        / \                           //
  //         |  \__                   /   \                          //
  //         |     \__               /     \       counter-          //
  //         |      second         first    \      clockwise for all //
  //         |           \__       /         \                       //
  //         |     *        \__   /     *     \                      //
  //         |                 \ /             \                     //
  //         p-------b5---------P------b7-------p                    //
  //         |               __/|\                                   //
  //         |     *      __/   | \                                  //
  //         |           /      |  \      NO FACE                    //
  //         |     bsplice      |   \                                //
  //         |   __/           b2    b3                              //
  //         p__/               |  *  \                              //
  //                NO FACE     |      \                             //
  //                            p-------p                            //
  //
  Self *first = this;

  // Making sure point adjacency is correct:
  if ( first->GetOrigin() != second->GetOrigin() )
    {
    itkQEDebugMacro("Edges not adjacent at same point!");
    return ( false );
    }

  if ( first->GetOnext() == second )
    {
    return ( true );
    }

  if ( first->IsLeftSet() )
    {
    itkQEDebugMacro("First should NOT have a left face.");
    return ( false );
    }

  // Second is an internal edge.
  if ( second->IsInternal() )
    {
    return ( false );
    }

  Self *bsplice; // Does not require initialisation;
  // Disconnect the triangles containing second:
  if ( second->IsLeftSet() )
    {
    bsplice = second->GetNextBorderEdgeWithUnsetLeft();
    second->GetOprev()->Splice(bsplice);
    }
  else
    {
    // Orientation is localy clockwise:
    bsplice = second;
    second->GetOprev()->Splice(bsplice);
    }

  // Reconnect second after first:
  first->Splice(bsplice);
  return ( true );
}

// ---------------------------------------------------------------------
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
void GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >::Disconnect()
{
  if ( this->IsDisconnected() )
    {
    return;
    }

  // Update faces if the edge isn't a wire
  if ( this->IsAtBorder() )
    {
    Self *       e = ( this->IsRightSet() ) ? this->GetSym() : this;
    IteratorGeom it = e->BeginGeomLnext();
    while ( it != e->EndGeomLnext() )
      {
      it.Value()->UnsetLeft();
      it++;
      }
    }
  else if ( this->IsInternal() )
    {
    // Consolidate face
    DualOriginRefType face = this->GetRight();
    for ( IteratorGeom it  = this->BeginGeomLnext();
          it != this->EndGeomLnext();
          it++ )
      {
      it.Value()->SetLeft(face);
      }
    }

  // Hint edges
  Self *e0 = this->GetOprev();
  Self *e1 = this->GetLnext();

  // Disconnect entries
  if ( !this->IsOriginDisconnected() )
    {
    e0->Splice(this);
    }
  if ( !this->IsDestinationDisconnected() )
    {
    e1->Splice( this->GetSym() );
    }

  // Normally, this edge is converted to a simple wire
  this->UnsetOrigin();
  this->UnsetDestination();
  this->UnsetLeft();
  this->UnsetRight();
}

// ---------------------------------------------------------------------
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool
GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >
::IsOriginSet() const
{
  return ( this->m_Origin != m_NoPoint );
}

// ---------------------------------------------------------------------
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool
GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >
::IsDestinationSet() const
{
  const Self *p1 = this->GetSym();

  if ( p1 == ITK_NULLPTR )
    {
    return false; // FIXME: Is this the right answer ?
    }

  return p1->IsOriginSet();
}

// ---------------------------------------------------------------------
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool
GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >
::IsRightSet() const
{
  const DualType *p1 = this->GetRot();

  if ( p1 == ITK_NULLPTR )
    {
    return false;  // FIXME: Is this the right answer ?
    }

  return p1->IsOriginSet();
}

// ---------------------------------------------------------------------
template< typename TVRef, typename TFRef,
          typename TPrimalData, typename TDualData, bool PrimalDual >
bool
GeometricalQuadEdge< TVRef, TFRef, TPrimalData, TDualData, PrimalDual >
::IsLeftSet() const
{
  const DualType *p1 = this->GetInvRot();

  if ( p1 == ITK_NULLPTR )
    {
    return false;  // FIXME: Is this the right answer ?
    }

  return p1->IsOriginSet();
}
} // end of namespace itk

#endif
