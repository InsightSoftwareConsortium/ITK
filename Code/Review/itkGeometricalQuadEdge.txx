// -------------------------------------------------------------------------
// itkGeometricalQuadEdge.txx
// $Revision: 1.2 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-14 15:49:44 $
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

#ifndef __itkGeometricalQuadEdge_txx
#define __itkGeometricalQuadEdge_txx

#include <vcl_limits.h>

namespace itk
{

/**
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
const typename GeometricalQuadEdge< TVRef, TFRef,
                             TPRef, TDRef, PrimalDual >::OrgRefType
GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::NOPOINT
                       = vcl_numeric_limits< OrgRefType >::max( );

/**
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    GeometricalQuadEdge( )
        : Superclass( ),
          m_Org( NOPOINT ),
          m_DataSet( false )
{
}

/**
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    bool GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    SetLnextRingWithSameLeftFace( const DualOrgRefType faceGeom,
                                  int maxSize )
{
  if( !this->IsLnextSharingSameFace( maxSize ) )
    {
    itkQEDebugMacro( "Lnext() edges do NOT share the same Left()." );
    return( false );
    }

  IteratorGeom it = this->BeginGeomLnext( );

  for( ; maxSize && ( it != this->EndGeomLnext( ) ); it++, maxSize-- )
    {
    it.Value( )->SetLeft( faceGeom );
    }

  return( true );
}

/**
 * \brief Check wether the Lnext() ring of "this" edge is exactly of
 *        size three AND if those three edges all share the same Left().
 * @return Returns true when the Lnext() ring is the one of a triangle.
 *         Returns false otherwise.
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    bool GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    IsLnextOfTriangle( )
{
  return( this->IsLnextSharingSameFace( 3 ) ); 
}

/**
 * \brief Check wether the incoming argument is in the Onext() ring
 *        of "this" edge or not.
 * @param b The edge to test.
 * @return Returns true when "this" edge and the incoming argument are
 *         in the same Onext() ring. Returns false otherwise.
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    bool GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    IsInOnextRing( Self* b )
{
  for( IteratorGeom it  = this->BeginGeomOnext( );
                      it != this->EndGeomOnext( );
                      it++ )
    {
    if( b == it.Value( ) )
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
          typename TPRef, typename TDRef, bool PrimalDual >
    bool GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    IsInLnextRing( Self* b )
{
  for( IteratorGeom it  = this->BeginGeomLnext( );
                      it != this->EndGeomLnext( );
                      it++ )
    {
    if( b == it.Value( ) )
      {
      return true;
      }
    }
  return false;
}

/**
 * \brief Check wether edge's Org is internal to the mesh (as opposed
 *        to being on the boundary) by looking if all the edges in the
 *        Onext() ring have a face set on both their Left() and Right()
 *        side.
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    bool GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    IsOrgInternal( )
{
  bool ret = true;
  IteratorGeom it = this->BeginGeomOnext( );
  for( ; it != this->EndGeomOnext( ); it++ )
    {
    ret &= ( it.Value( )->IsInternal( ) );
    }
  return( ret );
}

/**
 * \brief Consider the first few edges in Lnext() ring of "this" edge.
 *         Check wether those edges all share the same Left().
 * @param  maxSize Looks at most maxSize edges in the Lnext() ring.
 * @return Returns true when the Lnext() ring share THE same
 *         Left( ) faces. Return false otherwise.
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    bool GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    IsLnextSharingSameFace( int maxSize )
{
  IteratorGeom it = this->BeginGeomLnext( );
  
  for( ; maxSize && ( it != this->EndGeomLnext( ) ); it++, maxSize-- )
    {
    // The condition isn't complicated: if left faces aren't set,
    // continue, if just one is set return false, if both are set
    // check if the face is the same
    bool facesAreNotSet = !this->IsLeftSet() && !it.Value()->IsLeftSet();
    bool facesAreTheSame = this->GetLeft() == it.Value()->GetLeft();
    bool facesAreSet = this->IsLeftSet( ) && it.Value( )->IsLeftSet();
    //
    // FIXME: This boolean expression can be simplified.
    // 
    if( !( facesAreNotSet || ( facesAreSet && facesAreTheSame ) ) )
      {
      return( false );
      }
    }

  if( it != this->EndGeomLnext( ) )
    {
    // The Lnext ring is bigger than the caller expected
    return( false );
    }
  return( true );
}
 
/**
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    typename GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::Self*
    GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    GetNextBorderEdgeWithUnsetLeft( Self* edgeTest )
{
  /* Definition: an edge is said to be a boundary edge when it is
   * adjacent to noface i.e. when at least one of the faces
   * edge->GetLeft() or edge->GetRight() is unset.
   * Definition: an point is said to be a boundary point when at
   * least one of the edges of it's Onext() ring is a boundary
   * edge.
   * 
   * Assume "this" edge belongs to a triangulation (i.e. it belongs
   * to a QEMesh which represents a 2-manifold) which possesses a boundary.
   * Assume "this" edge instance is a boundary edge. Let us denote by
   * P the point which is the origin of "this" edge i.e. P is this->Org().
   * By definition P is a boundary point.
   * Then AT LEAST two [see the note below] edges of the Onext() ring
   * of P [which all have the point P as Org()] are themselves
   * boundary edges. And among those boundary edges AT LEAST one has
   * it's Left() face unset.
   * By iterating over the Onext() ring (which defines a local
   * ordering on edges) this method searches for the first edge whose
   * Left() face is unset AND which is encountered AFTER edgeTest.
   * @param edgeTest When present, this edge will be considered as
   *        the entry edge in the Onext() ring. When absent it shall
   *        be defaulted to "this" edge. (see the warning below).
   * @return When "this" edge is a boundary edge, return the first
   *         edge in "this" Onext() ring whose Left() face is unset
   *         AND located after edgeTest.
   *         When "this" edge is NOT a boundary edge the 0 is
   *         returned.
   * @warning When the Mesh possesing "this" edge is a 2-manifold
   *          then result of this method is unique in the sense that
   *          it is independant from the edgeTest parameter.
   *          But when the Mesh is not 2-manifold (this state can
   *          happen at intermediary stages of the building process,
   *          or during "surgical" operations on the Mesh, and
   *          even though the Mesh represents a triangulation)
   *          the result of this method is not unique in the sense
   *          that the result depends on the edgeTest parameter.
   *          Let us illusatre this dependance by considering a
   *          Mesh (which is a triangulation) which is not a 2-manifold.
   *          Assume the point P (the origin of "this" edge i.e.
   *          P = this->Orgv()) is TWICE on the border i.e. it
   *          is adjacent twice to noface. We can consider the situation
   *          of the following diagram, which depicts some Onext()
   *          ring around point P:
   *
   *                       \         /
   *                        \   *   /
   *                        i3     b2              counter-clockwise
   *                  *       \   /   NO FACE      Onext() order.
   *                           \ /
   *                 ----b4-----P----b1------
   *                           /|\
   *               NO FACE    / | \
   *                         /  |  \    *  <------ a * indicates the
   *                        /   |   \              the presence of a face
   *                       /    |    \
   *                     b5    i6     i7
   *                     /   *  |  *   \
   *                    /       |       \
   *
   *          On this example, and if we assume the Onext() oder is
   *          represented counter-clockwise, the edges are ordered as
   *          follows:
   *             b1, b2, i3, b4, b5, i6, i7
   *          (when arbitrarily starting at edge b1).
   *          We have four Boundary edges labeled b1, b2, b4, b5 and
   *          we have three internal edges (i.e. non boundary edges)
   *          labeled i3, i6 and i7.
   *          Depending on edgeTest, the result of this method
   *          will NOT return the same edge:
   *            - when edgeTest == b5 (or i6 or i7 or b1) then the edge
   *              b1 will be returned,
   *            - when edgeTest == b2 (or i3 or b4) then the edge
   *              b4 will be returned,
   *          Eventually, when edgeTest is absent, the result shall
   *          depend on the position of "this" in the Onext() ring().
   */

  // Be sure the Onext ring isn't already full
  if( this->IsOrgInternal( ) ) 
    {
    itkQEDebugMacro( "Internal point." );
    return( 0 );
    }

  // Update reference
  edgeTest = ( !edgeTest )? this: edgeTest;

  // On efficiency purposes
  if( edgeTest->IsIsolated( ) )
    {
    return( edgeTest );
    }

  // Ok, no more special cases
  IteratorGeom it = edgeTest->BeginGeomOnext( );
  for( ; it != edgeTest->EndGeomOnext( ); it++ )
    {
    if( !it.Value( )->IsLeftSet( ) )
      {
      return( it.Value( ) );
      }
    }

  // No border edge found
  itkQEDebugMacro( "Unfound border edge." );
  return( 0 );
}

/**
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    bool GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    InsertAfterNextBorderEdgeWithUnsetLeft( Self* isol, Self* hint )
{
  // When the geometry of isol is set it must match the
  // one of "this" Org(). If the geometry is not set, we assume
  // that both Org are the same, regardless their actual value.
  // Note: The purpose of this test is to avoid introducing some
  //       incoherence in the geometry at Org().
  // The things should go this way:
  // 1/ when the geometry of "this" Org is not set, then be paranoid
  //    and suspect the situation is allready snafu:
  //    1a/ if all edges of "this" Onext ring have an unset Org()
  //        (the situation is coherent), then proceed (Result=0)
  //        whatever the value of isol.Org() might be.
  //    1b/ if one of the edges of "this" Onext ring has an Org() set,
  //        then we deduce that there is allready some geometrical
  //        incoherence at this->Org() and exit this method (Result=1).
  // 2/ Then when we didn't exit at stage 1, consider isol.Org():
  //    2a/ when isol.Org() is absent proceed (result=0),
  //    2b/ when isol.Org() is present and Org == isol.OrgSet then
  //        proceed (result=0),
  //    2c/ when isol.Org() is present and Org != isol.OrgSet then
  //        exit (result=1).
  //
  // Here is what is implemented:
  // +--------+-------------+--------------------+--------+
  // | OrgSet | isol.OrgSet | Org == isol.OrgSet | Result |
  // +--------+-------------+--------------------+--------+
  // |   0    |      0      |        0           |    0   |
  // |   0    |      0      |        1           |    0   |
  // |   0    |      1      |        0           |    1   |
  // |   0    |      1      |        1           |    1   |
  // +--------+-------------+--------------------+--------+
  // |   1    |      0      |        0           |    1   |
  // |   1    |      0      |        1           |    1   |
  // |   1    |      1      |        0           |    1   |
  // |   1    |      1      |        1           |    0   |
  // +--------+-------------+--------------------+--------+
  //
  if( !(   !( IsOrgSet( ) || isol->IsOrgSet( ) )
         || (    IsOrgSet( )
              && isol->IsOrgSet( )
              && ( m_Org == isol->m_Org ) )
        ) 
    )
    {
    itkQEDebugMacro( "Isolated Org() differs from this Org." );
    return( false );
    }

  // Find out if this point has some room left for edge insertion:
  Self* edgeAfter = this->GetNextBorderEdgeWithUnsetLeft( hint );
  if( !edgeAfter ) 
    {
    itkQEDebugMacro( "This point is yet surrounded by faces." );
    return( false );
    }

  // Normally, an edge was found
  edgeAfter->Splice( isol );
  return( true );
}

/**
 */
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    bool GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    ReorderOnextRingBeforeAddFace( Self* second )
{
  /* Assume "this->Orgv()" is a boundary point P that is thrice adjacent
   * to noface and consider the given situation is the one depicted by
   * the following diagram where:
   *   - P is "this->Orgv()" instance,
   *   - the * (star) indicates the presence of a face,
   *   - b1, b2, b3, b4, b5, b6 denote boundary edges,
   *   - p denotes some generic point,
   *   - A and B denote some specific points we want to discuss,
   *   - the Onext() ring order is represented counter-clockwise
   *     [which is coherent with the definition of edge->GetRigth()]
   *     i.e. the ordering of the edges is:
   *          b1, b2, b3, b4, b5, b6, b1...
   *
   *                    p       N       p
   *                   / \      O      / \
   *                  /   \           /   \
   *                 /     \    F    /     \         counter-clockwise
   *                /      b3   A   b2      \        Onext() ring order
   *               /         \  C  /         \
   *              /     *     \ E /     *     \
   *             /             \ /             \
   *             A------b4------P------b1-------B
   *                           / \
   *                          /   \
   *             NO FACE     /     \      NO FACE
   *                        /       \
   *                      b5         b6
   *                      /     *     \
   *                     /             \
   *                    p---------------p
   *
   * At P this Mesh doesn't represent a 2-manifold (since we are thrice
   * on the boundary). Nevertheless such a situation could arise in
   * intermediary stages (e.g. when building the Mesh, or during
   * surgical changes on the Mesh).
   *    Now, assume we are asked to build the triangle [P, A, B]. Note
   * that this request is not absurd since the current situation at
   * P isn't the one of a 2-manifold: hence when building the current
   * Onext() ring of P, we had not enough information to decide
   * wheter b4.Onext() should be b5 or b1. It is ONLY when we are
   * required to build the triangle [P, A, B] that we have the
   * additional information that b4.Onext() is indeed b1.
   *    When we are required to build triangle [P, A, B], we hence
   * need to change the Onext() ring order at P, i.e. we need to deal
   * with the triangle [P, b5, b6] which currently prevents
   * b4.Onext() to be b1. In other terms, when considering the
   * additional information that b4.Onext() is b1, and before
   * building the triangle [P, A, B], we need to reorder
   * the Onext() ring of P from it's current state
   *    b1, b2, b3, b4, b5, b6, b1...
   * to an order coherent with the [P, A, B] request, i.e.
   *     b1, b2, b5, b6, b3, b4, b1...
   *
   * In order to establish the "proper" Onext() ring at P we use
   * two Splice operations. The algorithm goes:
   *   - first disconnect the piece of the surface containing the edge
   *     [PB] (it would be the same process if we chose [PA]) from
   *     the Onext() ring at P.
   *   - second, re-integrate the disconnected piece at the desired
   *     location i.e. side by side with [PA] (respectively [PB] if
   *     we chose [PA] at first stage).
   * By "piece of surface containing the edge [PB]" we mean [all]
   * the triangle[s] starting at [PB] in the Onext() order and
   * having a left face set.
   *
   * We can illustrate this process on bit more general diagram than 
   * the last case (where the "piece of surface containing the edge
   * [PB]" is constituted by two triangles) and when using
   * the arguments of this method (i.e. [PA] = this and [PB] = second).
   * The initial stage is the following (we note first=this=[PA] and
   * second=[PB]) where the Onext() ring order is:
   *     first, b2, b3, second, b5, bsplice, b7, first...
   *
   *                    p       N       A
   *                   / \      O      / \
   *                  /   \           /   \
   *                 /     \    F    /     \       counter-clockwise
   *                /      b2   A  first    \      Onext() ring order
   *               /         \  C  /         \
   *              /     *     \ E /     *     \
   *             /             \ /             \
   *            p-------b3------P------b7-------p
   *                           /|\
   *                          / | \
   *          NO FACE        /  |  \      NO FACE
   *                        /   |   \
   *                  second   b5   bsplice
   *                      /  *  |  *  \
   *                     /      |      \
   *                    B-------p-------p
   *
   * The first stage, implemented as
   *     second->Oprev()->Splice( bsplice ),
   * yields the following diagram:
   *
   *                    p       N       A
   *                   / \      O      / \
   *                  /   \     F     /   \
   *                 /     \    A    /     \        counter-clockwise
   *                /      b2   C  first    \       Onext() ring order
   *               /         \  E  /         \ 
   *              /     *     \   /     *     \
   *             /             \ /             \
   *            p-------b3------P------b7-------p
   * 
   *                         NOFACE
   *
   *                           /|\
   *                          / | \
   *                         /  |  \
   *                        /   |   \
   *                  second   b5   bsplice
   *                      /  *  |  *  \
   *                     /      |      \
   *                    B-------p-------p
   *
   * and the second stage, implemented as
   *      first->Splice( bsplice ),
   * yields the following diagram:
   *
   *                                    A
   *         B__        NO FACE        / \
   *         |  \__                   /   \
   *         |     \__               /     \       counter-
   *         |      second         first    \      clockwise for all
   *         |           \__       /         \
   *         |     *        \__   /     *     \
   *         |                 \ /             \
   *         p-------b5---------P------b7-------p
   *         |               __/|\
   *         |     *      __/   | \
   *         |           /      |  \      NO FACE
   *         |     bsplice      |   \
   *         |   __/           b2    b3
   *         p__/               |  *  \
   *                NO FACE     |      \
   *                            p-------p
   */
  Self* first = this;
  Self* bsplice = 0;

  // Making sure point adjacency is correct:
  if( first->GetOrg( ) != second->GetOrg( ) )
    {
    itkQEDebugMacro( "Edges not adjacent at same point!" );
    return( false );
    } 

  if( first->GetOnext( ) == second )
    {
    itkQEDebugMacro( "Nothing to be done." );
    return( true );
    } 

  if( first->IsLeftSet( ) )
    {
    itkQEDebugMacro( "First should NOT have a left face." );
    return( false );
    } 

  if( second->IsInternal( ) )
    {
    itkQEDebugMacro( "Second is an internal edge." );
    return( false );
    } 

  // Disconnect the triangles containing second:
  if( second->IsLeftSet( ) )
    {
    bsplice = second->GetNextBorderEdgeWithUnsetLeft( );
    second->GetOprev( )->Splice( bsplice );
    }
  else
    {
    // Orientation is localy clockwise:
    itkQEDebugMacro( "Clockwise orientation case." );
    itkQEWarningMacro( "This code was never tested (it requires "
                       "heterogenously oriented triangles)." );
    bsplice = second;
    second->GetOprev( )->Splice( bsplice );
    }
 
  // Reconnect second after first:
  first->Splice( bsplice );
  return( true );
}

// ---------------------------------------------------------------------
template< typename TVRef, typename TFRef,
          typename TPRef, typename TDRef, bool PrimalDual >
    void GeometricalQuadEdge< TVRef, TFRef, TPRef, TDRef, PrimalDual >::
    Disconnect( )
{
  if( this->IsDisconnected( ) )
    {
    itkQEDebugMacro( "Edge already disconnected." );
    return;
    } 

  // Update faces if the edge isn't a wire
  if( this->IsAtBorder( ) )
    {
    Self* e = ( this->IsRightSet( ) )? this->GetSym( ): this;
    IteratorGeom it = e->BeginGeomLnext( );
    for( ; it != e->EndGeomLnext( ); it++ )
      {
      it.Value( )->UnsetLeft( );
      }
    }
  else if( this->IsInternal( ) )
    {
    // Consolidate face
    DualOrgRefType face = this->GetRight( );
    for( IteratorGeom it  = this->BeginGeomLnext( );
                      it != this->EndGeomLnext( );
                      it++ )
      {
      it.Value( )->SetLeft( face );
      }
    } 

  // Hint edges
  Self* e0 = this->GetOprev( );
  Self* e1 = this->GetLnext( );

  // Disconnect entries
  if( !this->IsOrgDisconnected( ) )
    {
    e0->Splice( this );
    }
  if( !this->IsDestDisconnected( ) )
    {
    e1->Splice( this->GetSym( ) );
    }

  // Normally, this edge is converted to a simple wire
  this->UnsetOrg( );
  this->UnsetDest( );
  this->UnsetLeft( );
  this->UnsetRight( );
}

} 

#endif 

