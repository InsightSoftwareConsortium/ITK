/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMesh.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMesh_txx
#define __itkQuadEdgeMesh_txx

#include <vector>

namespace itk
{

template< typename TPixel, unsigned int VDimension, typename TTraits >
const typename QuadEdgeMesh< TPixel, VDimension, TTraits >::PointIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >::m_NoPoint =
QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal::m_NoPoint;

template< typename TPixel, unsigned int VDimension, typename TTraits >
const typename QuadEdgeMesh< TPixel, VDimension, TTraits >::CellIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >::m_NoFace =
QuadEdgeMesh< TPixel, VDimension, TTraits >::QEDual::m_NoPoint;

/**
 * Clear all this mesh by deleting all contained edges which as
 * a side effect deletes adjacent faces
 */
template< typename TPixel, unsigned int VDimension, typename TTraits >
void 
QuadEdgeMesh< TPixel, VDimension, TTraits >
::Clear()
{
  if( this->GetCells() )
    {
    // First delete all the edges (since it also clears adjacent faces).
    itkQEMeshForAllPrimalEdgesMacro( Self, this, edgeToDelete )
      {
      this->LightWeightDeleteEdge( edgeToDelete );
      }
    itkQEMeshForAllPrimalEdgesEndMacro
    }

 // Clear the points potentialy left behind by LightWeightDeleteEdge():
 this->GetPoints()->clear();
}

/**
 * \brief The one and only method to modify the edge connectivity.
 */
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::PointIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >
::Splice( QEPrimal* a, QEPrimal* b )
{
  bool SplitingOrigin = a->IsInOnextRing( b );
  PointIdentifier resultingOriginId;

  if( SplitingOrigin )
    {
    // see TODO's entry dated 2006-01-24
    /* We consider the following situation which depicts the Onext()
     * ring around the point Origin (which is both a->GetOrigin() and
     * b->GetOrigin():
     *
     *              \         /
     *               \       /
     *               e3     e2              counter-clockwise
     *                 \   /                Onext() order.
     *                  \ /
     *      -----b------Org------a----
     *                  / \
     *                 /   \
     *                /     \
     *               /       \
     *              /         \
     *            e5           e6
     *            /             \
     *           /               \
     *
     * The result of this method is then:
     *
     *         \         /
     *          \       /
     *          e3     e2
     *            \   /
     *             \ /
     * ----b------newOrg
     *
     *                  Org------a-----
     *                  / \
     *                 /   \
     *                /     \
     *               /       \
     *              /         \
     *            e5           e7
     *            /             \
     *           /               \
     */

    // Handle connectivity at QEQuadEdge level:
    a->Splice( b );

    ////////// Handle the geometrical references:
    // Make sure the Origin's edge entry doesn't point to an entry edge
    // that isn't any more in the Onext ring:
    PointIdentifier orgId = a->GetOrigin();
    PointType org = this->GetPoint( orgId );
    org.SetEdge( a );
    this->SetPoint( orgId, org );

    // Create a newOrigin point by duplicating the geometry of Origin...
    PointType newOrigin  = org;
    newOrigin.SetEdge( b );
    PointIdentifier newOriginId = this->AddPoint( newOrigin );

    // ...and inform Onext ring of b that their Origin() have changed:
    typename QEPrimal::IteratorGeom it;
    for( it = b->BeginGeomOnext(); it != b->EndGeomOnext(); it++ )
      {
      it.Value()->SetOrigin( newOriginId );
      }
      resultingOriginId = newOriginId;
   }
 else
   {
   // see TODO's entry dated 2006-01-24
   /* We consider the following situation which depicts the Onext()
    * rings around the point Origin = a->GetOrigin() and
    * oldOrigin = b->GetOrigin():
    *
    *         \         /
    *          \       /
    *          e3     e2
    *            \   /
    *             \ /
    * ----b------oldOrg
    *
    *                  Org------a-----
    *                  / \
    *                 /   \
    *                /     \
    *               /       \
    *              /         \
    *            e5           e7
    *            /             \
    *           /               \
    *
    *
    * The result of this method is then:
    *
    *              \         /
    *               \       /
    *               e3     e2              counter-clockwise
    *                 \   /                Onext() order.
    *                  \ /
    *      -----b------Org------a----
    *                  / \
    *                 /   \
    *                /     \
    *               /       \
    *              /         \
    *            e5           e6
    *            /             \
    *           /               \
    *
    * Note: in this case we must handle the geometry first and
    *       then the connectivity.
    */

    // Since this is the geometrical version of Splice() we
    // have additional geometrical information that we should use
    // to check the correctness of the situation.

    /////////////////////////////////////////////////////////////
    // First, consider the vertices: Origin and oldOrigin must be different.
    PointIdentifier oldOriginId = b->GetOrigin();
    PointIdentifier orgId = a->GetOrigin();

    if( oldOriginId == orgId )
      {
      itkWarningMacro( "Trying to fuse the same point!" );
      return m_NoPoint;
      }

    /** \todo Compare the geometry of the two points and accept
     * splicing when their geometry matches. We could fix
     * an epsilon threshold distance above which the two points
     * are considered distinct.
     */
    PointType oldOrigin = this->GetPoint( oldOriginId );
    PointType org = this->GetPoint( orgId );

    /////////////////////////////////////////////////////////////
    /* We are done with the vertices and we might need to consider the
     * possible initial adjacent face[s]. We shall accept to proceed
     * with Splicing if and only if the following conditions are met:
     * [1] a and b both share the SAME Left face,
     * [2] a and b and in the same Lnext() ring,
     * [3] a and b are not too close followers in the Lnext() ring
     *    [this is to avoid to create a face with only two edges which
     *     is equivalent to two different edges adjacent to the same two
     *     vertices].
     *
     *                   V ---<-b---- V
     *                  /              \
     *                 /                \
     *                /              a.Lnext().Lnext()
     *               /                    \
     *              /        Face          \
     *             V                        V
     *              \     a.Splice(b)      /
     *               \     is OK          /
     *                \               a.Lnext()
     *                 \                /
     *                  \              /
     *                   V ----a->--- V
     *
     * Basically, we accept to proceed with spliting if there is a
     * single face on the left and this face is at least an hexagone
     * and the vertices we wish to splice are at least two vertices aside.
     */

    FaceRefType aLeftFace = a->GetLeft();
    FaceRefType bLeftFace = b->GetLeft();

    bool MustReconstructFace = false;
    if( ( aLeftFace == m_NoFace && bLeftFace != m_NoFace )
        || ( aLeftFace != m_NoFace && bLeftFace == m_NoFace ) )
      {
      itkWarningMacro("Face on one side but not the other. Cancel.");
      return m_NoPoint;
      }

    if( aLeftFace != m_NoFace && bLeftFace != m_NoFace )
      {
      if( ( aLeftFace == bLeftFace ) && ( a->GetLnext() != b )
          && ( a->GetLnext()->GetLnext() != b )
          && ( b->GetLnext() != a )
          && ( b->GetLnext()->GetLnext() != a )
          && ( a->IsInLnextRing( b ) )
          && ( b->IsInLnextRing( a ) ) )
        {
        this->DeleteFace( aLeftFace );
        MustReconstructFace = true;
        }
      else
        {
        itkWarningMacro( "Face is not at least and hexagon." );
        return( m_NoPoint );
        }
      }

    // Notice that when aLeftFace == m_NoFace and bLeftFace == m_NoFace
    // we simply proceed... (with MustReconstructFace initialy set to
    // false.

    ///////////////////////////////////////////////////////////////
    // Handle connectivity at QEQuadEdge level:
    a->Splice( b );

    ///////////////////////////////////////////////////////////////
    // Back to dealing with the geometrical references. First
    // make sure the oldOrigin's edge entry won't be used any more:
    oldOrigin.SetEdge( (QEPrimal*)0 );
    this->SetPoint( oldOriginId, oldOrigin );

    // We need to inform the edges ranging from a->Onext() to b that
    // their Origin() have changed. Let's over do it (read, be lazy) and
    // inform the full Onext() ring:
    typename QEPrimal::IteratorGeom it;
    for( it = a->BeginGeomOnext(); it != a->EndGeomOnext(); it++ )
      {
      it.Value()->SetOrigin( orgId );
      }
    resultingOriginId = oldOriginId;

    ///////////////////////////////////////////////////////////////
    // Now that we are done with the handling of the geometry of
    // vertices proceed with the geometry of the faces. When we
    // are spliting a face (through Splicing) we must construct two
    // new faces:
    if( MustReconstructFace )
      {
      this->AddFace( a );
      this->AddFace( b );
      }
  }

  this->Modified();
  return resultingOriginId;
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
bool QuadEdgeMesh< TPixel, VDimension, TTraits >
::FindClosestPoint( 
 const CoordRepArrayType coords, PointIdentifier & pointId ) const
{
  VectorType vP;
  vP.Get_vnl_vector().copy_in( coords );
  PointsContainerConstIterator pit = this->GetPoints()->Begin();
  pointId = pit.Index();

  while( pit != this->GetPoints()->End() )
    {
    VectorType v0 = pit.Value().GetVectorFromOrigin();
    VectorType v1 = this->GetVector( pointId );

    if( ( v0 - vP ).GetNorm() < ( v1 - vP ).GetNorm() )
      {
      pointId = pit.Index();
      }
    pit++;
    }

  return true;
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
void QuadEdgeMesh< TPixel, VDimension, TTraits >
::SetCell( CellIdentifier cId, CellAutoPointer& cell )
{
  (void)cId;

  EdgeCellType* qe = (EdgeCellType*) NULL;
  PolygonCellType* pe = (PolygonCellType*) NULL;

  if( ( qe = dynamic_cast< EdgeCellType* >( cell.GetPointer() ) ) )
    {
    this->AddEdge( qe->GetOrigin(), qe->GetDestination() );
    }
  else if( ( pe = dynamic_cast< PolygonCellType* >( cell.GetPointer() ) ) )
    {
    PointIdList points;

    typename PolygonCellType::PointIdIterator pit = pe->PointIdsBegin();

    while( pit != pe->PointIdsEnd() )
      {
      points.push_back( *pit );
      pit++;
      }

    this->AddFace( points );
    }
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::PointIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >
::FindFirstUnusedPointIndex()
{
  PointIdentifier pid;

  if( m_FreePointIndexes.size() == 0 )
    {
    pid = this->GetNumberOfPoints();

    if( pid != 0 )
      {
      PointsContainerIterator last = this->GetPoints()->End();
      last--;
      pid = last.Index() + 1;
      }
    }
  else
    {
    pid = m_FreePointIndexes.front();
    m_FreePointIndexes.pop();
    }

  return pid;
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::PointIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >
::AddPoint( const PointType& p )
{
  PointIdentifier pid = this->FindFirstUnusedPointIndex();
  this->SetPoint( pid, p );
  return pid;
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
void QuadEdgeMesh< TPixel, VDimension, TTraits >
::DeletePoint( const PointIdentifier& pid )
{
  if( this->GetPoint( pid ).GetEdge() )
    {
    itkDebugMacro("Point is not isolated.");
    return;
    }

  this->GetPoints()->DeleteIndex( pid );
  m_FreePointIndexes.push( pid );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::PointType
QuadEdgeMesh< TPixel, VDimension, TTraits >
::GetPoint( const PointIdentifier& pid ) const
{
  return this->GetPoints()->GetElement( pid );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::VectorType
QuadEdgeMesh< TPixel, VDimension, TTraits >
::GetVector( const PointIdentifier& pid ) const
{
  return this->GetPoint( pid ).GetVectorFromOrigin();
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::CellIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >
::FindFirstUnusedCellIndex()
{
  CellIdentifier cid;

  if( m_FreeCellIndexes.size() == 0 )
    {
    cid = this->GetNumberOfCells();

    if( cid != 0 )
      {
      CellsContainerIterator last = this->GetCells()->End();
      last--;
      cid = last.Index() + 1;
      }
    }
  else
    {
    cid = m_FreeCellIndexes.front();
    m_FreeCellIndexes.pop();
    }

  return cid;
}

/**
*\brief  Construct a new edge ending at points with identifiers given
*        as arguments.
* @param  orgPid first endpoint (origin) of the edge to Add.
* @param destPid second endpoint (destination) of the edge to Add.
* @sa \ref GeometricalQuadEdge::InsertAfterNextBorderEdgeWithUnsetLeft
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >
::AddEdge( const PointIdentifier& orgPid, const PointIdentifier& destPid )
{
  // Make sure the points are different
  if( orgPid == destPid )
    {
    itkDebugMacro( "Creating an edge between the same point." );
    return (QEPrimal*)NULL;
    }

  // Make sure the points are allready in the QuadEdgeMesh container:
  if( !( this->GetPoints()->IndexExists( orgPid ) ) ||
      !( this->GetPoints()->IndexExists( destPid ) ) )
    {
    itkDebugMacro( "One of the points not in the PointSet." );
    return (QEPrimal*)NULL;
    }

  // Make sure the edge is not allready in the container
  QEPrimal* e = this->FindEdge( orgPid, destPid );
  if( e != (QEPrimal*)NULL )
    {
    itkDebugMacro("Edge already in QuadEdgeMesh.");
    return e;
    }

  // Check if the points have room to receive a new edge
  QEPrimal*  eOrigin = this->FindEdge(  orgPid );
  QEPrimal* eDestination = this->FindEdge( destPid );

  if( eOrigin && eOrigin->IsOriginInternal() )
    {
    itkDebugMacro("No room for a new edge in the Origin() ring.");
    return (QEPrimal*)NULL;
    }

  if( eDestination && eDestination->IsOriginInternal() )
    {
    itkDebugMacro("No room for a new edge in the Destination() ring.");
    return (QEPrimal*)NULL;
    }

  // Ok, there's room and the points exist
  EdgeCellType* newEdge = new EdgeCellType( true );

  newEdge->SetOrigin (  orgPid );
  newEdge->SetDestination( destPid );

  if( !eOrigin )
    {
    PointType pOrigin = this->GetPoint( orgPid );
    pOrigin.SetEdge( newEdge );
    this->SetPoint( orgPid, pOrigin );
    }
  else
    {
    eOrigin->InsertAfterNextBorderEdgeWithUnsetLeft( newEdge );
    }

  if( !eDestination )
    {
    PointType pDestination = this->GetPoint( destPid );
    pDestination.SetEdge( newEdge->GetSym() );
    this->SetPoint( destPid, pDestination );
    }
  else
    {
    eDestination->InsertAfterNextBorderEdgeWithUnsetLeft( newEdge->GetSym() );
    }

  // Add it to the container
  this->PushOnContainer( newEdge );

  return newEdge;
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
void 
QuadEdgeMesh< TPixel, VDimension, TTraits >
::PushOnContainer( EdgeCellType* newEdge )
{
  // Add it to the container
  CellIdentifier eid = this->FindFirstUnusedCellIndex();
  newEdge->SetIdent( eid );
  newEdge->GetSym()->SetIdent( eid );
  CellAutoPointer edge;
  edge.TakeOwnership( newEdge );
  this->Superclass::SetCell( eid, edge );
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
void 
QuadEdgeMesh< TPixel, VDimension, TTraits >
::DeleteEdge( const PointIdentifier& orgPid, const PointIdentifier& destPid )
{
  // Check if the edge exists
  QEPrimal* e = this->FindEdge( orgPid, destPid );

  if( e == (QEPrimal*)NULL )
    {
    itkDebugMacro("Edge missing in mesh.");
    return;
    }

  this->DeleteEdge( e );
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
void 
QuadEdgeMesh< TPixel, VDimension, TTraits >
::
DeleteEdge( QEPrimal* e )
{
  const PointIdentifier& orgPid  = e->GetOrigin();
  const PointIdentifier& destPid = e->GetDestination();

  // Check if the Origin point's edge ring entry should be changed
  PointType pOrigin = this->GetPoint( orgPid );
  if( pOrigin.GetEdge() == e )
    {
    if( !e->IsOriginDisconnected() )
      {
      pOrigin.SetEdge( e->GetOprev() );
      }
    else
      {
      pOrigin.SetEdge( (QEPrimal*)NULL );
      }

    this->SetPoint( orgPid, pOrigin );
    }

  // Same for the Destination point
  PointType pDestination = this->GetPoint( destPid );

  if( pDestination.GetEdge() == e->GetSym() )
    {
    if( !e->IsDestinationDisconnected() )
      {
      pDestination.SetEdge( e->GetLnext() );
      }
    else
      {
      pDestination.SetEdge( (QEPrimal*)0 );
      }

    this->SetPoint( destPid, pDestination );
    }

  // This container serves to avoid the MS .net bug when
  // one wants to delete a map element using a map::iterator.
  // Normally, if we delete a map element using an iterator,
  // it should keep the iterator validity but .net doesn't
  // like it, so we delay the cell deletion to a later loop.
  typedef std::vector< CellIdentifier > DeleteCellsCont;
  DeleteCellsCont cellsToDelete;

  // Delete all references to 'e' in the container
  CellsContainerIterator cit = this->GetCells()->Begin();

  while( cit != this->GetCells()->End() )
    {
    EdgeCellType* cell = dynamic_cast< EdgeCellType* >( cit.Value() );
    PolygonCellType* pcell = dynamic_cast< PolygonCellType* >(cit.Value());
    bool toDelete = false;

    if( cell != (EdgeCellType*)NULL )
      {
      QEPrimal* edge = dynamic_cast< QEPrimal* >( cell );
      toDelete = ( edge == e || edge->GetSym() == e );
      }
    else if( pcell != (PolygonCellType*)0 )
      {
      QEPrimal* edge = pcell->GetEdgeRingEntry();
      typename QEPrimal::IteratorGeom it = edge->BeginGeomLnext();

      while( it != edge->EndGeomLnext() && !toDelete )
        {
        toDelete = ( ( it.Value() == e ) ||
                         ( it.Value()->GetSym() == e ) );
        it++;
        }

      // Unset left faces
      if( toDelete )
        {
        for( it = edge->BeginGeomLnext();
            it != edge->EndGeomLnext();
            it++ )
          {
          it.Value()->UnsetLeft();
          }
        }
      }

    if( toDelete )
      {
      cellsToDelete.push_back( cit.Index() );
      m_FreeCellIndexes.push( cit.Index() );
      }
    cit++;
    }

  // Delete the elements in the map
  typename DeleteCellsCont::iterator dit = cellsToDelete.begin();

  while( dit != cellsToDelete.end() )
    {
    this->GetCells()->DeleteIndex( *dit );
    dit++;
    }

  // Now, disconnect it and let the garbage collector do the rest
  e->Disconnect();
  this->Modified();
}

/**
* Delete the incoming edge and all LOCAL references to this edge.
* By local we mean the ones we can reasonably be aware of i.e.
* the adjacent faces (that we also delete) and the adjacent points
* (when the incoming edge is their Onext ring entry).
* This is to be opposed to \ref DeleteEdge that searches for ALL
* references to the incoming edge (which is a much heavier process
* because one as to make an exhaustive search in the CellContainer).
* \note: when deleting the adjacent faces we also handle the
*        suppression of the references to those faces in the Lnext()
*        and Rnext() rings.
* \warning Nothing is done to remove the potential isolated points
*        left by this edge deletion (the caller might want to recycle
*        them). Hence it is the caller's responsability to manage the
*        clean-up of adjacent points (when necessary).
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
void 
QuadEdgeMesh< TPixel, VDimension, TTraits >
::LightWeightDeleteEdge( QEPrimal* e )
{
  /////////////////////////////////////////////////////////////////
  // First make sure the points are not pointing to the edge we are
  // trying to delete.
  const PointIdentifier& orgPid  = e->GetOrigin();
  const PointIdentifier& destPid = e->GetDestination();
  // Check if the Origin point's edge ring entry is the edge we are
  // trying to delete. When this is the case shift the Origin edge entry
  // to another edge and when no other edge is available leave it
  // to NULL.
  PointType pOrigin = this->GetPoint( orgPid );

  if( pOrigin.GetEdge() == e )
    {
    if( !e->IsOriginDisconnected() )
      {
      pOrigin.SetEdge( e->GetOprev() );
      }
    else
      {
      pOrigin.SetEdge( (QEPrimal*)NULL );
      }

    this->SetPoint( orgPid, pOrigin );
    }

  // Same thing for the Destination point:
  PointType pDestination = this->GetPoint( destPid );

  if( pDestination.GetEdge() == e->GetSym() )
    {
    if( !e->IsDestinationDisconnected() )
      {
      pDestination.SetEdge( e->GetLnext() );
      }
    else
      {
      pDestination.SetEdge( (QEPrimal*)NULL );
      }

    this->SetPoint( destPid, pDestination );
    }

  /////////////////////////////////////////////////////////////////
  // Second we need to destroy the adjacent faces (both GetLeft()
  // and GetRight() when they exist) because their very definition
  // makes reference to the edge we are trying to delete:
  if( e->IsLeftSet() )
    {
    this->DeleteFace( e->GetLeft() );
    }

  if( e->IsRightSet() )
    {
    this->DeleteFace( e->GetRight() );
    }

  /////////////////////////////////////////////////////////////////
  // Third we need to remove from the container the EdgeCell
  // representing the edge we are trying to destroy at the itk
  // level. For this we first get our hands back on the EdgeCell
  // by upcasting to EdgeCellType*:
  EdgeCellType* edgeCell = dynamic_cast< EdgeCellType* > ( e );

  if( !edgeCell )
    {
    edgeCell = dynamic_cast< EdgeCellType* > ( e->GetSym() );
    }

  if( !edgeCell )
    {
    itkWarningMacro("Neither e nor e->Sym() are upcastable");
    return;
    }

  this->GetCells()->DeleteIndex( edgeCell->GetIdent() );

  // Eventually, we disconnect (at the QuadEdge level) the edge we
  // are trying to delete and let the garbage collector do the rest:
  e->Disconnect();
  this->Modified();
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
void
QuadEdgeMesh< TPixel, VDimension, TTraits >
::DeleteFace( FaceRefType faceToDelete )
{
  PolygonCellType* cellToDelete = dynamic_cast< PolygonCellType* >
                     ( this->GetCells()->GetElement( faceToDelete ) );

  if( !cellToDelete )
    {
    itkDebugMacro( "No such face in container" );
    return;
    }

  // Iterate on the edges adjacent to face and remove references to
  // to this face:
  QEPrimal* e = cellToDelete->GetEdgeRingEntry();

  if( faceToDelete != e->GetLeft() )
    {
    e = e->GetSym();
    }

  if( faceToDelete != e->GetLeft() )
    {
    itkWarningMacro("Neither e nor e->Sym() are the correct face");
    return;
    }

  typename QEPrimal::IteratorGeom it;

  for( it = e->BeginGeomLnext(); it != e->EndGeomLnext(); it++ )
    {
    it.Value()->UnsetLeft();
    }

  this->GetCells()->DeleteIndex( faceToDelete );
  this->Modified();
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >
::GetEdge() const
{
  QEPrimal* e = (QEPrimal*)NULL;
  CellsContainerIterator cit = this->GetCells()->Begin();

  while( cit != this->GetCells()->End() && e == (QEPrimal*)0 )
    {
    e = dynamic_cast< QEPrimal* >( cit.Value() );

    // Check if we can get an edge from a potential polygon
    if( e == (QEPrimal*)NULL )
      {
      PolygonCellType* pol =
        dynamic_cast< PolygonCellType* >( cit.Value() );
      e = ( pol )?
        dynamic_cast< QEPrimal* >( pol->GetEdgeRingEntry() ): e;
      }
    cit++;
    }

  return e;
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >
::GetEdge( const CellIdentifier& eid ) const
{
  QEPrimal* cell = ( dynamic_cast< QEPrimal* >
      ( this->GetCells()->GetElement( eid ) ) );

  return cell;
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >
::FindEdge( const PointIdentifier& pid0 ) const
{
  PointType p = this->GetPoint( pid0 );
  const QEPrimal * edge =
    dynamic_cast< const QEPrimal* >( p.GetEdge() );
  return const_cast< QEPrimal * >( edge ); 
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >
::FindEdge( const PointIdentifier& pid0, const PointIdentifier& pid1 ) const
{
  QEPrimal * initialEdge = this->FindEdge( pid0 );

  QEPrimal * edgeFound = static_cast< QEPrimal * >(NULL);

  if( initialEdge )
    {
    typename QEPrimal::IteratorGeom it = initialEdge->BeginGeomOnext();

    while( it != initialEdge->EndGeomOnext() )
      {
      if(  it.Value()->GetDestination() == pid1 )
        {
        edgeFound = dynamic_cast< QEPrimal* >( it.Value() );
        break;
        }
      ++it;
      }
    
    }

  return edgeFound;
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >
::AddFace( const PointIdList& points )
{
  // Check that there are no duplicate points
  for(unsigned int i=0; i < points.size(); i++)
    {
    typename PointIdList::const_iterator itr = points.begin();
    typename PointIdList::const_iterator end = points.end();
    typename PointIdList::size_type count = 0;
    const PointIdentifier pointId = points[i];
    while( itr != end )
      {
      if( *itr == pointId )
        {
        ++count;
        }
      ++itr;
      }
    if( count != 1 )
      {
      itkDebugMacro("Point "<<i<<" is duplicated");
      return (QEPrimal*) NULL;
      }
    }

  // Check that all points exist
  for(unsigned int i=0; i < points.size(); i++)
    {
    if( !this->GetPoints()->IndexExists( points[i] ) )
      {
      itkDebugMacro("Point "<<i<<" is missing in the mesh");
      return (QEPrimal*) NULL;
      }
    }

  // Check if edges have no face on the left.
  for(unsigned int i=0; i < points.size(); i++)
    {
    PointIdentifier pid0 = points[i];
    PointIdentifier pid1 = points[ (i+1) % points.size() ];

    QEPrimal* edge = this->FindEdge( pid0, pid1 );

    if( edge )
      {
      if( edge->IsLeftSet() )
        {
        itkDebugMacro("Edge [" << i << " " << ((i+1) % points.size())
          <<" has a left face.");
        return (QEPrimal*) NULL;
        }
      }
    }

  // Now create edges as needed.
  for(unsigned int i=0; i < points.size(); i++)
    {
    PointIdentifier pid0 = points[i];
    PointIdentifier pid1 = points[ (i+1) % points.size() ];
    QEPrimal* edge = this->FindEdge( pid0, pid1 );

    if( !edge )
      {
      this->AddEdge( pid0, pid1 );
      }
    }

  // Reorder all Onext rings
  for(unsigned int i=0; i < points.size(); i++)
    {
    PointIdentifier pid0 = points[ (i+points.size()-1) % points.size() ];
    PointIdentifier pid1 = points[i];
    PointIdentifier pid2 = points[ (i+1) % points.size() ];

    QEPrimal* e0 = this->FindEdge( pid1, pid2 );
    QEPrimal* e1 = this->FindEdge( pid1, pid0 );

    e0->ReorderOnextRingBeforeAddFace( e1 );
    }

  // all edges are ready to receive a face on the left
  QEPrimal* entry = this->FindEdge( points[0], points[1] );

  if( !entry )
    {
    itkDebugMacro("entry == NULL");
    return (QEPrimal*) NULL;
    }

  this->AddFace(entry);

  return entry;
}

/**
* We here make the strong assumption that the caller was wise enough
* to build/handle the connectivity at the QE level. This method
* simply creates a new PolygonCell and assigns it as the left face
* of all edges in the Lnext ring of the incoming argument.
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
void
QuadEdgeMesh< TPixel, VDimension, TTraits >
::AddFace( QEPrimal* entry )
{
  // Create the cell and add it to the container
  PolygonCellType* faceCell = new PolygonCellType( entry );
  CellIdentifier fid = this->FindFirstUnusedCellIndex();
  faceCell->SetIdent( fid );

  // Associate the above generated CellIndex as the default FaceRefType
  // of the new face [ i.e. use the itk level CellIdentifier as the
  // GeometricalQuadEdge::m_Origin of dual edges (edges of type QEDual) ].
  typename QEPrimal::IteratorGeom it;
  for( it = entry->BeginGeomLnext(); it != entry->EndGeomLnext(); it++ )
    {
    it.Value()->SetLeft( fid );
    }

  CellAutoPointer face;
  face.TakeOwnership( faceCell );
  this->Superclass::SetCell( fid, face );
}

/**
* Add a triangle face to this QuadEdgeMesh.
* @param aPid \ref PointIdentifier of first point
* @param bPid \ref PointIdentifier of second point
* @param cPid \ref PointIdentifier of third point
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >
::AddFaceTriangle( 
  const PointIdentifier& aPid,
  const PointIdentifier& bPid,
  const PointIdentifier& cPid )
{
  PointIdList points;
  points.push_back( aPid );
  points.push_back( bPid );
  points.push_back( cPid );
  return this->AddFace( points );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
QuadEdgeMesh< TPixel, VDimension, TTraits >
::QuadEdgeMesh()
{
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::CoordRepType
QuadEdgeMesh< TPixel, VDimension, TTraits >
::ComputeEdgeLength( QEPrimal* e )
{
  PointType org  = this->GetPoint( e->GetOrigin()  );
  PointType dest = this->GetPoint( e->GetDestination() );

  return ( dest.GetVectorFromOrigin() -
      org.GetVectorFromOrigin() ).GetNorm();
}

/**
* \brief Compute the total number of USED points. This differs from
* \ref itk::Mesh::GetNumberOfPoints() that will return the total number of
* points including the ones that have no entry in the edge ring.
*
* \note This method is an optional utility of the class: its
* understanding is not usefull at first contact with the class.
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
unsigned long 
QuadEdgeMesh< TPixel, VDimension, TTraits >
::ComputeNumberOfPoints() const
{
/* TODO The following code couldn't be used because the Macro
 * has not Const correct version. Preserve it and move it to
 * the documentation of an example.
 itkQEMeshForAllPointsMacro( Self, this, point, dummyIndex )
 {
   if( point.GetEdge() )
    numberOfPoints++;
 }
 itkQEMeshForAllPointsEndMacro;
 */
  typedef typename PointsContainer::ConstIterator PointsContainerIterator;
  const PointsContainer* points = this->GetPoints();

  if( ! points )
    {
    itkWarningMacro("No point container");
    return 0;
    }

  unsigned long numberOfPoints = 0;
  PointsContainerIterator pointIterator = points->Begin();

  while( pointIterator != points->End() )
    {
    if( pointIterator.Value().GetEdge() )
      {
      numberOfPoints++;
      }
    pointIterator++;
    }

  return numberOfPoints;
}

/**
* \brief Compute the total number of faces.
*
* \note This method is an optional utility of the class: its
* understanding is not usefull at first contact with the class.
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
unsigned long 
QuadEdgeMesh< TPixel, VDimension, TTraits >
::ComputeNumberOfFaces() const
{
  unsigned long numberOfFaces = 0;
  CellsContainerConstIterator cellIterator = this->GetCells()->Begin();
  CellsContainerConstIterator cellEnd      = this->GetCells()->End();

  while( cellIterator != cellEnd )
    {
    if( cellIterator.Value()->GetNumberOfPoints() > 2 )
      {
      numberOfFaces++;
      }
    ++cellIterator;
  }

  return numberOfFaces;
}

/**
* \brief Compute the total number of edges.
*
* \note This method is an optional utility of the class: it's
*       understanding is not usefull at first contact with the class.
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
unsigned long 
QuadEdgeMesh< TPixel, VDimension, TTraits >
::ComputeNumberOfEdges() const
{
  unsigned long numberOfEdges = 0;

  CellsContainerConstIterator cellIterator = this->GetCells()->Begin();
  CellsContainerConstIterator cellEnd      = this->GetCells()->End();

  while( cellIterator != cellEnd )
  {
  if( QEPrimal* edge = dynamic_cast< QEPrimal* >( cellIterator.Value()) )
    {
    (void)edge;
    numberOfEdges++;
    }

    ++cellIterator;
  }

return numberOfEdges;
}

} // namespace itk

#endif 
