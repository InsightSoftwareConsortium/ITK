// ------------------------------------------------------------------------
// itkQuadEdgeMesh.txx
// $Revision: 1.1 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
// ------------------------------------------------------------------------
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
// ------------------------------------------------------------------------

#ifndef __itkQuadEdgeMesh_txx
#define __itkQuadEdgeMesh_txx

#include <algorithm>
#include <vector>

namespace itk
{

template< typename TPixel, unsigned int VDimension, typename TTraits >
const typename QuadEdgeMesh< TPixel, VDimension, TTraits >::PointIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >::NOPOINT
             =  QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal::NOPOINT;

template< typename TPixel, unsigned int VDimension, typename TTraits >
const typename QuadEdgeMesh< TPixel, VDimension, TTraits >::CellIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >::NOFACE
                = QuadEdgeMesh< TPixel, VDimension, TTraits >::QEDual::NOPOINT;

/**
 * Clear all this mesh by deleting all contained edges which as
 * a side effect deletes adjacent faces
 */
template< typename TPixel, unsigned int VDimension, typename TTraits >
    void QuadEdgeMesh< TPixel, VDimension, TTraits >::
    Clear()
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
    QuadEdgeMesh< TPixel, VDimension, TTraits >::
    Splice( QEPrimal* a, QEPrimal* b )
{
    bool SplitingOrg = a->IsInOnextRing( b );
    PointIdentifier resultingOrgId;

    if( SplitingOrg )
    {
    // see TODO's entry dated 2006-01-24
        /* We consider the following situation which depicts the Onext()
         * ring around the point Org (which is both a->GetOrg() and
         * b->GetOrg():
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
        // Make sure the Org's edge entry doesn't point to an entry edge
        // that isn't any more in the Onext ring:
        PointIdentifier orgId = a->GetOrg();
        PointType org = this->GetPoint( orgId );
        org.SetEdge( a );
        this->SetPoint( orgId, org );
  
        // Create a newOrg point by duplicating the geometry of Org...
        PointType newOrg  = org;
        newOrg.SetEdge( b );
        PointIdentifier newOrgId = this->AddPoint( newOrg );
  
        // ...and inform Onext ring of b that their Org() have changed:
        typename QEPrimal::IteratorGeom it;
        for( it = b->BeginGeomOnext(); it != b->EndGeomOnext(); it++ )
        {
           it.Value()->SetOrg( newOrgId );
        }
        resultingOrgId = newOrgId;
    }
    else
    {
    // see TODO's entry dated 2006-01-24
        /* We consider the following situation which depicts the Onext()
         * rings around the point Org = a->GetOrg() and
         * oldOrg = b->GetOrg():
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
        // First, consider the vertices: Org and oldOrg must be different.
        PointIdentifier oldOrgId = b->GetOrg();
        PointIdentifier orgId = a->GetOrg();
        if( oldOrgId == orgId )
        {
           itkWarningMacro( "Trying to fuse the same point!" );
           return( NOPOINT );
        }

        /** \todo Compare the geometry of the two points and accept
         * splicing when their geometry matches. We could fix
         * an epsilon threshold distance above which the two points
         * are considered distinct.
         */
        PointType oldOrg = this->GetPoint( oldOrgId );
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
        if(    ( aLeftFace == NOFACE && bLeftFace != NOFACE )
            || ( aLeftFace != NOFACE && bLeftFace == NOFACE ) )
        {
           itkWarningMacro( "Face on one side but not the other. Canceling." );
           return( NOPOINT );
        }
        if( aLeftFace != NOFACE && bLeftFace != NOFACE )
        {
           if(   ( aLeftFace == bLeftFace )
              && ( a->GetLnext()              != b ) 
              && ( a->GetLnext()->GetLnext() != b ) 
              && ( b->GetLnext()              != a ) 
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
              return( NOPOINT );
           }
        }
        // Notice that when aLeftFace == NOFACE and bLeftFace == NOFACE 
        // we simply proceed... (with MustReconstructFace initialy set to
        // false.

        ///////////////////////////////////////////////////////////////
        // Handle connectivity at QEQuadEdge level:
        a->Splice( b );

        ///////////////////////////////////////////////////////////////
        // Back to dealing with the geometrical references. First
        // make sure the oldOrg's edge entry won't be used any more:
        oldOrg.SetEdge( (QEPrimal*)0 );
        this->SetPoint( oldOrgId, oldOrg );
  
        // We need to inform the edges ranging from a->Onext() to b that
        // their Org() have changed. Let's over do it (read, be lazy) and
        // inform the full Onext() ring:
        typename QEPrimal::IteratorGeom it;
        for( it = a->BeginGeomOnext(); it != a->EndGeomOnext(); it++ )
        {
           it.Value()->SetOrg( orgId );
        }
        resultingOrgId = oldOrgId;

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
    return resultingOrgId;
}

/**
 */
template< typename TPixel, unsigned int VDimension, typename TTraits >
    bool QuadEdgeMesh< TPixel, VDimension, TTraits >::
    FindClosestPoint( CoordRepType coords[ PointDimension ],
                      PointIdentifier* pointId ) const
{
    VectorType vP;
    vP.Get_vnl_vector().copy_in( coords );
    PointsContainerConstIterator pit = this->GetPoints()->Begin();
    *pointId = pit.Index();
    for( ; pit != this->GetPoints()->End(); pit++ ) {

        VectorType v0 = pit.Value().GetVectorFromOrigin();
        VectorType v1 = this->GetVector( *pointId );
        if( ( v0 - vP ).GetNorm() < ( v1 - vP ).GetNorm() )
            *pointId = pit.Index();

    } // rof
    return( true );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
    void QuadEdgeMesh< TPixel, VDimension, TTraits >::
    SetCell( CellIdentifier cId, CellAutoPointer& cell )
{
  (void)cId;

  EdgeCellType* qe = (EdgeCellType*) NULL;
  PolygonCellType* pe = (PolygonCellType*) NULL;

  if( ( qe = dynamic_cast< EdgeCellType* >( cell.GetPointer() ) ) )
    {
    this->AddEdge( qe->GetOrg(), qe->GetDest() );
    }
  else if( ( pe = dynamic_cast< PolygonCellType* >( cell.GetPointer() ) ) )
    {
    PointIdList points;

    typename PolygonCellType::PointIdIterator pit = pe->PointIdsBegin();

    for( ; pit != pe->PointIdsEnd(); pit++ )
      {
      points.push_back( *pit );
      }

    this->AddFace( points );
    }
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::PointIdentifier
QuadEdgeMesh< TPixel, VDimension, TTraits >::FindFirstUnusedPointIndex()
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
    QuadEdgeMesh< TPixel, VDimension, TTraits >::
    AddPoint( const PointType& p )
{
    PointIdentifier pid = this->FindFirstUnusedPointIndex();
    this->SetPoint( pid, p );
    return( pid );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
    void QuadEdgeMesh< TPixel, VDimension, TTraits >::
    DeletePoint( const PointIdentifier& pid )
{
  if( this->GetPoint( pid ).GetEdge() )
    {
    itkDebugMacro( "Point is not isolated." );
    return;
    }

  this->GetPoints()->DeleteIndex( pid );
  m_FreePointIndexes.push( pid );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
    typename QuadEdgeMesh< TPixel, VDimension, TTraits >::PointType
    QuadEdgeMesh< TPixel, VDimension, TTraits >::
    GetPoint( const PointIdentifier& pid ) const
{
    return( this->GetPoints()->GetElement( pid ) );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
    typename QuadEdgeMesh< TPixel, VDimension, TTraits >::VectorType
    QuadEdgeMesh< TPixel, VDimension, TTraits >::
    GetVector( const PointIdentifier& pid ) const
{
    return( this->GetPoint( pid ).GetVectorFromOrigin() );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
    typename QuadEdgeMesh< TPixel, VDimension, TTraits >::CellIdentifier
    QuadEdgeMesh< TPixel, VDimension, TTraits >::
    FindFirstUnusedCellIndex()
{
  CellIdentifier cid;
  if( m_FreeCellIndexes.size() == 0 ) {

      cid = this->GetNumberOfCells();
      if( cid != 0 ) {

          CellsContainerIterator last = this->GetCells()->End();
          last--;
          cid = last.Index() + 1;

      } // fi

  } else {

      cid = m_FreeCellIndexes.front();
      m_FreeCellIndexes.pop();

  } // fi
  return( cid );
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
    QuadEdgeMesh< TPixel, VDimension, TTraits >::
    AddEdge( const PointIdentifier& orgPid,
             const PointIdentifier& destPid )
{
  // Make sure the points are different
  if( orgPid == destPid )
  {
      itkDebugMacro( "Creating an edge between the same point." );
      return( (QEPrimal*)0 );
  } // fi

  // Make sure the points are allready in the QuadEdgeMesh container:
  if( !( this->GetPoints()->IndexExists( orgPid ) ) ||
      !( this->GetPoints()->IndexExists( destPid ) ) )
  {
      itkDebugMacro( "One of the points not in the PointSet." );
      return( (QEPrimal*)0 );
  } // fi

  // Make sure the edge is not allready in the container
  QEPrimal* e = this->FindEdge( orgPid, destPid );
  if( e != (QEPrimal*)0 )
  {
      itkDebugMacro( "Edge allready in QuadEdgeMesh." );
      return( e );
  } // fi

  // Check if the points have room to receive a new edge
  QEPrimal*  eOrg = this->FindEdge(  orgPid );
  QEPrimal* eDest = this->FindEdge( destPid );
  if( eOrg )
      if( eOrg->IsOrgInternal() )
      {
          itkDebugMacro( "No room for a new edge in the Org() ring." );
          return( (QEPrimal*)0 );
      }
  if( eDest )
      if( eDest->IsOrgInternal() )
      {
          itkDebugMacro( "No room for a new edge in the Dest() ring." );
          return( (QEPrimal*)0 );
      }

  // Ok, there's room and the points exist
  EdgeCellType* newEdge = new EdgeCellType( true );

  newEdge->SetOrg (  orgPid );
  newEdge->SetDest( destPid );

  if( !eOrg )
  {
      PointType pOrg = this->GetPoint( orgPid );
      pOrg.SetEdge( newEdge );
      this->SetPoint( orgPid, pOrg );
  } else
      eOrg->InsertAfterNextBorderEdgeWithUnsetLeft( newEdge );

  if( !eDest )
  {
      PointType pDest = this->GetPoint( destPid );
      pDest.SetEdge( newEdge->GetSym() );
      this->SetPoint( destPid, pDest );
  } else
      eDest->InsertAfterNextBorderEdgeWithUnsetLeft( newEdge->GetSym() );

  // Add it to the container
  this->PushOnContainer( newEdge );

  return( newEdge );
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
  void QuadEdgeMesh< TPixel, VDimension, TTraits >::
  PushOnContainer( EdgeCellType* newEdge )
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
  void QuadEdgeMesh< TPixel, VDimension, TTraits >::
  DeleteEdge( const PointIdentifier& orgPid,
              const PointIdentifier& destPid )
{
  // Check if the edge exists
  QEPrimal* e = this->FindEdge( orgPid, destPid );
  if( e == (QEPrimal*)0 )
  {
      itkDebugMacro( "Edge not present in mesh." );
      return;
  } // fi
  this->DeleteEdge( e );
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
  void QuadEdgeMesh< TPixel, VDimension, TTraits >::
  DeleteEdge( QEPrimal* e )
{
  const PointIdentifier& orgPid  = e->GetOrg();
  const PointIdentifier& destPid = e->GetDest();
  // Check if the Org point's edge ring entry should be changed
  PointType pOrg = this->GetPoint( orgPid );
  if( pOrg.GetEdge() == e )
  {
      if( !e->IsOrgDisconnected() )
          pOrg.SetEdge( e->GetOprev() );
      else
          pOrg.SetEdge( (QEPrimal*)0 );
      this->SetPoint( orgPid, pOrg );
  } // fi

  // Same for the Dest point
  PointType pDest = this->GetPoint( destPid );
  if( pDest.GetEdge() == e->GetSym() )
  {
      if( !e->IsDestDisconnected() )
          pDest.SetEdge( e->GetLnext() );
      else
          pDest.SetEdge( (QEPrimal*)0 );
      this->SetPoint( destPid, pDest );
  } // fi

  // This container serves to avoid the MS .net bug when
  // one wants to delete a map element using a map::iterator.
  // Normally, if we delete a map element using an iterator,
  // it should keep the iterator validity but .net doesn't
  // like it, so we delay the cell deletion to a later loop.
  typedef std::vector< CellIdentifier > DeleteCellsCont;
  DeleteCellsCont cellsToDelete;

  // Delete all references to 'e' in the container
  CellsContainerIterator cit = this->GetCells()->Begin();
  for( ; cit != this->GetCells()->End(); cit++ )
  {
      EdgeCellType* cell = dynamic_cast< EdgeCellType* >( cit.Value() );
      PolygonCellType* pcell = dynamic_cast< PolygonCellType* >( cit.Value() );
      bool toDelete = false;
      if( cell != (EdgeCellType*)0 )
      {
          QEPrimal* edge = dynamic_cast< QEPrimal* >( cell );
          toDelete = ( edge == e || edge->GetSym() == e );
      }
      else if( pcell != (PolygonCellType*)0 )
      {
          QEPrimal* edge = pcell->GetEdgeRingEntry();
          typename QEPrimal::IteratorGeom it = edge->BeginGeomLnext();
          for( ; it != edge->EndGeomLnext() && !toDelete; it++ )
              toDelete = ( ( it.Value() == e ) ||
                           ( it.Value()->GetSym() == e ) );

          // Unset left faces
          if( toDelete )
              for( it = edge->BeginGeomLnext();
                   it != edge->EndGeomLnext();
                   it++ )
                  it.Value()->UnsetLeft();
      } // fi

      if( toDelete )
      {
          cellsToDelete.push_back( cit.Index() );
          m_FreeCellIndexes.push( cit.Index() );
      }

  } // rof

  // Delete the elements in the map
  typename DeleteCellsCont::iterator dit = cellsToDelete.begin();
  for( ; dit != cellsToDelete.end(); dit++ )
      this->GetCells()->DeleteIndex( *dit );

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
  void QuadEdgeMesh< TPixel, VDimension, TTraits >::
  LightWeightDeleteEdge( QEPrimal* e )
{
  /////////////////////////////////////////////////////////////////
  // First make sure the points are not pointing to the edge we are
  // trying to delete.
  const PointIdentifier& orgPid  = e->GetOrg();
  const PointIdentifier& destPid = e->GetDest();
  // Check if the Org point's edge ring entry is the edge we are
  // trying to delete. When this is the case shift the Org edge entry
  // to another edge and when no other edge is available leave it
  // to NULL.
  PointType pOrg = this->GetPoint( orgPid );
  if( pOrg.GetEdge() == e )
  {
      if( !e->IsOrgDisconnected() )
          pOrg.SetEdge( e->GetOprev() );
      else
          pOrg.SetEdge( (QEPrimal*)0 );
      this->SetPoint( orgPid, pOrg );
  } // fi

  // Same thing for the Dest point:
  PointType pDest = this->GetPoint( destPid );
  if( pDest.GetEdge() == e->GetSym() )
  {
      if( !e->IsDestDisconnected() )
          pDest.SetEdge( e->GetLnext() );
      else
          pDest.SetEdge( (QEPrimal*)0 );
      this->SetPoint( destPid, pDest );
  } // fi

  /////////////////////////////////////////////////////////////////
  // Second we need to destroy the adjacent faces (both GetLeft()
  // and GetRight() when they exist) because their very definition
  // makes reference to the edge we are trying to delete:
  if( e->IsLeftSet() )
     this->DeleteFace( e->GetLeft() );
  if( e->IsRightSet() )
     this->DeleteFace( e->GetRight() );

  /////////////////////////////////////////////////////////////////
  // Third we need to remove from the container the EdgeCell
  // representing the edge we are trying to destroy at the itk
  // level. For this we first get our hands back on the EdgeCell
  // by upcasting to EdgeCellType*:
  EdgeCellType* edgeCell = dynamic_cast< EdgeCellType* > ( e );
  if( !edgeCell)
  {
     edgeCell = dynamic_cast< EdgeCellType* > ( e->GetSym() );
  }
  if( !edgeCell)
  {
     itkWarningMacro( "Neither e nor e->Sym() are upcastable !" );
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
  void QuadEdgeMesh< TPixel, VDimension, TTraits >::
  DeleteFace( FaceRefType faceToDelete )
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
     e = e->GetSym();
  if( faceToDelete != e->GetLeft() )
  {
     itkWarningMacro( "Neither e nor e->Sym() are the proper face !" );
     return;
  }
  typename QEPrimal::IteratorGeom it;
  for( it = e->BeginGeomLnext(); it != e->EndGeomLnext(); it++ )
     it.Value()->UnsetLeft();

  this->GetCells()->DeleteIndex( faceToDelete );
  this->Modified();
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
  typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
  QuadEdgeMesh< TPixel, VDimension, TTraits >::
  GetEdge() const
{
  QEPrimal* e = (QEPrimal*)0;
  CellsContainerIterator cit = this->GetCells()->Begin();
  for( ; cit != this->GetCells()->End() && e == (QEPrimal*)0; cit++ )
  {
      e = dynamic_cast< QEPrimal* >( cit.Value() );

      // Check if we can get an edge from a potential polygon
      if( e == (QEPrimal*)0 )
      {
          PolygonCellType* pol =
              dynamic_cast< PolygonCellType* >( cit.Value() );
          e = ( pol )?
              dynamic_cast< QEPrimal* >( pol->GetEdgeRingEntry() ): e;
      } // fi
  } // rof
  return( e );
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
  typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
  QuadEdgeMesh< TPixel, VDimension, TTraits >::
  GetEdge( const CellIdentifier& eid ) const
{
  QEPrimal* cell = ( dynamic_cast< QEPrimal* >
                   ( this->GetCells()->GetElement( eid ) ) );
  return( cell );
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
  typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
  QuadEdgeMesh< TPixel, VDimension, TTraits >::
  FindEdge( const PointIdentifier& pid0 ) const
{
  PointType p = this->GetPoint( pid0 );
  return( dynamic_cast< QEPrimal* >( p.GetEdge() ) );
}

/**
*/
template< typename TPixel, unsigned int VDimension, typename TTraits >
  typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
  QuadEdgeMesh< TPixel, VDimension, TTraits >::
  FindEdge( const PointIdentifier& pid0,
            const PointIdentifier& pid1 ) const
{
  QEPrimal* e = this->FindEdge( pid0 );
  if( e )
  {
      QEPrimal* d = (QEPrimal*)0;
      typename QEPrimal::IteratorGeom it = e->BeginGeomOnext();
      for( ; it != e->EndGeomOnext(); it++ )
          d = ( it.Value()->GetDest() == pid1 )?
              dynamic_cast< QEPrimal* >( it.Value() ): d;
      e = d;
  } // fi
  return( e );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >::AddFace( PointIdList& points )
{
  // Check that there are no duplicate points
  for(unsigned int i=0; i < points.size(); i++)
    {
    if( std::count( points.begin(), points.end(), points[i] ) != 1 )
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
    void QuadEdgeMesh< TPixel, VDimension, TTraits >::
    AddFace( QEPrimal* entry )
{
  // Create the cell and add it to the container
  PolygonCellType* faceCell = new PolygonCellType( entry );
  CellIdentifier fid = this->FindFirstUnusedCellIndex();
  faceCell->SetIdent( fid );

  // Associate the above generated CellIndex as the default FaceRefType
  // of the new face [ i.e. use the itk level CellIdentifier as the
  // GeometricalQuadEdge::m_Org of dual edges (edges of type QEDual) ].
  typename QEPrimal::IteratorGeom it;
  for( it = entry->BeginGeomLnext(); it != entry->EndGeomLnext(); it++ )
    {
    it.Value()->SetLeft( fid );
    }

  CellAutoPointer face;
  face.TakeOwnership( faceCell );
  this->Superclass::SetCell( fid, face );

}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
    typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
    QuadEdgeMesh< TPixel, VDimension, TTraits >::
    AddFace( unsigned int nPoints,
             const PointIdentifier& p1,
             const PointIdentifier& p2,
             const PointIdentifier& p3, ... )
{

  // Capture point list
  PointIdList points;

  va_list argList;
  va_start( argList, p3 );

  points.push_back( p1 );
  points.push_back( p2 );
  points.push_back( p3 );

  for(unsigned int i=3; i < nPoints; i++)
    {
    PointIdentifier pid = va_arg( argList, PointIdentifier );
    points.push_back( pid );
    }

  va_end( argList );

  return this->AddFace( points );
}

/**
 * Add a triangle face to this QuadEdgeMesh.
 * @param aPid \ref PointIdentifier of first point
 * @param bPid \ref PointIdentifier of second point
 * @param cPid \ref PointIdentifier of third point
 */
template< typename TPixel, unsigned int VDimension, typename TTraits >
typename QuadEdgeMesh< TPixel, VDimension, TTraits >::QEPrimal*
QuadEdgeMesh< TPixel, VDimension, TTraits >::
AddFaceTriangle( const PointIdentifier& aPid,
    const PointIdentifier& bPid,
    const PointIdentifier& cPid )
{
  return this->AddFace( 3, aPid, bPid, cPid );
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
  QuadEdgeMesh< TPixel, VDimension, TTraits >::QuadEdgeMesh() : Superclass()
{
}

//////////////////////////////////////////////////////////////////////////
template< typename TPixel, unsigned int VDimension, typename TTraits >
    typename QuadEdgeMesh< TPixel, VDimension, TTraits >::CoordRepType
    QuadEdgeMesh< TPixel, VDimension, TTraits >::
    ComputeEdgeLength( QEPrimal* e )
{
 PointType org  = this->GetPoint( e->GetOrg()  );
 PointType dest = this->GetPoint( e->GetDest() );

 return (dest.GetVectorFromOrigin() - org.GetVectorFromOrigin()).GetNorm();
}

/**
 * \brief Compute the total number of USED points. This differs from
 *        \ref itk::Mesh::GetNumberOfPoints() that will return the total
 *        number of points including the ones that have no entry in the
 *        edge ring.
 *
 * \note This method is an optional utility of the class: it's
 *       understanding is not usefull at first contact with the class.
 */
template< typename TPixel, unsigned int VDimension, typename TTraits >
    unsigned long QuadEdgeMesh< TPixel, VDimension, TTraits >::
    ComputeNumberOfPoints() const
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
    itkWarningMacro( "No point container" );
    return 0;
    }

  unsigned long numberOfPoints = 0;
  PointsContainerIterator pointIterator = points->Begin();

  for( ; pointIterator != points->End(); pointIterator++ )
    {
    if( pointIterator.Value().GetEdge() )
      {
      numberOfPoints++;
      }
    }

  return numberOfPoints;
}

/**
 * \brief Compute the total number of faces.
 *
 * \note This method is an optional utility of the class: it's
 *       understanding is not usefull at first contact with the class.
 */
template< typename TPixel, unsigned int VDimension, typename TTraits >
    unsigned long QuadEdgeMesh< TPixel, VDimension, TTraits >::
    ComputeNumberOfFaces() const
{
    unsigned long numberOfFaces = 0;
    CellsContainerConstIterator cellIterator = this->GetCells()->Begin();
    CellsContainerConstIterator cellEnd      = this->GetCells()->End();
    while( cellIterator != cellEnd )
    {
        if( cellIterator.Value()->GetNumberOfPoints() > 2 )
            numberOfFaces++;
        ++cellIterator;
    }
    return( numberOfFaces );
}

/**
 * \brief Compute the total number of edges.
 *
 * \note This method is an optional utility of the class: it's
 *       understanding is not usefull at first contact with the class.
 */
template< typename TPixel, unsigned int VDimension, typename TTraits >
    unsigned long QuadEdgeMesh< TPixel, VDimension, TTraits >::
    ComputeNumberOfEdges() const
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


