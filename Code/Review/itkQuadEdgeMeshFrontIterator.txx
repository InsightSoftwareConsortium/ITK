// -------------------------------------------------------------------------
// itkQuadEdgeMeshFrontIterator.txx
// $Revision $
// $Author $
// $Name $
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

#ifndef __itkQuadEdgeMeshFrontIterator_txx
#define __itkQuadEdgeMeshFrontIterator_txx

#include "itkQuadEdgeMeshFrontIterator.h"

namespace itk
{

// ---------------------------------------------------------------------
template< class TMesh, class TQE >
    QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::
    QuadEdgeMeshFrontBaseIterator( MeshType* mesh,
                       bool      start,
                       QEType*   seed )
        : m_Mesh( mesh ), m_Seed( seed ), m_Start( start )
    {
        if( !mesh )
        {
            // Invalidate the iterator and call it quits
            m_Start = false;
            return;
        }
        if( !seed )
        {
            seed = FindDefaultSeed( );
            if( !seed )
            {
                // Invalidate the iterator and call it quits
                m_Start = false;
                return;
            }
        }
        m_Front = new FrontType;
        m_Front->push_back( FrontAtom( seed, 0 ) );
        m_IsPointVisited = IsVisitedContainerType::New( );
        m_IsPointVisited->SetElement( seed->GetOrg( ), true );
        m_IsPointVisited->SetElement( seed->GetDest( ), true );
        m_CurrentEdge = seed;
    }

// ---------------------------------------------------------------------
template< class TMesh, class TQE >
    QuadEdgeMeshFrontBaseIterator< TMesh, TQE >&
    QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::
    operator++( )
    {
       // We continue only if not previously marked as finish...
       if( !m_Start )
       {
          return( *this );
       }
                                                                                
       // ... or until the front is empty:
       if(  m_Front->empty( ) )
       {
          m_Start = false;
          return( *this );
       }
                                                                                
       // Sort on the Cost:
       m_Front->sort( );
       // Consider the edge with lowest Cost:
       FrontTypeIterator fit = m_Front->begin( );
       QEType* edge = fit->Edge;
                                                                                
        // Traverse the Onext ring in search of an unvisited Org:
        typedef typename QEType::IteratorGeom      QEIterator;
        for( QEIterator qit  = edge->BeginGeomOnext( );
                        qit != edge->EndGeomOnext( );
                        qit++ )
        {
           QEType* oEdge = qit.Value( );
           // Things are quite straightforward except when QEType 
           // is in fact a QEDual (in disguise) AND oEdge->GetDest( )
           // is NOFACE [in which case oEdge->GetDest() has a value
           // but oEdge->IsDestSet() is false]. When this is the case
           // we consider oEdge->GetDest() as allready visited.
           if(    ( m_IsPointVisited->IndexExists( oEdge->GetDest( ) ) )
               || ( ! oEdge->IsDestSet( ) ) )
           {
              continue;
           }
           else
           {
              // Mark the destination as visited:
              m_IsPointVisited->SetElement( oEdge->GetDest( ), true );

              // Compute the Cost of the new OrgType:
              CoordRepType oCost = this->GetCost( oEdge ) + fit->Cost;

              // Push the Sym() on the front:
              m_Front->push_back( FrontAtom( oEdge->GetSym( ), oCost ) );
           
              // We still want to handle oEdge
              m_CurrentEdge = oEdge;
              return( *this );
           }
        }
                                                                                
        // All the edge->Org() neighbours were allready visited. Remove
        // the edge from the front, and move to next edge...
        m_Front->pop_front( );
        m_CurrentEdge = (QEType*)0;
        return( this->operator++() );
    }

/**
 * Find in the cell container an arbitrary underlying edge with type
 * QEType.
 */
template< class TMesh, class TQE >
    typename QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::QEType*
    QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::
    FindDefaultSeed( )
    {
        QEType* edgeFound = (QEType*)0;
        (void)edgeFound;
        typename MeshType::CellsContainerIterator cellIterator;
        cellIterator = m_Mesh->GetCells()->Begin();
        if( QEType* edge = dynamic_cast< QEType* >( cellIterator.Value()) )
        {
           return edge;
        }
        typedef typename QEType::Dual QEDual;
        if( QEDual* edge = dynamic_cast< QEDual* >( cellIterator.Value()) )
        {
           return edge->GetRot( );
        }
        return (QEType*)0;
    }



} 

#endif 


