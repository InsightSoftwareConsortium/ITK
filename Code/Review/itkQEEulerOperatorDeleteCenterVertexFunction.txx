// -------------------------------------------------------------------------
// itkQEEulerOperatorDeleteCenterVertexFunction.txx
// $Revision: 1.1 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-09 00:58:17 $
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
#ifndef __ITKQUADEDGEMESH__ITKQEEULEROPERATORDELETECENTERVERTEXFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQEEULEROPERATORDELETECENTERVERTEXFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.

namespace itkQE
{

template < class TMesh, class TQEType >
  typename EulerOperatorDeleteCenterVertexFunction< TMesh, TQEType >::OutputType
  EulerOperatorDeleteCenterVertexFunction< TMesh, TQEType >::
  Evaluate( QEType* g )
{

   if( !g )
   {
      itkDebugMacro( "Input is not an edge." );
      return( (QEType*) 0 );
   }
   
   
   if( !this->m_Mesh )
   {
      itkDebugMacro( "No mesh present." );
      return( (QEType*) 0 );
   }

   if(  !g->IsInternal( ) )
   {
      itkDebugMacro( "The edge is either border or wire." );
      return( (QEType*) 0 );
   }


   // None of the incident facets of e->GetDest() is a hole.
   std::set< PointIdentifier > visitedSet;
   
   //one-ring
   QEType* g_sym = g->GetSym( );
   double one_ring_points = 0.;
   double two_ring_points = 0.;
   typedef typename QEType::IteratorGeom QEIterator;
   for(  QEIterator it = g_sym->BeginGeomOnext( );
                    it != g_sym->EndGeomOnext( );
                    it++ )
   {
      one_ring_points++;
      QEType* one_edge = it.Value( );  
      if ( !one_edge->IsInternal( ) )
      {
         itkDebugMacro( "DeleteVertex requires a full one-ring, i.e. no holes." );
         return( (QEType*) 0 );
      }
      visitedSet.insert( one_edge->GetDest( ) );
     
      // There are at least two distinct facets incident to the facets that 
      // are incident to e->GetDest(). i.e. the 2-ring minus the one ring 
      // is at least two faces. (This prevents the operation from collapsing 
      // a volume into two facets glued together with opposite orientations, 
      // such as would happen with any vertex of a tetrahedron.) 
 
      //two-ring
      QEType* edge_sym = one_edge->GetSym( );
      for( QEIterator iit  = edge_sym->BeginGeomOnext( );
                      iit != edge_sym->EndGeomOnext( );
                      iit++ )
      {
         QEType* two_edge = it.Value( );     
         if( !visitedSet.count( two_edge->GetDest( ) ) )
         {      
            two_ring_points++;
            visitedSet.insert( two_edge->GetDest( ) );
         }

      } // ENDOFFOR two-ring
   
   } // ENDOFFOR one-ring    
   
   if (0)// ( two_ring_points > 0)
   {
      itkDebugMacro( "DeleteVertex requires at least two distinct \
      facets incident to the facets that are incident to e->GetDest()." );
      return( (QEType*) 0 );
   }
   
   // let's do the job now.
   QEType* h = g->GetRprev( );
   PointIdentifier PointToRemoveID = g->GetDest( );
   this->m_Mesh->LightWeightDeleteEdge( g );
   g = h->GetRnext( );
   while( g != h )
   {
      while( ( g->GetDest( ) != PointToRemoveID ) && ( g != h ) )
      {
         g = g->GetRnext( );
      }
      if ( g != h )
      {
         this->m_Mesh->LightWeightDeleteEdge( g );
         g = h->GetRnext( );     
      }
   }
   
   // someone PLEASE explain me why this line is not working.
   // this->m_Mesh->AddFace( h->GetSym( ) );
   
   return( h );
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQEEULEROPERATORDeleteCenterVertexFUNCTION__TXX__

// eof - itkQEEulerOperatorDeleteCenterVertexFunction.txx

