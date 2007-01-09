// -------------------------------------------------------------------------
// itkQEMeshFrontPaths.txx
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
// - The duck master (Alex Gouaillard) gouaillard@creatis.insa-lyon.fr
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------

#ifndef __ITKQUADEDGEMESH__MESHFRONTPATHS__TXX__
#define __ITKQUADEDGEMESH__MESHFRONTPATHS__TXX__

namespace itkQE
{
    // ---------------------------------------------------------------------
    template< class TMesh, class TQE >
        void MeshFrontPaths< TMesh, TQE >::
        SetSeed( FrontQEType* seed )
    {
        m_Front.clear( );
        m_Front.push_back( FrontAtom( seed, 0 ) );
        m_IsPointVisited->SetElement( seed->GetOrg( ), true );
    }

    // ---------------------------------------------------------------------
    template< class TMesh, class TQE >
        void MeshFrontPaths< TMesh, TQE >::
        GenerateData( )
    {      
        while( m_Front.size( ) > 0 ) 
        {
            m_Front.sort( );

            // Pop actual edge
            FrontIterator fit = m_Front.begin( );
            FrontQEType* edge = fit->Edge;
            CoordRepType cost = fit->Cost;
            m_Front.pop_front( );

            // Treat actual edge
            this->PreTreatEdge( edge );

            // Traverse the Onext ring
            for( QEIterator qit = edge->BeginGeomOnext( );
                    qit != edge->EndGeomOnext( ); qit++ ) 
            {
                FrontQEType* oEdge = qit.Value( );

                // Check if the edge has already been followed in
                // any direction
                if( !m_IsPointVisited->IndexExists( oEdge->GetDest( ) ) )
                {           
                    m_IsPointVisited->SetElement( oEdge->GetDest( ), true );
                              
                    // treat candidate
                    CoordRepType oCost = this->GetCost( oEdge ) + cost;
                    m_Front.push_back( FrontAtom( oEdge->GetSym( ), oCost ) );
                    TreatCandidate( oEdge->GetSym( ) );

                } // fi
            
            } // rof

            this->PostTreatEdge( edge );

            // Let the user define a stop procedure
            this->UpdateStop( );

        } // elihw
    }

 // ---------------------------------------------------------------------
 template< class TMesh, class TQE >
   void MeshFrontPaths< TMesh, TQE >::
   PrintFront( )
  {
      FrontIterator it;
      for( it = m_Front.begin( ); it != m_Front.end( ); it++)
      {
         FrontQEType* edge = it->Edge;
         cout << edge->GetOrg( )  << "-";
         cout << edge->GetDest( ) << " | ";
         cout << it->Cost << endl;
      }
      cout << "//-------------------------------" << endl;  
  }

} // ecapseman
        
#endif // __ITKQUADEDGEMESH__MESHFRONTPATHS__TXX__

// eof - itkQEMeshFrontPaths.txx
