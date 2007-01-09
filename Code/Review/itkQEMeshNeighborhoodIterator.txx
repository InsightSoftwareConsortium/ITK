// -------------------------------------------------------------------------
// itkQEMeshNeighborhoodIterator.txx
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

#ifndef __ITKQUADEDGEMESH__MESHNEIGHBORHOODITERATOR__TXX__
#define __ITKQUADEDGEMESH__MESHNEIGHBORHOODITERATOR__TXX__

namespace itkQE
{
    // ---------------------------------------------------------------------
    template< class TMesh, typename TQE >
        void MeshNeighborhoodIterator< TMesh, TQE >::
        GoToBegin( )
    {
        // Clear iteration attributes
        m_Visited.clear( );
        while( m_Front.size( ) > 0 ) m_Front.pop( );

        // Initialize iteration
        m_Front.push( m_StartPid );
        m_Visited[ m_StartPid ] = 0;
    }

    // ---------------------------------------------------------------------
    template< class TMesh, typename TQE >
        void MeshNeighborhoodIterator< TMesh, TQE >::
        GoToNext( )
    {
        if( m_Front.size( ) > 0 ) {

            PointIdentifier lastPid = m_Front.front( );
            unsigned int dist = m_Visited[ lastPid ];
            if( dist < m_Order ) {

                QEType* e = m_Mesh->FindEdge( lastPid );
                QEIterator it = e->BeginGeomOnext( );
                for( ; it != e->EndGeomOnext( ); it++ ) {

                    PointIdentifier pid = it.Value( )->GetDest( );
                    if( m_Visited.find( pid ) == m_Visited.end( ) ) {

                        m_Visited[ pid ] = dist + 1;
                        m_Front.push( pid );

                    } // fi

                } // rof

            } // fi

        } // fi
        m_Front.pop( );
    }

} // enamespace

#endif // __ITKQUADEDGEMESH__MESHNEIGHBORHOODITERATOR__TXX__

// eof - itkQEMeshNeighborhoodIterator.txx
