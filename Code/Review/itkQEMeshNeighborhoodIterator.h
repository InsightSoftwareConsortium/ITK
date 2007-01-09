// -------------------------------------------------------------------------
// itkQEMeshNeighborhoodIterator.h
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

#ifndef __ITKQUADEDGEMESH__MESHNEIGHBORHOODITERATOR__H__
#define __ITKQUADEDGEMESH__MESHNEIGHBORHOODITERATOR__H__

#include <map>
#include <queue>

namespace itkQE
{
    /**
     * Construct an iterator (well somehow) on the points contained in the
     * neighborhood of a constructor specified origin point.
     * The size of the neighborhood (which is also specified in the
     * constructor through the order argument) is given as number of
     * edges from the origin point (i.e. the "distance" is the one obtained
     * with associated a length of 1 to all edges).
     * \warning strict iterator syntax is not implemented e.g. one must
     *          use GoToNext( ) instead of ++.
     */
    template< class TMesh, typename TQE >
        class MeshNeighborhoodIterator
        {
            public:
            typedef MeshNeighborhoodIterator Self;

            typedef TMesh                              MeshType;
            typedef TQE                                QEType;
            typedef typename MeshType::CoordRepType    CoordRepType;
            typedef typename MeshType::PointType       PointType;
            typedef typename MeshType::VectorType      VectorType;
            typedef typename MeshType::PointIdentifier PointIdentifier;
            typedef typename QEType::IteratorGeom      QEIterator;

            /** Iteration types. */
            typedef std::map< PointIdentifier, unsigned int > VisitedType;
            typedef std::queue< PointIdentifier >             FrontType;

            public:
            MeshNeighborhoodIterator( MeshType* mesh,
                                      PointIdentifier pid,
                                      unsigned int order )
                : m_Mesh( mesh ), m_StartPid( pid ), m_Order( order )
            { }
            virtual ~MeshNeighborhoodIterator( ) { }
            PointIdentifier GetIndex( )
            { return( m_Front.front( ) ); }
            PointType GetPoint( )
            { return( m_Mesh->GetPoint( this->GetIndex( ) ) ); }
            VectorType GetVector( )
            { return( m_Mesh->GetVector( this->GetIndex( ) ) ); }
            void GoToBegin( );
            bool IsAtEnd( )
            { return( m_Front.size( ) == 0 ); }
            void GoToNext( );

            protected:
            MeshType*       m_Mesh;
            PointIdentifier m_StartPid;
            unsigned int    m_Order;
            VisitedType     m_Visited;
            FrontType       m_Front;
        };

} // enamespace

#include "itkQEMeshNeighborhoodIterator.txx"

#endif // __ITKQUADEDGEMESH__MESHNEIGHBORHOODITERATOR__H__

// eof - itkQEMeshNeighborhoodIterator.h
