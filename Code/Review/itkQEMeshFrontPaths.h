// -------------------------------------------------------------------------
// itkQEMeshFrontPaths.h
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

#ifndef __ITKQUADEDGEMESH__MESHFRONTPATHS__H__
#define __ITKQUADEDGEMESH__MESHFRONTPATHS__H__

#include <itkMapContainer.h>
#include <itkProcessObject.h>

#include <list>

namespace itkQE
{
    /**
     */
    template< class TMesh,
              class TQE = typename TMesh::QEType >
        class MeshFrontPaths
        {
            public:
            // Standard types
            typedef MeshFrontPaths                  Self;

            // Template types
            typedef TMesh  MeshType;
            typedef TQE    FrontQEType;

            // Mesh types
            typedef typename MeshType::Pointer         MeshPointer;
            typedef typename MeshType::ConstPointer    MeshConstPointer;
            typedef typename MeshType::PointIdentifier PointIdentifier;
            typedef typename MeshType::PointType       PointType;
            typedef typename MeshType::VectorType      VectorType;
            typedef typename MeshType::CoordRepType    CoordRepType;

            // QE types
            /** \todo Does this renaming bring any additional semantics ? */
            typedef typename FrontQEType::OrgRefType        QEOrgType;
            typedef typename FrontQEType::IteratorGeom      QEIterator;
            typedef typename FrontQEType::ConstIteratorGeom QEConstIterator;


            // Front type
            class FrontAtom
            {
                public:
                FrontAtom( FrontQEType* e = (FrontQEType*)0,
                           const CoordRepType c = 0 )
                    : Edge( e ), Cost( c )
                { }
                virtual ~FrontAtom( ) { }
                FrontAtom& operator=( const FrontAtom& r )
                { Edge = r.Edge; Cost = r.Cost; }
                bool operator==( const FrontAtom& r ) const
                { return( Edge == r.Edge ); }
                bool operator!=( const FrontAtom& r ) const
                { return( Edge != r.Edge ); }
                bool operator<( const FrontAtom& r ) const
                { return( Cost < r.Cost ); }

                public:
                FrontQEType* Edge;
                CoordRepType Cost;
            };
            typedef std::list< FrontAtom >             FrontType;
            typedef typename FrontType::iterator       FrontIterator;
            typedef typename FrontType::const_iterator FrontConstIterator;
            typedef itk::MapContainer< QEOrgType, bool >
               IsVisitedContainerType;
            typedef typename IsVisitedContainerType::Pointer
               IsVisitedPointerType;
               

            public:
            MeshFrontPaths( )          
            { 
               m_Input = ( MeshType* )0;
               m_IsPointVisited = IsVisitedContainerType::New();
            };
            virtual ~MeshFrontPaths( ) { };

            /** Input IO. */
            void SetInput( const MeshType* input )
            { this->m_Input = const_cast< MeshType* >( input ); };
            const MeshType* GetInput( ) const { return( m_Input ); };

            /** Front methods. */
            virtual void SetSeed( FrontQEType* seed );
            virtual void AddToFront( FrontQEType* fe ) 
               { m_Front.push_back( FrontAtom( fe, 0 ) ); };

            protected:
            virtual void TreatCandidate( FrontQEType* edge ) { (void)edge; };
            virtual void PostTreatEdge( FrontQEType* edge )  { (void)edge; };
            virtual void PreTreatEdge( FrontQEType* edge )   { (void)edge; };
            virtual CoordRepType GetCost( FrontQEType* edge ){ (void)edge; return( 1 ); }
            virtual void UpdateStop( )                  { };
            virtual void GenerateData( );

            private:
            MeshFrontPaths( const Self& );  // Not impl.
            Self& operator=( const Self& ); // Not impl.

            void PrintFront( );
            protected:
            IsVisitedPointerType    m_IsPointVisited;
            FrontType               m_Front;
            MeshType*               m_Input;
        };

} // ecapseman

#include "itkQEMeshFrontPaths.txx"

#endif // __ITKQUADEDGEMESH__MESHFRONTPATHS__H__

// eof - itkQEMeshFrontPaths.h
