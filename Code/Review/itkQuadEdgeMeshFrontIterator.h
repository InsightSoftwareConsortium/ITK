// -------------------------------------------------------------------------
// itkQuadEdgeMeshFrontIterator.h
// $Revision: 1.2 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-24 22:52:30 $
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

#ifndef __itkQuadEdgeMeshFrontIterator_h
#define __itkQuadEdgeMeshFrontIterator_h

// -------------------------------------------------------------------------
#define itkQEDefineFrontIteratorMethodsMacro( MeshTypeArg )                  \
    /* Dual definition placed before others because of .NET that cannot */   \
    /* cope with definition of FrontIterator (that further hides the    */   \
    /* defintion of the template).                                      */   \
    typedef typename MeshTypeArg::QEDual QEDualType;                         \
    typedef typename MeshTypeArg::QEPrimal QEPrimalType;                     \
    typedef QuadEdgeMeshFrontIterator< MeshTypeArg,                          \
                                             QEDualType > FrontDualIterator; \
    typedef QuadEdgeMeshConstFrontIterator< MeshTypeArg,                     \
                                                  QEDualType >               \
                                                     ConstFrontDualIterator; \
    typedef QuadEdgeMeshFrontIterator< MeshTypeArg,                          \
                                             QEPrimalType >   FrontIterator; \
    typedef QuadEdgeMeshConstFrontIterator< MeshTypeArg,                     \
                                                  QEPrimalType >             \
                                                         ConstFrontIterator; \
                                                                             \
    virtual FrontIterator BeginFront( QEPrimalType* seed =(QEPrimalType*)0 ) \
    { return( FrontIterator( this, true, seed ) ); }                         \
                                                                             \
    virtual ConstFrontIterator BeginFront( QEPrimalType* seed ) const        \
    { return( ConstFrontIterator( this, true, seed ) ); }                    \
                                                                             \
    virtual FrontIterator EndFront( )                                        \
    { return( FrontIterator( this, false ) ); }                              \
                                                                             \
    virtual ConstFrontIterator EndFront( ) const                             \
    { return( ConstFrontIterator( this, false ) ); }                         \
                                                                             \
    virtual FrontDualIterator BeginDualFront( QEDualType* seed =(QEDualType*) 0 )    \
    { return( FrontDualIterator( this, true, seed ) ); }                     \
                                                                             \
    virtual ConstFrontDualIterator BeginDualFront( QEDualType* seed ) const      \
    { return( ConstFrontDualIterator( this, true, seed ) ); }                \
                                                                             \
    virtual FrontDualIterator EndDualFront( )                                \
    { return( FrontDualIterator( this, false ) ); }                          \
                                                                             \
    virtual ConstFrontDualIterator EndDualFront( ) const                     \
    { return( ConstFrontDualIterator( this, false ) ); }

namespace itk
{

/**
 * Front iterator on Mesh class.
 */
template< typename TMesh, typename TQE >
class QuadEdgeMeshFrontBaseIterator
{
    public:
    /// Hierarchy typedefs & values.
    typedef QuadEdgeMeshFrontBaseIterator Self;

    // Template types
    typedef TMesh  MeshType;
    typedef TQE    QEType;

    protected:
    // Mesh types
    typedef typename MeshType::CoordRepType    CoordRepType;
    // QE types
    typedef typename QEType::OriginRefType     QEOriginType;

    /** Atomic information associated to each edge of the front. Note that
     *  when sorting this list, the sorting criteria is the Cost attribute.
     */
    class FrontAtom
    {
        public:
        FrontAtom( QEType* e = (QEType*)0, const CoordRepType c = 0 )
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
        QEType* Edge;
        CoordRepType Cost;
    };

    /** The active front is simply a list of edges that can be sorted on
     *  the sort attribute FrontAtom 
     */
    typedef std::list< FrontAtom >       FrontType;
    typedef typename FrontType::iterator FrontTypeIterator;
    typedef FrontType*                   FrontTypePointer;

    /** Wether an Origin (i.e. a vertex or a face since we either deal with
     *  primal or dual edges) was allready visited.
     */
    typedef itk::MapContainer< QEOriginType, bool > IsVisitedContainerType;
    typedef typename IsVisitedContainerType::Pointer IsVisitedPointerType;

    public:
    /** Object creation methods. */
    QuadEdgeMeshFrontBaseIterator( MeshType* mesh  = (MeshType*)0,
                       bool      start = true,
                       QEType*   seed  = (QEType*)0 );
    virtual ~QuadEdgeMeshFrontBaseIterator( ) { }

    Self& operator=( const Self& r )
    {
        m_Mesh  = r.m_Mesh;
        m_Start = r.m_Start;
        m_Seed  = r.m_Seed;
        m_Front = r.m_Front;
        m_IsPointVisited = r.m_IsPointVisited;
        m_CurrentEdge = r.m_CurrentEdge;
        return( *this );
    }

    // Iteration methods.
    bool operator==( Self & r )
    {
        return( m_Start == r.m_Start );
    }

    bool operator==( const Self & r ) const
    {
        return( m_Start == r.m_Start );
    }

    bool operator!=( Self & r )
    {
        return( !( this->operator==( r ) ) );
    }

    bool operator!=( const Self & r ) const
    {
        return( !( this->operator==( r ) ) );
    }

    Self & operator++( );

    Self & operator++( int ) { return( this->operator++( ) ); }

    protected:
    /** Find a default seed by taking any edge (with proper type) in
     *  the current mesh.
     */
    QEType* FindDefaultSeed( );

    /** The default cost associated to an edge is simply 1. This corresponds
     *  to the "topological metric" i.e. all edges have unit length.
     */
    virtual CoordRepType GetCost( QEType* edge ){ (void)edge; return( 1 ); }

    protected:
    /// Mesh on which we propagate the front
    MeshType* m_Mesh;
    /// Initial seed of the front
    QEType* m_Seed;
    /// Wether the iterator is active
    bool m_Start;
    /// The active front
    FrontTypePointer m_Front;
    /// The allready visited points
    IsVisitedPointerType m_IsPointVisited;
    /// The current edge at this stage of iteration
    QEType* m_CurrentEdge;
};

/**
 * No const iterator.
 */
template< typename TMesh, typename TQE >
class QuadEdgeMeshFrontIterator
    : public QuadEdgeMeshFrontBaseIterator< TMesh, TQE  >
{
    public:
    /** Hierarchy typedefs and values. */
    typedef QuadEdgeMeshFrontIterator                   Self;
    typedef QuadEdgeMeshFrontBaseIterator< TMesh, TQE > Superclass;
    typedef typename Superclass::MeshType   MeshType;
    typedef typename Superclass::QEType     QEType;

    public:
    /** Object creation methods. */
    QuadEdgeMeshFrontIterator( MeshType* mesh = (MeshType*)0,
                   bool      start = true,
                   QEType*   seed  = (QEType*)0 )
        : Superclass( mesh, start, seed ) { }
    virtual ~QuadEdgeMeshFrontIterator( ) { }
    QEType* Value( ) { return( this->m_CurrentEdge ); }
};

/**
 * Const iterator.
 */
template< class TMesh, class TQE = typename TMesh::QEType >
class QuadEdgeMeshConstFrontIterator
    : public QuadEdgeMeshFrontBaseIterator< TMesh, TQE >
{
    public:
    /** Hierarchy typedefs & values. */
    typedef QuadEdgeMeshConstFrontIterator                Self;
    typedef QuadEdgeMeshFrontBaseIterator< TMesh, TQE >   Superclass;
    typedef typename Superclass::QEType       QEType;
    typedef typename Superclass::MeshType     MeshType;
    typedef QuadEdgeMeshFrontIterator< MeshType, QEType > NoConstType;

    public:
    /** Object creation methods. */
    QuadEdgeMeshConstFrontIterator( const MeshType* mesh = (MeshType*)0,
                              bool     start = true,
                              QEType*  seed  = (QEType*)0 ){
      (void)mesh;
      (void)start;
      (void)seed;
    }
    /** \todo do we need here a    : Superclass( mesh, start, seed ) { } */
    virtual ~QuadEdgeMeshConstFrontIterator( ) { }
    Self& operator=( const NoConstType& r )
    {
        this->m_Mesh  = r.GetMesh( );
        return( *this );
    }
    const QEType* Value( ) const { return( this->m_CurrentEdge ); }
};

} 

#include "itkQuadEdgeMeshFrontIterator.txx"

#endif 

