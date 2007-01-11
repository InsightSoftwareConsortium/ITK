// -------------------------------------------------------------------------
// itkQEMesh.h
// $Revision: 1.2 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-11 21:31:25 $
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

#ifndef __ITKQUADEDGEMESH__MESH__H__
#define __ITKQUADEDGEMESH__MESH__H__

#include <cstdarg>
#include <queue>
#include <vector>
#include <list>

#include <itkMesh.h>

#include "itkQEMeshTraits.h"
#include "itkQELineCell.h"
#include "itkQEPolygonCell.h"
#include "itkQEFrontIterator.h"

/**
 * \brief Documentation of itkQE namespace
 * \todo More comments here !
 *
 * \note Design notes: some Mesh algorithms are based on iterating various
 *    connectivity operators e.g. curvature driven surface deformation. 
 *    Many of those connectivity altering operators (e.g. the Euler operators)
 *    are lightweight in the sense that they only modify very limited regions
 *    of a Mesh: they typically act within the range of couple edges of
 *    distance from a considered vertex, edge or face.
 *    On the one side, we cannot choose to implement those atomic operations
 *    as "classical" itk filters since each filter invocation yields a new
 *    copy of the the input mesh as it's output: this would drasticaly
 *    increase the memory consumption.
 *    In fact, those atomic operations have a too much finer grain to be
 *    implemeted as filters: the filter is more at the scale of the 
 *    application of a large number of such atomic operations.
 *    One the other hand, we cannot choose to implement those atomic operations
 *    as methods ot this Mesh class (or a derived one) a the risk of rapid
 *    code bloat.
 *    Maybe we could choose to make thematic regroupment within derived
 *    classes, but his would force and end user to multiple inheritance which
 *    can prove to be a drag in a templated context.
 *    Eventually, we chose to implement them as function object: the 
 *    loosely coupling of those operation methods with the targeted Mesh
 *    object and heavier invocation syntax are a small price to pay in
 *    exchange for optimal memory usage and end user modularity.
 *    But we couldn't inherit from \ref itk::FunctionBase since it's 
 *    Evaluate( const InputType& input ) method promises to leave it's
 *    argument (the mesh we want to modify in our case) untouched.
 *    Hence we created the \ref itkQE::MeshFunctionBase class whose main
 *    difference with \ref itk::FunctionBase is that it's Evaluate( )
 *    method allows to modify the considered mesh.
 *    When considering a new Mesh method we are left with four possible
 *    "slots" to implement it:
 *      - the Mesh method
 *      - a derived class from itk::FunctionBase when the method leaves
 *        the mesh constant.
 *      - a derived class from \ref itkQE::MeshFunctionBase when the
 *        method modifies the mesh (typically in the case of Euler operators)
 *      - as a classic Mesh filter.
 *    The choice of the slot is a mere matter of trade-of and in order
 *    to keep itkQE::Mesh tiny and humanly readable key decision factors 
 *    can be the occurence of the calls and the human level complexity of
 *    the code.
 *    With those criteria in mind we made the following choices:
 *      - really atomic, lightweight and general purpose methods like
 *        \ref Mesh::ComputeNumberOfPoints are left within the Mesh class.
 *      - heavier methods and less often called like 
 *        \ref SanityCheckMeshFunction were implemented as derived classes of
 *        \ref itk::FunctionBase.
 *      - methods with the same weight (measured e.g. in number of lines of
 *        code) but that modify the considered mesh, like 
 *        \ref BoundaryRepresentativeEdgesMeshFunction or
 *        \ref ZipMeshFunction, were implemented as derived classes of
 *        \ref itkQE::MeshFunctionBase. Still we mesh modifications are
 *        really limited and concern a couple edges.
 *     - more specialised methods, with a wider scope and that require a
 *       copy of the mesh should follow the classical itk Filter pattern,
 *       like \ref itkQE::MeshExtractComponentFilter, and inherit from
 *       \ref itk::MeshToMeshFilter.
 */
namespace itkQE
{
/**
 * QE-based itk::Mesh.
 */
template< typename TPixel, unsigned int VDimension,
          typename TTraits = itkQE::MeshTraits< TPixel, VDimension, bool, bool > >
class Mesh
: public itk::Mesh< TPixel, VDimension, TTraits >
{
    public:
    /** Input template parameters. */
    typedef TTraits Traits;
    typedef TPixel  PixelType;
   
    /** Standard typedefs. */
    typedef Mesh                                    Self;
    typedef itk::Mesh< TPixel, VDimension, Traits > Superclass;
    typedef itk::SmartPointer< Self >               Pointer;
    typedef itk::SmartPointer< const Self >         ConstPointer;

    /** Convenient constants obtained from MeshTraits. */
    itkStaticConstMacro( PointDimension, unsigned int,
                         Traits::PointDimension );
    itkStaticConstMacro( MaxTopologicalDimension, unsigned int,
                         Traits::MaxTopologicalDimension );

    /** Types defined in superclass. */
    typedef typename Superclass::CellPixelType    CellPixelType;
    typedef typename Superclass::CoordRepType     CoordRepType;
    typedef typename Superclass::PointIdentifier  PointIdentifier;
    typedef typename Superclass::PointType        PointType;
    typedef typename Superclass::CellTraits       CellTraits;

    // Point section:
    typedef typename Superclass::PointsContainer        PointsContainer;
    typedef typename Superclass::PointsContainerPointer PointsContainerPointer;
    typedef typename Superclass::PointLocatorPointer    PointLocatorPointer;
    typedef typename Superclass::PointLocatorType       PointLocatorType;

    // Point data section:
    typedef typename Superclass::PointDataContainer     PointDataContainer;
    typedef typename Superclass::PointDataContainerPointer
                                 PointDataContainerPointer;
    typedef typename Superclass::PointDataContainerIterator
                                 PointDataContainerIterator;
    typedef typename Superclass::PointsContainerConstIterator
                                 PointsContainerConstIterator;
    typedef typename Superclass::PointsContainerIterator
                                 PointsContainerIterator;

    // Cell section:
    typedef typename Superclass::CellIdentifier         CellIdentifier;
    typedef typename Superclass::CellType               CellType;
    typedef typename Superclass::CellAutoPointer        CellAutoPointer;
    typedef typename Superclass::CellFeatureIdentifier  CellFeatureIdentifier;
    typedef typename Superclass::CellFeatureCount       CellFeatureCount;
    typedef typename Superclass::CellMultiVisitorType   CellMultiVisitorType;
    typedef typename Superclass::CellsContainer         CellsContainer;
    typedef typename Superclass::CellsContainerPointer  CellsContainerPointer;
    typedef typename Superclass::CellsContainerConstIterator
                                 CellsContainerConstIterator;
    typedef typename Superclass::CellsContainerIterator
                                 CellsContainerIterator;
    typedef typename Superclass::CellLinksContainer     CellLinksContainer;
    typedef typename Superclass::CellLinksContainerPointer
                                 CellLinksContainerPointer;
    typedef typename Superclass::CellLinksContainerIterator
                                 CellLinksContainerIterator;

    // Cell data section:
    typedef typename Superclass::CellDataContainer      CellDataContainer;
    typedef typename Superclass::CellDataContainerPointer
                                 CellDataContainerPointer;
    typedef typename Superclass::CellDataContainerIterator
                                 CellDataContainerIterator;

    // Point / Cell correspondance section:
    typedef typename Superclass::PointCellLinksContainer
                                 PointCellLinksContainer;
    typedef typename Superclass::PointCellLinksContainerIterator
                                 PointCellLinksContainerIterator;

    // BoundaryAssignMents section:
    typedef typename Superclass::BoundaryAssignmentsContainer
                                 BoundaryAssignmentsContainer;
    typedef typename Superclass::BoundaryAssignmentsContainerPointer
                                 BoundaryAssignmentsContainerPointer;
    typedef typename Superclass::BoundaryAssignmentsContainerVector
                                 BoundaryAssignmentsContainerVector;

    // Miscelaneous section:
    typedef typename Superclass::BoundingBoxPointer         BoundingBoxPointer;
    typedef typename Superclass::BoundingBoxType            BoundingBoxType;
    typedef typename Superclass::RegionType                 RegionType;
    typedef typename Superclass::InterpolationWeightType
                                 InterpolationWeightType;

    /** Specific types for a quad-edge structure. */
    typedef typename Traits::PrimalDataType PrimalDataType;
    typedef typename Traits::DualDataType   DualDataType;
    typedef typename Traits::QEPrimal       QEPrimal;
    typedef typename Traits::QEDual         QEDual;
    typedef typename Traits::QEPrimal       QEType;
    // See the TODO entry dated from 2005-05-28
    // struct QEType : public QEPrimal, public QEDual {};
    typedef typename Traits::VertexRefType  VertexRefType;
    typedef typename Traits::FaceRefType    FaceRefType;
    typedef typename Traits::VectorType     VectorType;

    /** Possible specialized cell types. */
    typedef itkQE::LineCell< CellType >     EdgeCellType;
    typedef itkQE::PolygonCell< CellType >  PolygonCellType;

    /** Free insertion indexes. */
    typedef std::queue< PointIdentifier > FreePointIndexesType;
    typedef std::queue< CellIdentifier >  FreeCellIndexesType;

    /** Auxiliary types. */
    typedef std::vector< PointIdentifier > PointIdList;
    typedef std::list< QEPrimal* > EdgeListType;
    typedef EdgeListType* EdgeListPointerType;

    /// Reserved PointIdentifier designated to represent the absence of Point
    static const PointIdentifier NOPOINT;
    /// Reserved CellIdentifier designated to represent the absence of Face
    static const CellIdentifier NOFACE;

    public:
    /** Basic itk::Object interface. */
    itkNewMacro( Self );
    itkTypeMacro( Mesh, itkMesh );
    /** FrontIterator definitions */
    itkQEDefineFrontIteratorMethodsMacro( Self );

    public:
    virtual bool RequestedRegionIsOutsideOfTheBufferedRegion( )
        { return( false ); }

    virtual void Clear( );
    /** Overloaded to avoid a bug in itk::Mesh that prevents proper inheritance
     * Refer to
     * http://public.kitware.com/pipermail/insight-users/2005-March/012459.html
     * and
     * http://public.kitware.com/pipermail/insight-users/2005-April/012613.html
     */
    virtual void CopyInformation( const itk::DataObject* data ) { (void)data; }
    /// One of the reasons of itkQE is precisely to avoid updating connexions.
    void BuildCellLinks( ) { }

    virtual bool FindClosestPoint( CoordRepType coords[ PointDimension ],
                                   PointIdentifier* pointId ) const;
    
    /** Overloaded methods for itk-syntax compatilibity. */
    void SetCell( CellIdentifier cId, CellAutoPointer& cell );

    /** Methods to simplify point/edge insertion/search. */
    virtual PointIdentifier FindFirstUnusedPointIndex( );
    virtual CellIdentifier  FindFirstUnusedCellIndex( );
    virtual void PushOnContainer( EdgeCellType* newEdge );

    // ////////////////// Adding Point/Edge/Face methods
    virtual PointIdentifier AddPoint( const PointType& p );
    virtual QEPrimal* AddEdge( const PointIdentifier& orgPid,
                               const PointIdentifier& destPid );
    virtual void      AddFace( QEPrimal* e );
    virtual QEPrimal* AddFace( PointIdList& points );
    virtual QEPrimal* AddFace( unsigned int nPoints,
                               const PointIdentifier& p1,
                               const PointIdentifier& p2,
                               const PointIdentifier& p3, ... );
    virtual QEPrimal* AddFaceTriangle( const PointIdentifier& aPid,
                                       const PointIdentifier& bPid,
                                       const PointIdentifier& cPid );

    // ////////////////// Deletion methods
    virtual void DeletePoint( const PointIdentifier& pid );
    virtual void DeleteEdge( const PointIdentifier& orgPid,
                             const PointIdentifier& destPid );
    virtual void DeleteEdge( QEPrimal* e );
    virtual void LightWeightDeleteEdge( QEPrimal* e );
    virtual void DeleteFace( FaceRefType faceToDelete );

    // //////////////////
    virtual PointType  GetPoint ( const PointIdentifier& pid ) const;
    virtual VectorType GetVector( const PointIdentifier& pid ) const;
    virtual QEPrimal*  GetEdge( ) const;
    virtual QEPrimal*  GetEdge( const CellIdentifier& eid ) const;
    virtual QEPrimal*  FindEdge( const PointIdentifier& pid0 ) const;
    virtual QEPrimal*  FindEdge( const PointIdentifier& pid0,
                                 const PointIdentifier& pid1 ) const;

    ///  Compute the euclidian length of argument edge
    CoordRepType ComputeEdgeLength( QEPrimal* e );

    unsigned long ComputeNumberOfPoints( ) const;
    unsigned long ComputeNumberOfFaces( ) const;
    unsigned long ComputeNumberOfEdges( ) const;

    PointIdentifier Splice( QEPrimal* a, QEPrimal* b );

    protected:
    /** Memory management methods. */
    Mesh( );
    virtual ~Mesh( ) { }

    private:
    Mesh( const Self& );           // Not impl.
    void operator=( const Self& ); // Not impl.

    protected:
    FreePointIndexesType m_FreePointIndexes;
    FreeCellIndexesType  m_FreeCellIndexes;

};

} // enamespace

#include "itkQEMeshMacro.h"
#include "itkQEMesh.txx"

#endif // __ITKQUADEDGEMESH__MESH__H__

// eof - itkQEMesh.h
