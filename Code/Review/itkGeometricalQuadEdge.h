// -------------------------------------------------------------------------
// itkGeometricalQuadEdge.h
// $Revision: 1.9 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-24 23:57:41 $
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

#ifndef __itkGeometricalQuadEdge_h
#define __itkGeometricalQuadEdge_h

#include "itkQuadEdge.h"

namespace itk
{

/** \class GeometricalQuadEdge
 * \brief This class extends the QuadEdge by adding a reference to the Origin.
 *
 * The class is implemented in such a way that it can generate its own Dual.
 * In a physical edge, there will be four GeometricalQuadEdge. Two of them will
 * be Primal and two will be Dual. The Primal ones are parallel to the physical
 * edge and their origins relate to the mesh points. The Dual ones are
 * orthogonal to the physical edge and their origins relate to the faces at
 * each side of the physical edge.
 *
 * The only purpose of the last paramater of the template is to guarantee that
 * the two types GeometricalQuadEdge<...> and GeometricalQuadEdge<...>::Dual
 * are always different (in the sense that their typeid() are different).  If
 * we only had the four first parameters and assume that
 * GeometricalQuadEdge<...> gets instantiated with types such that TVRef =
 * TFRef and TPrimalData = TDualData then this instantiation
 * GeometricalQuadEdge<...> and GeometricalQuadEdge<...>::Dual would be the
 * same types (this is simply due to the very definition of
 * GeometricalQuadEdge<...>::Dual). This would in turn make the types QEType
 * and QEDual of \ref itkQE::Mesh identical and would prevent any algorithm
 * requiring to distinguish those types (e.g. by relying on a
 * dynamic_cast<QEType*>) to be effective.  This justifies the existence of
 * last dummy template parameter and it's default value.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://hdl.handle.net/1926/306
 
 *
 * \sa QuadEdge
 *
 * \ingroup MeshObjects
 */
template< typename TVRef,  typename TFRef,
          typename TPrimalData, typename TDualData, 
          bool PrimalDual = true >
class GeometricalQuadEdge : public QuadEdge
{
public:
  /** Hierarchy typedefs. */
  typedef GeometricalQuadEdge Self;
  typedef QuadEdge     Superclass;
  typedef Self*        RawPointer;

  /**
   * Dual type, basically the same type with swapped template
   * parameters.
   *
   */
  typedef GeometricalQuadEdge< TFRef, TVRef, 
    TDualData, TPrimalData, !PrimalDual >         DualType;

  /** Input template parameters & values convenient renaming. */
  typedef TVRef               OriginRefType;
  typedef TFRef               DualOriginRefType;
  typedef TPrimalData         PrimalDataType;
  typedef TDualData           DualDataType;

public:

  /** Iterator types. */
  typedef QuadEdgeMeshIteratorGeom< Self >      IteratorGeom;
  typedef QuadEdgeMeshConstIteratorGeom< Self > ConstIteratorGeom;

  /** Basic iterators methods. */
  itkQEDefineIteratorGeomMethodsMacro( Onext );
  itkQEDefineIteratorGeomMethodsMacro( Sym );
  itkQEDefineIteratorGeomMethodsMacro( Lnext );
  itkQEDefineIteratorGeomMethodsMacro( Rnext );
  itkQEDefineIteratorGeomMethodsMacro( Dnext );
  itkQEDefineIteratorGeomMethodsMacro( Oprev );
  itkQEDefineIteratorGeomMethodsMacro( Lprev );
  itkQEDefineIteratorGeomMethodsMacro( Rprev );
  itkQEDefineIteratorGeomMethodsMacro( Dprev );
  itkQEDefineIteratorGeomMethodsMacro( InvOnext );
  itkQEDefineIteratorGeomMethodsMacro( InvLnext );
  itkQEDefineIteratorGeomMethodsMacro( InvRnext );
  itkQEDefineIteratorGeomMethodsMacro( InvDnext );


  /** QE macros. */
  itkQEAccessorsMacro( Superclass, Self, DualType );

public:
  /** Memory creation methods. */
  GeometricalQuadEdge();
  virtual ~GeometricalQuadEdge();

  /** Set methods. */
  void SetOrigin( const OriginRefType v )
     { m_Origin = v; }

  void SetDestination( const OriginRefType v )
     { this->GetSym( )->SetOrigin( v ); }

  void SetRight( const DualOriginRefType v )
     { this->GetRot( )->SetOrigin( v ); }

  void SetLeft( const DualOriginRefType v )
     { this->GetInvRot( )->SetOrigin( v ); }

  /**
   * Set the Left() of all the edges in the Lnext() ring of "this"
   * with the same given geometrical information.
   * @param  faceGeom Looks at most maxSize edges in the Lnext() ring.
   * @param  maxSize Sets at most maxSize edges in the Lnext() ring.
   * @return Returns true on success. False otherwise.
   */
  bool SetLnextRingWithSameLeftFace( const DualOriginRefType faceGeom,
                                     int maxSize = 100 );

  void UnsetOrigin( )   { m_Origin = NOPOINT; }
  void UnsetDestination( )  { this->GetSym( )->UnsetOrigin( ); }
  void UnsetRight( ) { this->GetRot( )->UnsetOrigin( ); }
  void UnsetLeft( )  { this->GetInvRot( )->UnsetOrigin( ); }

  /** Get methods. */
  //ORIENTATION_NOTE: this definition of GetLeft (or GetRight)
  // implicitely assumes that the Onext order is counter-clockwise !
  OriginRefType     GetOrigin( )   { return( m_Origin ); }
  OriginRefType     GetDestination( )  { return( this->GetSym( )->GetOrigin( ) ); }
  DualOriginRefType GetRight( ) { return( this->GetRot( )->GetOrigin( ) ); }
  DualOriginRefType GetLeft( )  { return( this->GetInvRot( )->GetOrigin( ) ); }

  /** Boolean accessors. */
  bool IsOriginSet( ) const;
  bool IsDestinationSet( ) const;
  bool IsRightSet( ) const;
  bool IsLeftSet() const;

  /** Extra data set methods. */
  void SetPrimalData( const PrimalDataType data )
  { m_Data = data; this->SetPrimalData( ); }
  void SetDualData( const DualDataType data )
  { this->GetRot( )->SetPrimalData( data ); }

  void SetPrimalData( ) { m_DataSet = true; }
  void SetDualData( )   { this->GetRot( )->SetPrimalData( ); }

  void UnsetPrimalData( ) { m_Data = false; }
  void UnsetDualData( )   { this->GetRot( )->UnsetPrimalData( ); }

  /** Extra data get methods. */
  PrimalDataType GetPrimalData( ) { return( m_Data ); }
  DualDataType   GetDualData( )
  { return( this->GetRot( )->GetPrimalData( ) ); }

  /** Boolean accessors. */
  bool IsPrimalDataSet( ) { return( m_DataSet ); }
  bool IsDualDataSet( )
  { return( this->GetRot( )->IsPrimalDataSet( ) ); }

  /**
   * @return Returns true when "this" has no faces set on both sides.
   *         Return false otherwise.
   */
  bool IsWire( )
  { return( !( this->IsLeftSet( ) ) && !( this->IsRightSet( ) ) ); }

  /**
   * @return Returns true when "this" is on the boundary i.e.
   *         one and only one of the faces is set. Return false
   *         otherwise.
   */
  bool IsAtBorder( )
  {
      return( ( this->IsLeftSet( ) && !this->IsRightSet( ) ) ||
              ( !this->IsLeftSet( ) && this->IsRightSet( ) ) );
  }

  /**
   * @return Returns true when "this" has faces set on both sides.
   *         Return false otherwise.
   */
  bool IsInternal() const
     { return( this->IsLeftSet() && this->IsRightSet() ); }

  bool IsOriginInternal() const;
  bool IsLnextSharingSameFace( int maxSize = 100 );
  bool IsLnextOfTriangle( );
  bool IsInOnextRing( Self* );
  bool IsInLnextRing( Self* );

  Self* GetNextBorderEdgeWithUnsetLeft( Self* edgeTest = 0 );

  bool InsertAfterNextBorderEdgeWithUnsetLeft( Self* isol,
                                               Self* hint = 0 );

  bool ReorderOnextRingBeforeAddFace( Self* second );

  /** Disconnection methods. */
  bool IsOriginDisconnected( )
  { return( this == this->GetOnext( ) ); }
  bool IsDestinationDisconnected( )
  { return( this->GetSym( )->IsOriginDisconnected( ) ); }
  bool IsDisconnected( )
  { return( this->IsOriginDisconnected( ) &&
            this->IsDestinationDisconnected( ) ); }
  void Disconnect( );

public:
  // Reserved OriginRefType designated to represent the absence of Origin
  static const OriginRefType NOPOINT;

protected:
  OriginRefType     m_Origin;    // Geometrical information
  PrimalDataType    m_Data;      // User data associated to this edge.
  bool              m_DataSet;   // Indicates if the data is set.
};

} 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGeometricalQuadEdge.txx"
#endif

#endif 

