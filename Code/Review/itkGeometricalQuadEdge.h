// -------------------------------------------------------------------------
// itkGeometricalQuadEdge.h
// $Revision: 1.6 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-23 22:32:42 $
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

/**
 * The only purpose of the last paramater of the template is to guarantee
 * that the two types GeometricalQuadEdge<...> and GeometricalQuadEdge<...>::Dual are
 * always different (in the sense that their typeid() are different).
 * If we only had the four first parameters and assume that
 * GeometricalQuadEdge<...> gets instantiated with types such that TVRef = TFRef 
 * and TPData = TDData then this instantiation GeometricalQuadEdge<...> and
 * GeometricalQuadEdge<...>::Dual would be the same types (this is simply due to
 * the very definition of GeometricalQuadEdge<...>::Dual). This would in turn
 * make the types QEType and QEDual of \ref itkQE::Mesh identical and
 * would prevent any algorithm requiring to distinguish those types
 * (e.g. by relying on a dynamic_cast<QEType*>) to be effective.
 * This justifies the existence of last dummy template parameter and
 * it's default value.
 */
template< typename TVRef,  typename TFRef,
          typename TPData, typename TDData, bool PrimalDual = true >
class GeometricalQuadEdge
    : public QuadEdge
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
   * WARNING: Recursive template definitions work fine in linux.
   *          I haven't run tests on Win$-based PCs.
   */
  typedef GeometricalQuadEdge< TFRef, TVRef, TDData, TPData,
                        !PrimalDual > Dual;

  /** Input template parameters & values convenient renaming. */
  typedef TVRef  OrgRefType;
  typedef TFRef  DualOrgRefType;
  typedef TPData PrimalDataType;
  typedef TDData DualDataType;

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


  /** Method that creates a complete definition of a quad-edge.
   *  The Onext ring and the Rot sub-algebra.  */
  virtual void MakeEdge();

  /** QE macros. */
  itkQEAccessorsMacro( Superclass, Self, Dual );

public:
  /** Memory creation methods. */
  GeometricalQuadEdge();
  virtual ~GeometricalQuadEdge();

  /** Set methods. */
  void SetOrg( const OrgRefType v )
     { m_Org = v; }

  void SetDest( const OrgRefType v )
     { this->GetSym( )->SetOrg( v ); }

  void SetRight( const DualOrgRefType v )
     { this->GetRot( )->SetOrg( v ); }

  void SetLeft( const DualOrgRefType v )
     { this->GetInvRot( )->SetOrg( v ); }

  /**
   * Set the Left() of all the edges in the Lnext() ring of "this"
   * with the same given geometrical information.
   * @param  faceGeom Looks at most maxSize edges in the Lnext() ring.
   * @param  maxSize Sets at most maxSize edges in the Lnext() ring.
   * @return Returns true on success. False otherwise.
   */
  bool SetLnextRingWithSameLeftFace( const DualOrgRefType faceGeom,
                                     int maxSize = 100 );

  void UnsetOrg( )   { m_Org = NOPOINT; }
  void UnsetDest( )  { this->GetSym( )->UnsetOrg( ); }
  void UnsetRight( ) { this->GetRot( )->UnsetOrg( ); }
  void UnsetLeft( )  { this->GetInvRot( )->UnsetOrg( ); }

  /** Get methods. */
  //ORIENTATION_NOTE: this definition of GetLeft (or GetRight)
  // implicitely assumes that the Onext order is counter-clockwise !
  OrgRefType     GetOrg( )   { return( m_Org ); }
  OrgRefType     GetDest( )  { return( this->GetSym( )->GetOrg( ) ); }
  DualOrgRefType GetRight( ) { return( this->GetRot( )->GetOrg( ) ); }
  DualOrgRefType GetLeft( )  { return( this->GetInvRot( )->GetOrg( ) ); }

  /** Boolean accessors. */
  bool IsOrgSet( ) const;
  bool IsDestSet( ) const;
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

  bool IsOrgInternal() const;
  bool IsLnextSharingSameFace( int maxSize = 100 );
  bool IsLnextOfTriangle( );
  bool IsInOnextRing( Self* );
  bool IsInLnextRing( Self* );

  Self* GetNextBorderEdgeWithUnsetLeft( Self* edgeTest = 0 );

  bool InsertAfterNextBorderEdgeWithUnsetLeft( Self* isol,
                                               Self* hint = 0 );

  bool ReorderOnextRingBeforeAddFace( Self* second );

  /** Disconnection methods. */
  bool IsOrgDisconnected( )
  { return( this == this->GetOnext( ) ); }
  bool IsDestDisconnected( )
  { return( this->GetSym( )->IsOrgDisconnected( ) ); }
  bool IsDisconnected( )
  { return( this->IsOrgDisconnected( ) &&
            this->IsDestDisconnected( ) ); }
  void Disconnect( );

public:
  /// Reserved OrgRefType designated to represent the absence of Org
  static const OrgRefType NOPOINT;

protected:
  OrgRefType     m_Org;     /// Geometrical information
  PrimalDataType m_Data;    /// User data associated to this edge.
  bool           m_DataSet; /// Indicates if the data is set.
};

} 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGeometricalQuadEdge.txx"
#endif

#endif 

