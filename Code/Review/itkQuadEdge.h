// -------------------------------------------------------------------------
// itkQuadEdge.h
// $Revision: 1.6 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-23 17:29:53 $
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

#ifndef __itkQuadEdge_h
#define __itkQuadEdge_h

#include "itkQuadEdgeMeshBaseIterator.h"

#include "itkMacro.h"

// Debugging macros for classes that do not derive from the itkObject.
// FIXME: Maybe variations of these macros should be moved into
// itkMacro.h
//
#define itkQEDebugMacro( x )                                            \
    {                                                                   \
        ::itk::OStringStream itkmsg;                                    \
        itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << "\n"   \
               << " (" << this << "): " x                               \
               << "\n\n";                                               \
        ::itk::OutputWindowDisplayDebugText( itkmsg.str( ).c_str( ) );  \
    }
#define itkQEWarningMacro( x )                                          \
    {                                                                   \
        ::itk::OStringStream itkmsg;                                    \
        itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << "\n" \
               << " (" << this << "): " x                               \
               << "\n\n";                                               \
        ::itk::OutputWindowDisplayWarningText( itkmsg.str( ).c_str( ) ); \
    }


// -------------------------------------------------------------------------
/**
 * Macro that defines overloaded members for the second order
 * topological accessors.
 *
 * @param st Superclass type.
 * @param pt Primal edge type.
 * @param dt Dual edge type.
 */
#define itkQEAccessorsMacro( st, pt, dt )                               \
   pt* GetOnext()                                                       \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetOnext() ) );             \
    }                                                                   \
                                                                        \
   dt* GetRot()                                                         \
    {                                                                   \
    return( dynamic_cast<  dt* >( this->st::GetRot() ) );               \
    }                                                                   \
                                                                        \
   pt* GetSym()                                                         \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetSym() ) );               \
    }                                                                   \
                                                                        \
   pt* GetLnext()                                                       \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetLnext() ) );             \
    }                                                                   \
                                                                        \
   pt* GetRnext()                                                       \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetRnext() ) );             \
    }                                                                   \
                                                                        \
   pt* GetDnext()                                                       \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetDnext() ) );             \
    }                                                                   \
                                                                        \
   pt* GetOprev()                                                       \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetOprev() ) );             \
    }                                                                   \
                                                                        \
   pt* GetLprev()                                                       \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetLprev() ) );             \
    }                                                                   \
                                                                        \
   pt* GetRprev()                                                       \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetRprev() ) );             \
    }                                                                   \
                                                                        \
   pt* GetDprev()                                                       \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetDprev() ) );             \
    }                                                                   \
                                                                        \
   dt* GetInvRot()                                                      \
    {                                                                   \
    return( dynamic_cast< dt* >( this->st::GetInvRot() ) );             \
    }                                                                   \
                                                                        \
   pt* GetInvOnext()                                                    \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetInvOnext() ) );          \
    }                                                                   \
                                                                        \
   pt* GetInvLnext()                                                    \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetInvLnext() ) );          \
    }                                                                   \
                                                                        \
   pt* GetInvRnext()                                                    \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetInvRnext() ) );          \
    }                                                                   \
                                                                        \
   pt* GetInvDnext()                                                    \
    {                                                                   \
    return( dynamic_cast<  pt* >( this->st::GetInvDnext() ) );          \
    }                                                                   \
  const pt* GetOnext() const                                            \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetOnext() ) );        \
    }                                                                   \
                                                                        \
  const dt* GetRot() const                                              \
    {                                                                   \
    return( dynamic_cast< const dt* >( this->st::GetRot() ) );          \
    }                                                                   \
                                                                        \
  const pt* GetSym() const                                              \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetSym() ) );          \
    }                                                                   \
                                                                        \
  const pt* GetLnext() const                                            \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetLnext() ) );        \
    }                                                                   \
                                                                        \
  const pt* GetRnext() const                                            \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetRnext() ) );        \
    }                                                                   \
                                                                        \
  const pt* GetDnext() const                                            \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetDnext() ) );        \
    }                                                                   \
                                                                        \
  const pt* GetOprev() const                                            \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetOprev() ) );        \
    }                                                                   \
                                                                        \
  const pt* GetLprev() const                                            \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetLprev() ) );        \
    }                                                                   \
                                                                        \
  const pt* GetRprev() const                                            \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetRprev() ) );        \
    }                                                                   \
                                                                        \
  const pt* GetDprev() const                                            \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetDprev() ) );        \
    }                                                                   \
                                                                        \
  const dt* GetInvRot() const                                           \
    {                                                                   \
    return( dynamic_cast< const dt* >( this->st::GetInvRot() ) );       \
    }                                                                   \
                                                                        \
  const pt* GetInvOnext() const                                         \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetInvOnext() ) );     \
    }                                                                   \
                                                                        \
  const pt* GetInvLnext() const                                         \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetInvLnext() ) );     \
    }                                                                   \
                                                                        \
  const pt* GetInvRnext() const                                         \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetInvRnext() ) );     \
    }                                                                   \
                                                                        \
  const pt* GetInvDnext() const                                         \
    {                                                                   \
    return( dynamic_cast< const pt* >( this->st::GetInvDnext() ) );     \
    }

// -------------------------------------------------------------------------
/** Macro that defines the method that creates a complete definition
 * of a quad-edge: The Onext ring and the Rot sub-algebra.
 *
 * \param pt Primal edge type.
 * \param dt Dual edge type.
 */
#define itkQEMakeEdgeMacro( pt, dt )    \
    virtual void MakeEdge()            \
    {                                   \
        pt* e2 = new pt();             \
        dt* e1 = new dt();             \
        dt* e3 = new dt();             \
        this->SetRot( e1 );             \
        e1->SetRot( e2 );               \
        e2->SetRot( e3 );               \
        e3->SetRot( this );             \
        this->SetOnext( this );         \
        e1->SetOnext( e3 );             \
        e2->SetOnext( e2 );             \
        e3->SetOnext( e1 );             \
    }

namespace itk
{

/** \class QuadEdge
 * \brief Base class for the implementation of a quad-edge data structure as
 * proposed in \ref DoxyReferencesGuibasStolfi "Guibas and Stolfi 1985"
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://hdl.handle.net/1926/306
 *
 * \sa \ref DoxyWalkingLocalShort "Accessing adjacent edges."
 */
class QuadEdge
{
public:
  /** Hierarchy typedefs & values. */
  typedef QuadEdge Self;

  /** Iterator types. */
  typedef QuadEdgeMeshIterator< Self >      Iterator;
  typedef QuadEdgeMeshConstIterator< Self > ConstIterator;

  /** Basic iterators methods. */
  itkQEDefineIteratorMethodsMacro( Onext );
  itkQEDefineIteratorMethodsMacro( Sym );
  itkQEDefineIteratorMethodsMacro( Lnext );
  itkQEDefineIteratorMethodsMacro( Rnext );
  itkQEDefineIteratorMethodsMacro( Dnext );
  itkQEDefineIteratorMethodsMacro( Oprev );
  itkQEDefineIteratorMethodsMacro( Lprev );
  itkQEDefineIteratorMethodsMacro( Rprev );
  itkQEDefineIteratorMethodsMacro( Dprev );
  itkQEDefineIteratorMethodsMacro( InvOnext );
  itkQEDefineIteratorMethodsMacro( InvLnext );
  itkQEDefineIteratorMethodsMacro( InvRnext );
  itkQEDefineIteratorMethodsMacro( InvDnext );



  /** Object creation methods. */
  QuadEdge();
  virtual ~QuadEdge();

  /** Sub-algebra Set methods. */
  void SetOnext( Self* onext );
  void SetRot( Self* rot );

  /** Sub-algebra Get methods. 
   *  Returns edge with same Origin (see \ref DoxyWalkingLocalShort
   *  "Accessing adjacent edges"). */
  Self* GetOnext();
  Self* GetRot();
  const Self* GetOnext() const;
  const Self* GetRot() const;


  /**
   * \brief Basic quad-edge topological method.
   *
   * This method describes all possible topological operations on an edge.
   * It is its own inverse. It works in two ways:
   *   1. If this->GetOrg() != b->GetOrg(), it slice a face in two.
   *   2. If this->GetOrg() == b->GetOrg(), it unifies two faces.
   *
   * \warning This class only handles of the connectivity and is not aware
   *    of the geometry that lies at the \ref GeometricalQuadEdge level.
   *    It is strongly discouraged to use this method. Instead you should
   *    use \ref itkQE::Mesh::Splice it's geometry aware version.
   * \sa \ref DoxySurgeryConnectivity
   */
  void Splice( Self* b );

  //  Second order accessors.

  /** SYMmetric edge
   * (see \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetSym();
  const Self* GetSym() const; 

  /** Returns next edge with same Left face
   * (see \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetLnext(); 
  const Self* GetLnext() const;

  /** Returns next edge with same Right face. The first edge
   * encountered when moving counter-clockwise from e around e->Right.
   * (see \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetRnext();
  const Self* GetRnext() const;

  /** Returns next edge with same right face and same Destination. The
   *  first edge encountered when moving counter-clockwise from e
   *  (see \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetDnext();
  const Self* GetDnext() const;

  /** Returns previous edge with same Origin
   *  (see \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetOprev();
  const Self* GetOprev() const;

  /** Returns previous edge with same Left face. The first edge
   * encountered when moving clockwise from e around e->Left.  (see
   * \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetLprev();
  const Self* GetLprev() const;

  /// PREVious edge with same Right face [i.e. the first edge
  /// encountered when moving clockwise from e around e->Right ]
  /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
  Self* GetRprev() { return( this->GetSym()->GetOnext() ); }
  const Self* GetRprev() const { return( this->GetSym()->GetOnext() ); }

  /// PREVious edge with same Right face and same Destination
  /// [i.e. the first edge encountered when moving clockwise from e
  /// around e->Dest ]
  /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
  Self* GetDprev() { return( this->GetInvRot()->GetOnext()->GetInvRot() ); }
  const Self* GetDprev() const { return( this->GetInvRot()->GetOnext()->GetInvRot() ); }

  // Inverse operators
  Self * GetInvRot();
  Self * GetInvOnext();
  Self * GetInvLnext();
  Self * GetInvRnext();
  Self * GetInvDnext();
  const Self * GetInvRot() const;
  const Self * GetInvOnext() const;
  const Self * GetInvLnext() const;
  const Self * GetInvRnext() const;
  const Self * GetInvDnext() const;

  /** Queries. */
  bool IsHalfEdge() const { return( m_Onext == (Self*)0 || m_Rot == (Self*)0 );}
  bool IsIsolated() const { return( this == this->GetOnext() ); }
  bool IsEdgeInOnextRing( Self* testEdge ) const;
  bool IsLnextGivenSizeCyclic( const int size ) const;
  unsigned int GetOrder() const;

protected:
  Self* m_Onext; /// Onext ring
  Self* m_Rot;   /// Rot ring
};

} 

#endif 

