// -------------------------------------------------------------------------
// itkQuadEdge.h
// $Revision: 1.8 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-23 18:04:40 $
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
   *
   * It is its own inverse. It works in two ways:
   *
   *   1. If this->GetOrg() != b->GetOrg(), it slice a face in two.
   *   2. If this->GetOrg() == b->GetOrg(), it unifies two faces.
   *
   * \warning This class only handles of the connectivity and is not aware
   *    of the geometry that lies at the \ref GeometricalQuadEdge level.
   *    It is strongly discouraged to use this method. Instead you should
   *    use \ref itkQE::Mesh::Splice it's geometry aware version.
   *
   * \sa \ref DoxySurgeryConnectivity
   */
  void Splice( Self* b );


  //  Second order accessors.

  /** Returns the symetric edge
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
   *  encountered when moving clockwise from e around e->Left.  
   * (see \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetLprev();
  const Self* GetLprev() const;

  /** Returns the previous edge with same Right face. The first edge
   *  encountered when moving clockwise from e around e->Right.
   *  (see \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetRprev();
  const Self* GetRprev() const;

  /** Returns the previous edge with same Right face and same Destination.
   *  The first edge encountered when moving clockwise from e around e->Dest.
   *  (see \ref DoxyWalkingLocalShort "Accessing adjacent edges"). */
  Self* GetDprev();
  const Self* GetDprev() const;

  /** Inverse operators */
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
  bool IsHalfEdge() const;
  bool IsIsolated() const;
  bool IsEdgeInOnextRing( Self* testEdge ) const;
  bool IsLnextGivenSizeCyclic( const int size ) const;
  unsigned int GetOrder() const;

protected: // FIXME: should be private
  Self* m_Onext; /// Onext ring
  Self* m_Rot;   /// Rot ring
};

} 

#endif 
