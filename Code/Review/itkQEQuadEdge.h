// -------------------------------------------------------------------------
// itkQEQuadEdge.h
// $Revision: 1.2 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-10 21:20:01 $
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

#ifndef __ITKQUADEDGEMESH__QUADEDGE__H__
#define __ITKQUADEDGEMESH__QUADEDGE__H__

#include "itkQEBaseIterator.h"
#include "itkQECommon.h"

// -------------------------------------------------------------------------
/**
 * Macro that defines overloaded members for the second order topological
 * accessors.
 *
 * @param st Superclass type.
 * @param pt Primal edge type.
 * @param dt Dual edge type.
 */
#define itkQEAccessorsMacro( st, pt, dt )                               \
  pt* GetOnext()                                                        \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetOnext() ) );              \
    }                                                                   \
                                                                        \
  dt* GetRot()                                                          \
    {                                                                   \
    return( dynamic_cast< dt* >( this->st::GetRot() ) );                \
    }                                                                   \
                                                                        \
  pt* GetSym()                                                          \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetSym() ) );                \
    }                                                                   \
                                                                        \
  pt* GetLnext()                                                        \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetLnext() ) );              \
    }                                                                   \
                                                                        \
  pt* GetRnext()                                                        \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetRnext() ) );              \
    }                                                                   \
                                                                        \
  pt* GetDnext()                                                        \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetDnext() ) );              \
    }                                                                   \
                                                                        \
  pt* GetOprev()                                                        \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetOprev() ) );              \
    }                                                                   \
                                                                        \
  pt* GetLprev()                                                        \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetLprev() ) );              \
    }                                                                   \
                                                                        \
  pt* GetRprev()                                                        \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetRprev() ) );              \
    }                                                                   \
                                                                        \
  pt* GetDprev()                                                        \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetDprev() ) );              \
    }                                                                   \
                                                                        \
  dt* GetInvRot()                                                       \
    {                                                                   \
    return( dynamic_cast< dt* >( this->st::GetInvRot() ) );             \
    }                                                                   \
                                                                        \
  pt* GetInvOnext()                                                     \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetInvOnext() ) );           \
    }                                                                   \
                                                                        \
  pt* GetInvLnext()                                                     \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetInvLnext() ) );           \
    }                                                                   \
                                                                        \
  pt* GetInvRnext()                                                     \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetInvRnext() ) );           \
    }                                                                   \
                                                                        \
  pt* GetInvDnext()                                                     \
    {                                                                   \
    return( dynamic_cast< pt* >( this->st::GetInvDnext() ) );           \
    }

// -------------------------------------------------------------------------
/**
 * Macro that defines the method that creates a complete definition of a
 * quad-edge: The Onext ring and the Rot sub-algebra.
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

namespace itkQE
{
/**
 * Base class for the implementation of a quad-edge data structure as
 * proposed in \ref DoxyReferencesGuibasStolfi "Guibas and Stolfi 1985"
 * \sa \ref DoxyWalkingLocalShort "Accessing adjacent edges."
 */
class QuadEdge
{
    public:
    /** Hierarchy typedefs & values. */
    typedef QuadEdge Self;

    /** Basic iterators methods. */
    itkQEDefineAllIteratorMethodsMacro;

    /** Object creation methods. */
    QuadEdge();
    virtual ~QuadEdge() { }

    void Splice( Self* b );

    /** Sub-algebra set methods. */
    void SetOnext( Self* onext ) { m_Onext = onext; }
    void SetRot( Self* rot )     { m_Rot = rot; }

    /** Sub-algebra get methods. */
    /// NEXT edge with same Origin
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetOnext() { return( m_Onext ); }
    Self* GetRot()   { return( m_Rot ); }

    //  Second order accessors.

    /// SYMmetric edge
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetSym()   { return( this->GetRot()->GetRot() ); }

    /// NEXT edge with same Left face
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetLnext() { return( this->GetInvRot()->GetOnext()->GetRot() ); }

    /// NEXT edge with same Right face [i.e. the first edge encountered
    /// when moving counter-clockwise from e around e->Right ]
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetRnext() { return( this->GetRot()->GetOnext()->GetInvRot() ); }
    /// NEXT edge with same right face and same Destination [i.e. the
    /// first edge encountered when moving counter-clockwise from e
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetDnext() { return( this->GetSym()->GetOnext()->GetSym() ); }
    /// PREVious edge with same Origin
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetOprev() { return( this->GetRot()->GetOnext()->GetRot() ); }
    /// PREVious edge with same Left face [i.e. the first edge encountered
    /// when moving clockwise from e around e->Left ]
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetLprev() { return( this->GetOnext()->GetSym() ); }

    /// PREVious edge with same Right face [i.e. the first edge
    /// encountered when moving clockwise from e around e->Right ]
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetRprev() { return( this->GetSym()->GetOnext() ); }

    /// PREVious edge with same Right face and same Destination
    /// [i.e. the first edge encountered when moving clockwise from e
    /// around e->Dest ]
    /// (see \ref DoxyWalkingLocalShort "Accessing adjacent edges").
    Self* GetDprev() { return( this->GetInvRot()->GetOnext()->GetInvRot() ); }

    // Inverse operators
    Self* GetInvRot()   { return( this->GetRot()->GetRot()->GetRot() ); }
    Self* GetInvOnext() { return( this->GetOprev() ); }
    Self* GetInvLnext() { return( this->GetLprev() ); }
    Self* GetInvRnext() { return( this->GetRprev() ); }
    Self* GetInvDnext() { return( this->GetDprev() ); }

    /** Queries. */
    bool IsHalfEdge() {return( m_Onext == (Self*)0 || m_Rot == (Self*)0 );}
    bool IsIsolated() { return( this == this->GetOnext() ); }
    bool IsEdgeInOnextRing( Self* testEdge );
    bool IsLnextGivenSizeCyclic( const int size );
    unsigned int GetOrder() const;

    protected:
    Self* m_Onext; /// Onext ring
    Self* m_Rot;   /// Rot ring
};

} // namespace

#endif // __ITKQUADEDGEMESH__QUADEDGE__H__

// eof - itkQEQuadEdge.h
