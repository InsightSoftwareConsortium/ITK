/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkQuadEdge_h
#define itkQuadEdge_h

#include "itkQuadEdgeMeshBaseIterator.h"
#include "ITKQuadEdgeMeshExport.h"

#include "itkMacro.h"

// Debugging macros for classes that do not derive from the itkObject.
// FIXME: Maybe variations of these macros should be moved into
// itkMacro.h
//
#define itkQEDebugMacro(x)                                        \
    {                                                             \
    std::ostringstream itkmsg;                                    \
    itkmsg << "Debug: In " __FILE__ ", line " << __LINE__ << "\n" \
           << " (" << this << "): " x                             \
           << "\n\n";                                             \
    OutputWindowDisplayDebugText( itkmsg.str().c_str() );         \
    }
#define itkQEWarningMacro(x)                                        \
    {                                                               \
    std::ostringstream itkmsg;                                      \
    itkmsg << "WARNING: In " __FILE__ ", line " << __LINE__ << "\n" \
           << " (" << this << "): " x                               \
           << "\n\n";                                               \
    OutputWindowDisplayWarningText( itkmsg.str().c_str() );         \
    }

// -------------------------------------------------------------------------
/**
 * Macro that defines overloaded members for the second order
 * topological accessors.
 *
 * @param st Superclass type.
 * @param pt Primal edge type.
 * @param dt Dual edge type.
 * \todo Should this macro be added to doxygen macros?
 */
#define itkQEAccessorsMacro(st, pt, dt)                               \
  pt * GetOnext()                                                     \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetOnext() ) );         \
    }                                                                 \
                                                                      \
  dt *GetRot()                                                        \
    {                                                                 \
    return ( dynamic_cast<  dt * >( this->st::GetRot() ) );           \
    }                                                                 \
                                                                      \
  pt *GetSym()                                                        \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetSym() ) );           \
    }                                                                 \
                                                                      \
  pt *GetLnext()                                                      \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetLnext() ) );         \
    }                                                                 \
                                                                      \
  pt *GetRnext()                                                      \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetRnext() ) );         \
    }                                                                 \
                                                                      \
  pt *GetDnext()                                                      \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetDnext() ) );         \
    }                                                                 \
                                                                      \
  pt *GetOprev()                                                      \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetOprev() ) );         \
    }                                                                 \
                                                                      \
  pt *GetLprev()                                                      \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetLprev() ) );         \
    }                                                                 \
                                                                      \
  pt *GetRprev()                                                      \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetRprev() ) );         \
    }                                                                 \
                                                                      \
  pt *GetDprev()                                                      \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetDprev() ) );         \
    }                                                                 \
                                                                      \
  dt *GetInvRot()                                                     \
    {                                                                 \
    return ( dynamic_cast< dt * >( this->st::GetInvRot() ) );         \
    }                                                                 \
                                                                      \
  pt *GetInvOnext()                                                   \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetInvOnext() ) );      \
    }                                                                 \
                                                                      \
  pt *GetInvLnext()                                                   \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetInvLnext() ) );      \
    }                                                                 \
                                                                      \
  pt *GetInvRnext()                                                   \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetInvRnext() ) );      \
    }                                                                 \
                                                                      \
  pt *GetInvDnext()                                                   \
    {                                                                 \
    return ( dynamic_cast<  pt * >( this->st::GetInvDnext() ) );      \
    }                                                                 \
  const pt *GetOnext() const                                          \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetOnext() ) );    \
    }                                                                 \
                                                                      \
  const dt *GetRot() const                                            \
    {                                                                 \
    return ( dynamic_cast< const dt * >( this->st::GetRot() ) );      \
    }                                                                 \
                                                                      \
  const pt *GetSym() const                                            \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetSym() ) );      \
    }                                                                 \
                                                                      \
  const pt *GetLnext() const                                          \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetLnext() ) );    \
    }                                                                 \
                                                                      \
  const pt *GetRnext() const                                          \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetRnext() ) );    \
    }                                                                 \
                                                                      \
  const pt *GetDnext() const                                          \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetDnext() ) );    \
    }                                                                 \
                                                                      \
  const pt *GetOprev() const                                          \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetOprev() ) );    \
    }                                                                 \
                                                                      \
  const pt *GetLprev() const                                          \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetLprev() ) );    \
    }                                                                 \
                                                                      \
  const pt *GetRprev() const                                          \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetRprev() ) );    \
    }                                                                 \
                                                                      \
  const pt *GetDprev() const                                          \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetDprev() ) );    \
    }                                                                 \
                                                                      \
  const dt *GetInvRot() const                                         \
    {                                                                 \
    return ( dynamic_cast< const dt * >( this->st::GetInvRot() ) );   \
    }                                                                 \
                                                                      \
  const pt *GetInvOnext() const                                       \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetInvOnext() ) ); \
    }                                                                 \
                                                                      \
  const pt *GetInvLnext() const                                       \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetInvLnext() ) ); \
    }                                                                 \
                                                                      \
  const pt *GetInvRnext() const                                       \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetInvRnext() ) ); \
    }                                                                 \
                                                                      \
  const pt *GetInvDnext() const                                       \
    {                                                                 \
    return ( dynamic_cast< const pt * >( this->st::GetInvDnext() ) ); \
    }

namespace itk
{
/** \class QuadEdge
 * \brief Base class for the implementation of a quad-edge data structure as
 * proposed in "Guibas and Stolfi 1985"
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/306
 *
 * \sa "Accessing adjacent edges."
 *
 * \ingroup MeshObjects
 * \ingroup ITKQuadEdgeMesh
 */

class ITKQuadEdgeMesh_EXPORT QuadEdge
{
public:
  /** Hierarchy typedefs & values. */
  typedef QuadEdge Self;

  /** Iterator types. */
  typedef QuadEdgeMeshIterator< Self >      Iterator;
  typedef QuadEdgeMeshConstIterator< Self > ConstIterator;

  /** Basic iterators methods. */
  inline itkQEDefineIteratorMethodsMacro(Onext);
  // itkQEDefineIteratorMethodsMacro( Sym );
  // itkQEDefineIteratorMethodsMacro( Lnext );
  // itkQEDefineIteratorMethodsMacro( Rnext );
  // itkQEDefineIteratorMethodsMacro( Dnext );
  // itkQEDefineIteratorMethodsMacro( Oprev );
  // itkQEDefineIteratorMethodsMacro( Lprev );
  // itkQEDefineIteratorMethodsMacro( Rprev );
  // itkQEDefineIteratorMethodsMacro( Dprev );
  // itkQEDefineIteratorMethodsMacro( InvOnext );
  // itkQEDefineIteratorMethodsMacro( InvLnext );
  // itkQEDefineIteratorMethodsMacro( InvRnext );
  // itkQEDefineIteratorMethodsMacro( InvDnext );

  /** Object creation methods. */
  QuadEdge();
  virtual ~QuadEdge();

  /** Sub-algebra Set methods. */
  inline void SetOnext(Self *onext) { this->m_Onext = onext; }
  inline void SetRot(Self *rot)     { this->m_Rot = rot; }

  /** Sub-algebra Get methods.
   *  Returns edge with same Origin (see
   *  "Accessing adjacent edges"). */
  inline Self * GetOnext() { return this->m_Onext; }
  inline Self * GetRot()   { return this->m_Rot; }
  inline const Self * GetOnext() const { return this->m_Onext; }
  inline const Self * GetRot() const { return this->m_Rot; }

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
   *    use itk::QuadEdgeMesh::Splice it's geometry aware version.
   *
   */
// TODO fix this ref
//   * \sa \ref DoxySurgeryConnectivity
  inline void Splice(Self *b)
  {
    Self *aNext     = this->GetOnext();
    Self *bNext     = b->GetOnext();
    Self *alpha     = aNext->GetRot();
    Self *beta      = bNext->GetRot();
    Self *alphaNext = alpha->GetOnext();
    Self *betaNext  = beta->GetOnext();

    this->SetOnext(bNext);
    b->SetOnext(aNext);
    alpha->SetOnext(betaNext);
    beta->SetOnext(alphaNext);
  }

  //  Second order accessors.

  /** Returns the symetric edge
   * (see "Accessing adjacent edges"). */
  inline Self * GetSym()
  {
    if ( this->m_Rot )
      {
      return ( this->m_Rot->m_Rot );
      }
    return ( this->m_Rot );
  }

  inline const Self * GetSym() const
  {
    if ( this->m_Rot )
      {
      return ( this->m_Rot->m_Rot );
      }
    return ( this->m_Rot );
  }

  /** Returns next edge with same Left face
   * (see "Accessing adjacent edges"). */
  Self * GetLnext();

  const Self * GetLnext() const;

  /** Returns next edge with same Right face. The first edge
   * encountered when moving counter-clockwise from e around e->Right.
   * (see "Accessing adjacent edges"). */
  Self * GetRnext();

  const Self * GetRnext() const;

  /** Returns next edge with same right face and same Destination. The
   *  first edge encountered when moving counter-clockwise from e
   *  (see "Accessing adjacent edges"). */
  Self * GetDnext();

  const Self * GetDnext() const;

  /** Returns previous edge with same Origin
   *  (see "Accessing adjacent edges"). */
  Self * GetOprev();

  const Self * GetOprev() const;

  /** Returns previous edge with same Left face. The first edge
   *  encountered when moving clockwise from e around e->Left.
   * (see "Accessing adjacent edges"). */
  Self * GetLprev();

  const Self * GetLprev() const;

  /** Returns the previous edge with same Right face. The first edge
   *  encountered when moving clockwise from e around e->Right.
   *  (see "Accessing adjacent edges"). */
  Self * GetRprev();

  const Self * GetRprev() const;

  /** Returns the previous edge with same Right face and same Destination.
   *  The first edge encountered when moving clockwise from e around e->Dest.
   *  (see "Accessing adjacent edges"). */
  Self * GetDprev();

  const Self * GetDprev() const;

  /** Inverse operators */
  inline Self * GetInvRot()
  {
#ifdef NDEBUG
    return ( this->GetRot()->GetRot()->GetRot() );
#else
    Self *p1 = this->GetRot();
    if ( !p1 ) { return ITK_NULLPTR; }
    Self *p2 = p1->GetRot();
    if ( !p2 ) { return ITK_NULLPTR; }
    Self *p3 = p2->GetRot();
    if ( !p3 ) { return ITK_NULLPTR; }
    return p3;
#endif
  }

  inline Self * GetInvOnext() { return this->GetOprev(); }
  inline Self * GetInvLnext() { return this->GetLprev(); }
  inline Self * GetInvRnext() { return this->GetRprev(); }
  inline Self * GetInvDnext() { return this->GetDprev(); }
  inline const Self * GetInvRot() const
  {
#ifdef NDEBUG
    return ( this->GetRot()->GetRot()->GetRot() );
#else
    const Self *p1 = this->GetRot();
    if ( !p1 ) { return ITK_NULLPTR; }
    const Self *p2 = p1->GetRot();
    if ( !p2 ) { return ITK_NULLPTR; }
    const Self *p3 = p2->GetRot();
    if ( !p3 ) { return ITK_NULLPTR; }
    return p3;
#endif
  }

  inline const Self * GetInvOnext() const { return this->GetOprev(); }
  inline const Self * GetInvLnext() const { return this->GetLprev(); }
  inline const Self * GetInvRnext() const { return this->GetRprev(); }
  inline const Self * GetInvDnext() const { return this->GetDprev(); }

  /** Queries. */
  inline bool IsHalfEdge() const { return ( ( m_Onext == this ) || ( m_Rot == ITK_NULLPTR ) ); }
  inline bool IsIsolated() const { return ( this == this->GetOnext() ); }
  bool IsEdgeInOnextRing(Self *testEdge) const;

  bool IsLnextGivenSizeCyclic(const int size) const;

  unsigned int GetOrder() const;

private:
  Self *m_Onext; /**< Onext ring */
  Self *m_Rot;   /**< Rot ring */
};
}

#endif
