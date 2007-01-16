// -------------------------------------------------------------------------
// itkQuadEdgeMeshBaseIterator.h
// $Revision: 1.2 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-16 22:30:06 $
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

#ifndef __itkQuadEdgeMeshBaseIterator_h
#define __itkQuadEdgeMeshBaseIterator_h

// -------------------------------------------------------------------------
#define itkQEDefineIteratorMethodsMacro( Op )                             \
  virtual Iterator Begin##Op()                                            \
    {                                                                     \
    return Iterator( this, Self::Iterator::Operator##Op, true );          \
    }                                                                     \
                                                                          \
  virtual ConstIterator Begin##Op() const                                 \
    {                                                                     \
    return ConstIterator( this, Self::ConstIterator::Operator##Op,        \
    true );                                                               \
    }                                                                     \
                                                                          \
  virtual Iterator End##Op()                                              \
    {                                                                     \
    return Iterator( this, Self::Iterator::Operator##Op, false );         \
    }                                                                     \
                                                                          \
  virtual ConstIterator End##Op() const                                   \
    {                                                                     \
    return ConstIterator( this, Self::ConstIterator::Operator##Op,        \
    false );                                                              \
    }

// -------------------------------------------------------------------------
#define itkQEDefineIteratorGeomMethodsMacro( Op )                         \
  virtual IteratorGeom BeginGeom##Op()                                    \
    {                                                                     \
    return IteratorGeom( this, Self::IteratorGeom::Operator##Op,          \
    true );                                                               \
    }                                                                     \
                                                                          \
  virtual ConstIteratorGeom BeginGeom##Op() const                         \
    {                                                                     \
    return ConstIteratorGeom( this,                                       \
    Self::ConstIteratorGeom::Operator##Op, true );                        \
    }                                                                     \
                                                                          \
  virtual IteratorGeom EndGeom##Op()                                      \
    {                                                                     \
    return IteratorGeom( this, Self::IteratorGeom::Operator##Op,          \
    false );                                                              \
    }                                                                     \
                                                                          \
  virtual ConstIteratorGeom EndGeom##Op() const                           \
    {                                                                     \
    return ConstIteratorGeom( this,                                       \
    Self::ConstIteratorGeom::Operator##Op, false );                       \
    }


namespace itk
{
  /**
   *  Base iterator class.
   */
  template< typename TQuadEdge >
    class QuadEdgeMeshBaseIterator
{
public:
  // Hierarchy typedefs & values.
  typedef QuadEdgeMeshBaseIterator Self;
  typedef TQuadEdge    QuadEdgeType;

  // Different types of iterators, one for each basic QE operation.
  enum
  {
    OperatorOnext    =  0,
    OperatorSym      =  1,
    OperatorLnext    =  2,
    OperatorRnext    =  3,
    OperatorDnext    =  4,
    OperatorOprev    =  5,
    OperatorLprev    =  6,
    OperatorRprev    =  7,
    OperatorDprev    =  8,
    OperatorInvOnext =  9,
    OperatorInvLnext = 10,
    OperatorInvRnext = 11,
    OperatorInvDnext = 12
  };

public:
  // Object creation methods.
  QuadEdgeMeshBaseIterator( QuadEdgeType* e,
      int op = OperatorOnext,
      bool start = true )
    : m_StartEdge( e ), m_Iterator( e ),
    m_OpType( op ), m_Start( start ) {}

  virtual ~QuadEdgeMeshBaseIterator() {}

  Self& operator=( const Self& r )
    {
    m_StartEdge = r.m_StartEdge;
    m_Iterator = r.m_Iterator;
    m_OpType = r.m_OpType;
    m_Start = r.m_Start;
    return *this;
    }

  QuadEdgeType* GetStartEdge() const { return m_StartEdge; }
  QuadEdgeType* GetIterator() const  { return m_Iterator; }
  int           GetOpType() const    { return m_OpType; }
  bool          GetStart() const     { return m_Start; }

  /** Iteration methods. */
  bool operator==( Self & r )
  {
    return ( m_StartEdge == r.m_StartEdge ) &&
        ( m_Iterator  == r.m_Iterator )  &&
        ( m_OpType    == r.m_OpType )  &&
        ( m_Start     == r.m_Start );
  }

  bool operator==( const Self & r ) const
  {
    return ( m_StartEdge == r.m_StartEdge ) &&
              ( m_Iterator  == r.m_Iterator )  &&
              ( m_OpType    == r.m_OpType )  &&
              ( m_Start     == r.m_Start );
  }

  bool operator!=( Self & r )
  {
    return !( this->operator==( r ) );
  }

  bool operator!=( const Self & r ) const
    {
    return !( this->operator==( r ) );
    }

  Self & operator++()
    {
    if( m_Start )
      {
        this->GoToNext();
        m_Start = !( m_Iterator == m_StartEdge );
      }

    return *this;
    }

  Self & operator++( int )
    {
    if( m_Start )
      {
      this->GoToNext();
      m_Start = !( m_Iterator == m_StartEdge );
      }

    return *this;
    }

protected:
  /** Method that should do all the iteration work. */
  virtual void GoToNext()
    {
    switch( m_OpType ) {
    case OperatorOnext    : m_Iterator = m_Iterator->GetOnext();
                            break;
    case OperatorSym      : m_Iterator = m_Iterator->GetSym();
                            break;
    case OperatorLnext    : m_Iterator = m_Iterator->GetLnext();
                            break;
    case OperatorRnext    : m_Iterator = m_Iterator->GetRnext();
                            break;
    case OperatorDnext    : m_Iterator = m_Iterator->GetDnext();
                            break;
    case OperatorOprev    : m_Iterator = m_Iterator->GetOprev();
                            break;
    case OperatorLprev    : m_Iterator = m_Iterator->GetLprev();
                            break;
    case OperatorRprev    : m_Iterator = m_Iterator->GetRprev();
                            break;
    case OperatorDprev    : m_Iterator = m_Iterator->GetDprev();
                            break;
    case OperatorInvOnext : m_Iterator = m_Iterator->GetInvOnext();
                            break;
    case OperatorInvLnext : m_Iterator = m_Iterator->GetInvLnext();
                            break;
    case OperatorInvRnext : m_Iterator = m_Iterator->GetInvRnext();
                            break;
    case OperatorInvDnext : m_Iterator = m_Iterator->GetInvDnext();
                            break;
    default: break;
    }
  }

protected:
  QuadEdgeType * m_StartEdge; /// Start edge
  QuadEdgeType * m_Iterator;  /// Current iteration position
  int           m_OpType;    /// Operation type
  bool          m_Start;     /// Indicates iteration has just started
};

/**
 * No const iterator.
 */
template< typename TQuadEdge >
class QuadEdgeMeshIterator : public QuadEdgeMeshBaseIterator< TQuadEdge >
{
public:
  /** Hierarchy typedefs and values. */
  typedef QuadEdgeMeshIterator                  Self;
  typedef QuadEdgeMeshBaseIterator< TQuadEdge > Superclass;
  typedef TQuadEdge                 QuadEdgeType;

public:
  /** Object creation methods. */
  QuadEdgeMeshIterator( QuadEdgeType* e = (QuadEdgeType*)0,
            int op = Superclass::OperatorOnext,
            bool start = true )
      : Superclass( e, op, start ) {}

  virtual ~QuadEdgeMeshIterator() {}

  QuadEdgeType * Value() { return this->m_Iterator; }
  const QuadEdgeType * Value() const { return this->m_Iterator; }
};

/**
 * No const geometrical iterator.
 */
template< typename TGeometricalQuadEdge >
    class QuadEdgeMeshIteratorGeom
    : public QuadEdgeMeshIterator< TGeometricalQuadEdge >
    {
        public:
        /** Hierarchy typedefs and values. */
        typedef QuadEdgeMeshIterator< TGeometricalQuadEdge >       Superclass;
        typedef TGeometricalQuadEdge                   QuadEdgeType;

        /** Geometric value type. */
        typedef typename QuadEdgeType::OrgRefType OrgRefType;

        public:
        QuadEdgeMeshIteratorGeom( QuadEdgeType* e = (QuadEdgeType*)0,
                      int op = Superclass::OperatorOnext,
                      bool start = true )
            : Superclass( e, op, start ) {}
        OrgRefType operator*() { return this->m_Iterator->GetOrg(); }
    };

/**
 * Const iterator.
 */
template< typename TQuadEdge >
    class QuadEdgeMeshConstIterator
    : public QuadEdgeMeshBaseIterator< TQuadEdge >
    {
        public:
        /** Hierarchy typedefs & values. */
        typedef QuadEdgeMeshConstIterator             Self;
        typedef QuadEdgeMeshBaseIterator< TQuadEdge > Superclass;
        typedef QuadEdgeMeshIterator< TQuadEdge >     NoConstType;
        typedef TQuadEdge                 QuadEdgeType;

        public:
        /** Object creation methods. */
        QuadEdgeMeshConstIterator( const QuadEdgeType* e = (QuadEdgeType*)0,
                       int op = Superclass::OperatorOnext,
                       bool start = true )
            : Superclass( ( QuadEdgeType* )e, op, start ) {}

        virtual ~QuadEdgeMeshConstIterator() {}

        Self& operator=( const NoConstType& r )
        {
            this->m_StartEdge = r.GetStartEdge();
            this->m_Iterator = r.GetIterator();
            this->m_OpType = r.GetOpType();
            this->m_Start = r.GetStart();
            return *this;
        }

        const QuadEdgeType* Value() const { return this->m_Iterator; }
    };

/**
 * Const geometrical iterator.
 */
template< typename TGeometricalQuadEdge >
    class QuadEdgeMeshConstIteratorGeom
    : public QuadEdgeMeshConstIterator< TGeometricalQuadEdge >
    {
        public:
        /** Hierarchy typedefs and values. */
        typedef QuadEdgeMeshConstIteratorGeom              Self;
        typedef QuadEdgeMeshConstIterator< TGeometricalQuadEdge > Superclass;
        typedef QuadEdgeMeshIteratorGeom< TGeometricalQuadEdge >  NoConstType;
        typedef TGeometricalQuadEdge                  QuadEdgeType;

        /** Geometric value type. */
        typedef typename QuadEdgeType::OrgRefType OrgRefType;

        public:
        QuadEdgeMeshConstIteratorGeom( const QuadEdgeType* e = (QuadEdgeType*)0,
                           int op = Superclass::OperatorOnext,
                           bool start = true )
            : Superclass( e, op, start ) {}

        virtual ~QuadEdgeMeshConstIteratorGeom() {}

        Self& operator=( const NoConstType& r )
        {
            this->m_StartEdge = r.GetStartEdge();
            this->m_Iterator = r.GetIterator();
            this->m_OpType = r.GetOpType();
            this->m_Start = r.GetStart();
            return *this;
        }

        const OrgRefType operator*() const
        {
            return this->m_Iterator->GetOrg();
        }
    };

} 

#endif 


