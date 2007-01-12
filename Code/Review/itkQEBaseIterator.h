// -------------------------------------------------------------------------
// itkQEBaseIterator.h
// $Revision: 1.8 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-12 21:29:43 $
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

#ifndef __ITKQUADEDGEMESH__BASEITERATOR__H__
#define __ITKQUADEDGEMESH__BASEITERATOR__H__

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


namespace itkQE
{
  /**
   *  Base iterator class.
   */
  template< typename TQuadEdge >
    class BaseIterator
    {
      public:
        // Hierarchy typedefs & values.
        typedef BaseIterator Self;
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
        BaseIterator( QuadEdgeType* e,
            int op = OperatorOnext,
            bool start = true )
          : m_StartEdge( e ), m_Iterator( e ),
          m_OpType( op ), m_Start( start ) {}

        virtual ~BaseIterator() {}

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
        bool operator==( BaseIterator& r )
        {
          return ( m_StartEdge == r.m_StartEdge ) &&
              ( m_Iterator  == r.m_Iterator )  &&
              ( m_OpType    == r.m_OpType )  &&
              ( m_Start     == r.m_Start );
        }

        bool operator==( const BaseIterator& r ) const
        {
          return ( m_StartEdge == r.m_StartEdge ) &&
                    ( m_Iterator  == r.m_Iterator )  &&
                    ( m_OpType    == r.m_OpType )  &&
                    ( m_Start     == r.m_Start );
        }

        bool operator!=( BaseIterator& r )
        {
          return !( this->operator==( r ) );
        }

        bool operator!=( const BaseIterator& r ) const
          {
          return !( this->operator==( r ) );
          }

        BaseIterator& operator++()
          {
          if( m_Start )
            {
              this->GoToNext();
              m_Start = !( m_Iterator == m_StartEdge );
            }

          return *this;
        }

        BaseIterator& operator++( int )
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
        QuadEdgeType* m_StartEdge; /// Start edge
        QuadEdgeType* m_Iterator;  /// Current iteration position
        int           m_OpType;    /// Operation type
        bool          m_Start;     /// Indicates iteration has just started
    };

/**
 * No const iterator.
 */
template< typename TQuadEdge >
    class Iterator
    : public BaseIterator< TQuadEdge >
    {
        public:
        /** Hierarchy typedefs and values. */
        typedef Iterator                  Self;
        typedef BaseIterator< TQuadEdge > Superclass;
        typedef TQuadEdge                 QuadEdgeType;

        public:
        /** Object creation methods. */
        Iterator( QuadEdgeType* e = (QuadEdgeType*)0,
                  int op = Superclass::OperatorOnext,
                  bool start = true )
            : Superclass( e, op, start ) {}

        virtual ~Iterator() {}

        QuadEdgeType* Value() { return this->m_Iterator; }
    };

/**
 * No const geometrical iterator.
 */
template< typename TQuadEdgeGeom >
    class IteratorGeom
    : public Iterator< TQuadEdgeGeom >
    {
        public:
        /** Hierarchy typedefs and values. */
        typedef Iterator< TQuadEdgeGeom >       Superclass;
        typedef TQuadEdgeGeom                   QuadEdgeType;

        /** Geometric value type. */
        typedef typename QuadEdgeType::OrgRefType OrgRefType;

        public:
        IteratorGeom( QuadEdgeType* e = (QuadEdgeType*)0,
                      int op = Superclass::OperatorOnext,
                      bool start = true )
            : Superclass( e, op, start ) {}
        OrgRefType operator*() { return this->m_Iterator->GetOrg(); }
    };

/**
 * Const iterator.
 */
template< typename TQuadEdge >
    class ConstIterator
    : public BaseIterator< TQuadEdge >
    {
        public:
        /** Hierarchy typedefs & values. */
        typedef ConstIterator             Self;
        typedef BaseIterator< TQuadEdge > Superclass;
        typedef Iterator< TQuadEdge >     NoConstType;
        typedef TQuadEdge                 QuadEdgeType;

        public:
        /** Object creation methods. */
        ConstIterator( const QuadEdgeType* e = (QuadEdgeType*)0,
                       int op = Superclass::OperatorOnext,
                       bool start = true )
            : Superclass( ( QuadEdgeType* )e, op, start ) {}

        virtual ~ConstIterator() {}

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
template< typename TQuadEdgeGeom >
    class ConstIteratorGeom
    : public ConstIterator< TQuadEdgeGeom >
    {
        public:
        /** Hierarchy typedefs and values. */
        typedef ConstIteratorGeom              Self;
        typedef ConstIterator< TQuadEdgeGeom > Superclass;
        typedef IteratorGeom< TQuadEdgeGeom >  NoConstType;
        typedef TQuadEdgeGeom                  QuadEdgeType;

        /** Geometric value type. */
        typedef typename QuadEdgeType::OrgRefType OrgRefType;

        public:
        ConstIteratorGeom( const QuadEdgeType* e = (QuadEdgeType*)0,
                           int op = Superclass::OperatorOnext,
                           bool start = true )
            : Superclass( e, op, start ) {}

        virtual ~ConstIteratorGeom() {}

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

} // namespace

#endif // __ITKQUADEDGEMESH__BASEITERATOR__H__

// eof - itkQEBaseIterator.h
