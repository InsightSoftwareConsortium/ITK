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
#ifndef itkQuadEdgeMeshBaseIterator_h
#define itkQuadEdgeMeshBaseIterator_h

#include "itkMacro.h"

// -------------------------------------------------------------------------
#define itkQEDefineIteratorMethodsMacro(Op)                         \
  virtual Iterator Begin##Op()                                    \
    {                                                               \
    return Iterator(this, Self::Iterator::Operator##Op, true);    \
    }                                                               \
                                                                    \
  virtual ConstIterator Begin##Op() const                         \
    {                                                               \
    return ConstIterator(this, Self::ConstIterator::Operator##Op, \
                         true);                                     \
    }                                                               \
                                                                    \
  virtual Iterator End##Op()                                      \
    {                                                               \
    return Iterator(this, Self::Iterator::Operator##Op, false);   \
    }                                                               \
                                                                    \
  virtual ConstIterator End##Op() const                           \
    {                                                               \
    return ConstIterator(this, Self::ConstIterator::Operator##Op, \
                         false);                                    \
    }

// -------------------------------------------------------------------------
#define itkQEDefineIteratorGeomMethodsMacro(Op)                               \
  virtual IteratorGeom BeginGeom##Op()                                      \
    {                                                                         \
    return IteratorGeom(this, Self::IteratorGeom::Operator##Op,             \
                        true);                                                \
    }                                                                         \
                                                                              \
  virtual ConstIteratorGeom BeginGeom##Op() const                           \
    {                                                                         \
    return ConstIteratorGeom(this,                                            \
                             Self::ConstIteratorGeom::Operator##Op, true);  \
    }                                                                         \
                                                                              \
  virtual IteratorGeom EndGeom##Op()                                        \
    {                                                                         \
    return IteratorGeom(this, Self::IteratorGeom::Operator##Op,             \
                        false);                                               \
    }                                                                         \
                                                                              \
  virtual ConstIteratorGeom EndGeom##Op() const                             \
    {                                                                         \
    return ConstIteratorGeom(this,                                            \
                             Self::ConstIteratorGeom::Operator##Op, false); \
    }

namespace itk
{
/**
 * \class QuadEdgeMeshBaseIterator
 *
 * \brief Base iterator class for QuadEdgeMesh
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TQuadEdge >
class QuadEdgeMeshBaseIterator
{
public:
  // Hierarchy typedefs & values.
  typedef QuadEdgeMeshBaseIterator Self;
  typedef TQuadEdge                QuadEdgeType;

  // Different types of iterators, one for each basic QE operation.
  enum {
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
  QuadEdgeMeshBaseIterator(QuadEdgeType *e,
                           int op = OperatorOnext,
                           bool start = true):
    m_StartEdge(e), m_Iterator(e),
    m_OpType(op), m_Start(start) {}

  virtual ~QuadEdgeMeshBaseIterator() {}

  Self & operator=(const Self & r)
  {
    if(this != &r)
      {
      m_StartEdge = r.m_StartEdge;
      m_Iterator = r.m_Iterator;
      m_OpType = r.m_OpType;
      m_Start = r.m_Start;
      }
    return ( *this );
  }

  QuadEdgeType * GetStartEdge() const { return ( m_StartEdge ); }
  QuadEdgeType * GetIterator() const { return ( m_Iterator ); }
  int           GetOpType() const { return ( m_OpType ); }
  bool          GetStart() const { return ( m_Start ); }

  /** Iteration methods. */
  bool operator==(Self & r)
  {
    return ( ( m_StartEdge == r.m_StartEdge )
             && ( m_Iterator  == r.m_Iterator )
             && ( m_OpType    == r.m_OpType )
             && ( m_Start     == r.m_Start ) );
  }

  bool operator==(const Self & r) const
  {
    return ( ( m_StartEdge == r.m_StartEdge )
             && ( m_Iterator  == r.m_Iterator )
             && ( m_OpType    == r.m_OpType )
             && ( m_Start     == r.m_Start ) );
  }

  bool operator!=(Self & r)
  {
    return ( !( this->operator==(r) ) );
  }

  bool operator!=(const Self & r) const
  {
    return ( !( this->operator==(r) ) );
  }

  Self & operator++()
  {
    if ( m_Start )
      {
      this->GoToNext();
      m_Start = !( m_Iterator == m_StartEdge );
      }

    return ( *this );
  }

  Self & operator++(int)
  {
    if ( m_Start )
      {
      this->GoToNext();
      m_Start = !( m_Iterator == m_StartEdge );
      }
    return ( *this );
  }

protected:
  /** Method that should do all the iteration work. */
  virtual void GoToNext()
  {
    switch ( m_OpType )
      {
      case OperatorOnext:
        m_Iterator = m_Iterator->GetOnext();
        break;
      case OperatorSym:
        m_Iterator = m_Iterator->GetSym();
        break;
      case OperatorLnext:
        m_Iterator = m_Iterator->GetLnext();
        break;
      case OperatorRnext:
        m_Iterator = m_Iterator->GetRnext();
        break;
      case OperatorDnext:
        m_Iterator = m_Iterator->GetDnext();
        break;
      case OperatorOprev:
        m_Iterator = m_Iterator->GetOprev();
        break;
      case OperatorLprev:
        m_Iterator = m_Iterator->GetLprev();
        break;
      case OperatorRprev:
        m_Iterator = m_Iterator->GetRprev();
        break;
      case OperatorDprev:
        m_Iterator = m_Iterator->GetDprev();
        break;
      case OperatorInvOnext:
        m_Iterator = m_Iterator->GetInvOnext();
        break;
      case OperatorInvLnext:
        m_Iterator = m_Iterator->GetInvLnext();
        break;
      case OperatorInvRnext:
        m_Iterator = m_Iterator->GetInvRnext();
        break;
      case OperatorInvDnext:
        m_Iterator = m_Iterator->GetInvDnext();
        break;
      default:
        break;
      }
  }

protected:
  QuadEdgeType *m_StartEdge;  /**< Start edge */
  QuadEdgeType *m_Iterator;   /**< Current iteration position */
  int           m_OpType;     /**< Operation type */
  bool          m_Start;      /**< Indicates iteration has just started */
};

/**
 * \class QuadEdgeMeshIterator
 *
 * \brief Non const iterator for QuadMesh
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TQuadEdge >
class QuadEdgeMeshIterator:
  public QuadEdgeMeshBaseIterator< TQuadEdge >
{
public:
  /** Hierarchy typedefs and values. */
  typedef QuadEdgeMeshIterator                  Self;
  typedef QuadEdgeMeshBaseIterator< TQuadEdge > Superclass;
  typedef TQuadEdge                             QuadEdgeType;

public:
  /** Object creation methods. */
  QuadEdgeMeshIterator(QuadEdgeType *e = (QuadEdgeType *)0,
                       int op = Superclass::OperatorOnext,
                       bool start = true):
    Superclass(e, op, start) {}

  virtual ~QuadEdgeMeshIterator() {}

  QuadEdgeType * Value() { return ( this->m_Iterator ); }
  const QuadEdgeType * Value() const { return ( this->m_Iterator ); }
};

/**
 * \class QuadEdgeMeshIteratorGeom
 *
 * \brief Non const geometrical iterator
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TGeometricalQuadEdge >
class QuadEdgeMeshIteratorGeom:
  public QuadEdgeMeshIterator< TGeometricalQuadEdge >
{
public:
  /** Hierarchy typedefs and values. */
  typedef QuadEdgeMeshIterator< TGeometricalQuadEdge > Superclass;
  typedef TGeometricalQuadEdge                         QuadEdgeType;

  /** Geometric value type. */
  typedef typename QuadEdgeType::OriginRefType OriginRefType;

public:
  QuadEdgeMeshIteratorGeom(QuadEdgeType *e = (QuadEdgeType *)0,
                           int op = Superclass::OperatorOnext,
                           bool start = true):
    Superclass(e, op, start) {}
  OriginRefType operator*() { return ( this->m_Iterator->GetOrigin() ); }
};

/**
 * \class QuadEdgeMeshConstIterator
 *
 * \brief Const iterator for QuadEdgeMesh
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TQuadEdge >
class QuadEdgeMeshConstIterator:
  public QuadEdgeMeshBaseIterator< TQuadEdge >
{
public:
  /** Hierarchy typedefs & values. */
  typedef QuadEdgeMeshConstIterator             Self;
  typedef QuadEdgeMeshBaseIterator< TQuadEdge > Superclass;
  typedef QuadEdgeMeshIterator< TQuadEdge >     NoConstType;
  typedef TQuadEdge                             QuadEdgeType;

public:
  /** Object creation methods. */
  QuadEdgeMeshConstIterator(const QuadEdgeType *e = (QuadEdgeType *)0,
                            int op = Superclass::OperatorOnext,
                            bool start = true):
    Superclass(const_cast< QuadEdgeType * >( e ), op, start) {}

  virtual ~QuadEdgeMeshConstIterator() {}

  Self & operator=(const NoConstType & r)
  {
    this->m_StartEdge = r.GetStartEdge();
    this->m_Iterator = r.GetIterator();
    this->m_OpType = r.GetOpType();
    this->m_Start = r.GetStart();
    return ( *this );
  }

  const QuadEdgeType * Value() const { return ( this->m_Iterator ); }
};

/**
 * \class QuadEdgeMeshConstIteratorGeom
 *
 * \brief Const geometrical iterator
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TGeometricalQuadEdge >
class QuadEdgeMeshConstIteratorGeom:
  public QuadEdgeMeshConstIterator< TGeometricalQuadEdge >
{
public:
  /** Hierarchy typedefs and values. */
  typedef QuadEdgeMeshConstIteratorGeom                     Self;
  typedef QuadEdgeMeshConstIterator< TGeometricalQuadEdge > Superclass;
  typedef QuadEdgeMeshIteratorGeom< TGeometricalQuadEdge >  NoConstType;
  typedef TGeometricalQuadEdge                              QuadEdgeType;

  /** Geometric value type. */
  typedef typename QuadEdgeType::OriginRefType OriginRefType;

public:
  QuadEdgeMeshConstIteratorGeom(const QuadEdgeType *e = (QuadEdgeType *)0,
                                int op = Superclass::OperatorOnext,
                                bool start = true):
    Superclass(e, op, start) {}

  virtual ~QuadEdgeMeshConstIteratorGeom() {}

  Self & operator=(const NoConstType & r)
  {
    this->m_StartEdge = r.GetStartEdge();
    this->m_Iterator = r.GetIterator();
    this->m_OpType = r.GetOpType();
    this->m_Start = r.GetStart();
    return ( *this );
  }

  const OriginRefType operator*() const
  {
    return ( this->m_Iterator->GetOrigin() );
  }
};
}

#endif
