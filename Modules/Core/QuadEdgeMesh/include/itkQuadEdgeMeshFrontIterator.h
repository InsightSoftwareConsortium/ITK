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
#ifndef itkQuadEdgeMeshFrontIterator_h
#define itkQuadEdgeMeshFrontIterator_h

#include "itkMapContainer.h"

// -------------------------------------------------------------------------
#define itkQEDefineFrontIteratorMethodsMacro(MeshTypeArg)                       \
  /* Dual definition placed before others because of .NET that cannot */        \
  /* cope with definition of FrontIterator (that further hides the    */        \
  /* definition of the template).                                      */        \
  typedef typename MeshTypeArg::QEDual   QEDualType;                            \
  typedef typename MeshTypeArg::QEPrimal QEPrimalType;                          \
  typedef QuadEdgeMeshFrontIterator< MeshTypeArg,                               \
                                     QEDualType > FrontDualIterator;            \
  typedef QuadEdgeMeshConstFrontIterator< MeshTypeArg,                          \
                                          QEDualType >                          \
  ConstFrontDualIterator;                                                       \
  typedef QuadEdgeMeshFrontIterator< MeshTypeArg,                               \
                                     QEPrimalType >   FrontIterator;            \
  typedef QuadEdgeMeshConstFrontIterator< MeshTypeArg,                          \
                                          QEPrimalType >                        \
  ConstFrontIterator;                                                           \
                                                                                \
  virtual FrontIterator BeginFront(QEPrimalType * seed = (QEPrimalType *)0)     \
    {                                                                           \
    return ( FrontIterator(this, true, seed) );                                 \
    }                                                                           \
                                                                                \
  virtual ConstFrontIterator BeginFront(QEPrimalType * seed) const              \
        { return ( ConstFrontIterator(this, true, seed) ); }                    \
                                                                                \
  virtual FrontIterator EndFront()                                              \
    {                                                                           \
    return ( FrontIterator(this, false) );                                      \
    }                                                                           \
                                                                                \
  virtual ConstFrontIterator EndFront() const                                   \
        { return ( ConstFrontIterator(this, false) ); }                         \
                                                                                \
  virtual FrontDualIterator BeginDualFront(QEDualType * seed = (QEDualType *)0) \
    {                                                                           \
    return ( FrontDualIterator(this, true, seed) );                             \
    }                                                                           \
                                                                                \
  virtual ConstFrontDualIterator BeginDualFront(QEDualType * seed) const        \
        { return ( ConstFrontDualIterator(this, true, seed) ); }                \
                                                                                \
  virtual FrontDualIterator EndDualFront()                                      \
    {                                                                           \
    return ( FrontDualIterator(this, false) );                                  \
    }                                                                           \
                                                                                \
  virtual ConstFrontDualIterator EndDualFront() const                           \
        { return ( ConstFrontDualIterator(this, false) ); }

namespace itk
{
/**
 * \class QuadEdgeMeshFrontBaseIterator
 *
 * \brief Front iterator on Mesh class
 *
 * Like topological and geometrical operators, it iterates on edges.
 * Unlike them, this iterator is not local, nor cyclic. Starting from a
 * given seed, it will create a front that propagates on the surface.
 * Depending on the weight associated which each edge, and on the type of the
 * seed (primal or dual) it can be used for frint propagation algorithm,
 * distance tree computation or other Djikstra like algorithms.
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TQE >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshFrontBaseIterator
{
public:
  // Hierarchy typedefs & values.
  typedef QuadEdgeMeshFrontBaseIterator Self;

  // Template types
  typedef TMesh MeshType;
  typedef TQE   QEType;

protected:
  // Mesh types
  typedef typename MeshType::CoordRepType CoordRepType;
  // QE types
  typedef typename QEType::OriginRefType QEOriginType;

  /**
   * \class FrontAtom
   *
   * \brief Atomic information associated to each edge of the front.
   *
   * Note that when sorting this list, the sorting criteria is the
   * Cost attribute.
   * \ingroup ITKQuadEdgeMesh
   */
  class FrontAtom
  {
public:
    FrontAtom(QEType *e = (QEType *)0, const CoordRepType c = 0):
      m_Edge(e), m_Cost(c)
    {}
    virtual ~FrontAtom() {}
    FrontAtom & operator=(const FrontAtom & r)
    { m_Edge = r.m_Edge; m_Cost = r.m_Cost; return *this; }
    bool operator==(const FrontAtom & r) const
    { return ( m_Edge == r.m_Edge ); }
    bool operator!=(const FrontAtom & r) const
    { return ( m_Edge != r.m_Edge ); }
    bool operator<(const FrontAtom & r) const
    { return ( m_Cost < r.m_Cost ); }

public:
    QEType *     m_Edge;
    CoordRepType m_Cost;
  };

  /** The active front is simply a list of edges that can be sorted on
   *  the sort attribute FrontAtom
   */
  typedef std::list< FrontAtom >       FrontType;
  typedef typename FrontType::iterator FrontTypeIterator;
  typedef FrontType *                  FrontTypePointer;

  /** Whether an Origin (i.e. a vertex or a face since we either deal with
   *  primal or dual edges) was already visited.
   */
  typedef MapContainer< QEOriginType, bool >       IsVisitedContainerType;
  typedef typename IsVisitedContainerType::Pointer IsVisitedPointerType;

public:
  /** Object creation methods. */
  QuadEdgeMeshFrontBaseIterator(MeshType *mesh  = (MeshType *)0,
                                bool start = true,
                                QEType *seed  = (QEType *)0);
  virtual ~QuadEdgeMeshFrontBaseIterator();

  Self & operator=(const Self & r)
  {
    if(this != &r)
      {
      m_Mesh  = r.m_Mesh;
      m_Start = r.m_Start;
      m_Seed  = r.m_Seed;
      m_Front = r.m_Front;
      m_IsPointVisited = r.m_IsPointVisited;
      m_CurrentEdge = r.m_CurrentEdge;
      }
    return ( *this );
  }

  // Iteration methods.
  bool operator==(Self & r)
  {
    return ( m_Start == r.m_Start );
  }

  bool operator==(const Self & r) const
  {
    return ( m_Start == r.m_Start );
  }

  bool operator!=(Self & r)
  {
    return ( !( this->operator==(r) ) );
  }

  bool operator!=(const Self & r) const
  {
    return ( !( this->operator==(r) ) );
  }

  Self & operator++();

  Self & operator++(int) { return ( this->operator++() ); }

  MeshType * GetMesh() const { return this->m_Mesh; }

protected:
  /** Find a default seed by taking any edge (with proper type) in
   *  the current mesh.
   */
  QEType * FindDefaultSeed();

  /** The default cost associated to an edge is simply 1. This corresponds
   *  to the "topological metric" i.e. all edges have unit length.
   */
  virtual CoordRepType GetCost(QEType *edge){ (void)edge; return ( 1 ); }

protected:
  /** Mesh on which we propagate the front */
  MeshType *m_Mesh;
  /** Initial seed of the front */
  QEType *m_Seed;
  /** Whether the iterator is active */
  bool m_Start;
  /** The active front */
  FrontTypePointer m_Front;
  /** The already visited points */
  IsVisitedPointerType m_IsPointVisited;
  /** The current edge at this stage of iteration */
  QEType *m_CurrentEdge;
};

/**
 * \class QuadEdgeMeshFrontIterator
 *
 * \brief Non const quad edge front iterator.
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TQE >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshFrontIterator:
  public QuadEdgeMeshFrontBaseIterator< TMesh, TQE  >
{
public:
  /** Hierarchy typedefs and values. */
  typedef QuadEdgeMeshFrontIterator                   Self;
  typedef QuadEdgeMeshFrontBaseIterator< TMesh, TQE > Superclass;
  typedef typename Superclass::MeshType               MeshType;
  typedef typename Superclass::QEType                 QEType;

public:
  /** Object creation methods. */
  QuadEdgeMeshFrontIterator(MeshType *mesh = (MeshType *)0,
                            bool start = true,
                            QEType *seed  = (QEType *)0):
    Superclass(mesh, start, seed) {}
  virtual ~QuadEdgeMeshFrontIterator() {}
  QEType * Value() { return ( this->m_CurrentEdge ); }
};

/**
 * \class QuadEdgeMeshConstFrontIterator
 *
 * \brief Const quad edge mesh front iterator.
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TMesh, typename TQE = typename TMesh::QEType >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshConstFrontIterator:
  public QuadEdgeMeshFrontBaseIterator< TMesh, TQE >
{
public:
  /** Hierarchy typedefs & values. */
  typedef QuadEdgeMeshConstFrontIterator                Self;
  typedef QuadEdgeMeshFrontBaseIterator< TMesh, TQE >   Superclass;
  typedef typename Superclass::QEType                   QEType;
  typedef typename Superclass::MeshType                 MeshType;
  typedef QuadEdgeMeshFrontIterator< MeshType, QEType > NoConstType;

public:
  /** Object creation methods. */
  QuadEdgeMeshConstFrontIterator(const MeshType *mesh = (MeshType *)0,
                                 bool start = true,
                                 QEType *seed  = (QEType *)0)
  {
    (void)mesh;
    (void)start;
    (void)seed;
  }

  /** \todo do we need here a    : Superclass( mesh, start, seed ) { } */
  virtual ~QuadEdgeMeshConstFrontIterator() {}
  Self & operator=(const NoConstType & r)
  {
    this->m_Mesh  = r.GetMesh();
    return ( *this );
  }

  const QEType * Value() const { return ( this->m_CurrentEdge ); }
};
}

#include "itkQuadEdgeMeshFrontIterator.hxx"

#endif
