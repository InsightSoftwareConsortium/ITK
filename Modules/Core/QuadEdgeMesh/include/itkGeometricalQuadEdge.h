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
#ifndef itkGeometricalQuadEdge_h
#define itkGeometricalQuadEdge_h

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
 * the two types GeometricalQuadEdge and GeometricalQuadEdge::Dual
 * are always different (in the sense that their typeid() are different). If
 * we only had the four first parameters and assume that
 * GeometricalQuadEdge gets instantiated with types such that TVRef =
 * TFRef and TPrimalData = TDualData then this instantiation
 * GeometricalQuadEdge and GeometricalQuadEdge::Dual would be the
 * same types (this is simply due to the very definition of
 * GeometricalQuadEdge::Dual). This would in turn make the types QEType
 * and QEDual of \ref QuadEdgeMesh identical and would prevent any algorithm
 * requiring to distinguish those types (e.g. by relying on a
 * dynamic_cast<QEType*>) to be effective.  This justifies the existence of
 * last dummy template parameter and it's default value.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/306
 *
 * \sa QuadEdge
 *
 * \ingroup MeshObjects
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TVRef,  typename TFRef,
          typename TPrimalData, typename TDualData,
          bool PrimalDual = true >
class ITK_TEMPLATE_EXPORT GeometricalQuadEdge:public QuadEdge
{
public:
  /** Hierarchy typedefs. */
  typedef GeometricalQuadEdge Self;
  typedef QuadEdge            Superclass;
  typedef Self *              RawPointer;

  /**
   * Dual type, basically the same type with swapped template
   * parameters.
   *
   */
  typedef GeometricalQuadEdge< TFRef, TVRef,
                               TDualData, TPrimalData, !PrimalDual >         DualType;

  /** Input template parameters & values convenient renaming. */
  typedef TVRef       OriginRefType;
  typedef TFRef       DualOriginRefType;
  typedef TPrimalData PrimalDataType;
  typedef TDualData   DualDataType;
  // Line Cell Id in Mesh Cell Container
  // used to go up to LineCell level
  typedef TFRef LineCellIdentifier;

public:

  /** Iterator types. */
  typedef QuadEdgeMeshIteratorGeom< Self >      IteratorGeom;
  typedef QuadEdgeMeshConstIteratorGeom< Self > ConstIteratorGeom;

  /** Basic iterators methods. */
  inline itkQEDefineIteratorGeomMethodsMacro(Onext);
  inline itkQEDefineIteratorGeomMethodsMacro(Sym);
  inline itkQEDefineIteratorGeomMethodsMacro(Lnext);
  inline itkQEDefineIteratorGeomMethodsMacro(Rnext);
  inline itkQEDefineIteratorGeomMethodsMacro(Dnext);
  inline itkQEDefineIteratorGeomMethodsMacro(Oprev);
  inline itkQEDefineIteratorGeomMethodsMacro(Lprev);
  inline itkQEDefineIteratorGeomMethodsMacro(Rprev);
  inline itkQEDefineIteratorGeomMethodsMacro(Dprev);
  inline itkQEDefineIteratorGeomMethodsMacro(InvOnext);
  inline itkQEDefineIteratorGeomMethodsMacro(InvLnext);
  inline itkQEDefineIteratorGeomMethodsMacro(InvRnext);
  inline itkQEDefineIteratorGeomMethodsMacro(InvDnext);

  /** QE macros. */
  itkQEAccessorsMacro(Superclass, Self, DualType);

public:
  /** Memory creation methods. */
  GeometricalQuadEdge();
  virtual ~GeometricalQuadEdge() {}

  /** Set methods. */
  inline void SetOrigin(const OriginRefType v)
  { m_Origin = v; }

  inline void SetDestination(const OriginRefType v)
  { this->GetSym()->SetOrigin(v); }

  inline void SetRight(const DualOriginRefType v)
  { this->GetRot()->SetOrigin(v); }

  inline void SetLeft(const DualOriginRefType v)
  { this->GetInvRot()->SetOrigin(v); }

  /**
   * Set the Left() of all the edges in the Lnext() ring of "this"
   * with the same given geometrical information.
   * @param  faceGeom Looks at most maxSize edges in the Lnext() ring.
   * @param  maxSize Sets at most maxSize edges in the Lnext() ring.
   * @return Returns true on success. False otherwise.
   */
  bool SetLnextRingWithSameLeftFace(const DualOriginRefType faceGeom,
                                    int maxSize = 100);

  inline void UnsetOrigin()   { m_Origin = m_NoPoint; }
  inline void UnsetDestination()  { this->GetSym()->UnsetOrigin(); }
  inline void UnsetRight() { this->GetRot()->UnsetOrigin(); }
  inline void UnsetLeft()  { this->GetInvRot()->UnsetOrigin(); }

  /** Get methods. */
  //ORIENTATION_NOTE: this definition of GetLeft (or GetRight)
  // implicitely assumes that the Onext order is counter-clockwise !
  inline const OriginRefType GetOrigin() const { return ( m_Origin ); }
  inline const OriginRefType GetDestination()  const { return ( GetSym()->GetOrigin() ); }
  inline const DualOriginRefType GetRight() const { return ( GetRot()->GetOrigin() ); }
  inline const DualOriginRefType GetLeft() const { return ( GetInvRot()->GetOrigin() ); }

  /** Boolean accessors. */
  bool IsOriginSet() const;

  bool IsDestinationSet() const;

  bool IsRightSet() const;

  bool IsLeftSet() const;

  /** Extra data set methods. */
  inline void SetPrimalData(const PrimalDataType data)
  { m_Data = data; this->SetPrimalData(); }
  inline void SetDualData(const DualDataType data)
  { this->GetRot()->SetPrimalData(data); }

  inline void SetPrimalData() { m_DataSet = true; }
  inline void SetDualData()   { this->GetRot()->SetPrimalData(); }

  inline void UnsetPrimalData() { m_Data = false; }
  inline void UnsetDualData()   { this->GetRot()->UnsetPrimalData(); }

  /** Extra data get methods. */
  inline PrimalDataType GetPrimalData() { return ( m_Data ); }
  inline DualDataType   GetDualData()
  { return ( this->GetRot()->GetPrimalData() ); }

  /** Boolean accessors. */
  inline bool IsPrimalDataSet() { return ( m_DataSet ); }
  inline bool IsDualDataSet()
  { return ( this->GetRot()->IsPrimalDataSet() ); }

  /**
   * @return Returns true when "this" has no faces set on both sides.
   *         Return false otherwise.
   */
  inline bool IsWire()
  { return ( !( this->IsLeftSet() ) && !( this->IsRightSet() ) ); }

  /**
   * @return Returns true when "this" is on the boundary i.e.
   *         one and only one of the faces is set. Return false
   *         otherwise.
   */
  inline bool IsAtBorder()
  {
    return ( ( this->IsLeftSet() && !this->IsRightSet() )
             || ( !this->IsLeftSet() && this->IsRightSet() ) );
  }

  /**
   * @return Returns true when "this" has faces set on both sides.
   *         Return false otherwise.
   */
  inline bool IsInternal() const
  { return ( this->IsLeftSet() && this->IsRightSet() ); }

  bool IsOriginInternal() const;

  bool IsLnextSharingSameFace(int maxSize = 100);

  bool IsLnextOfTriangle();

  bool IsInOnextRing(Self *);

  bool IsInLnextRing(Self *);

  Self * GetNextBorderEdgeWithUnsetLeft(Self *edgeTest = 0);

  bool InsertAfterNextBorderEdgeWithUnsetLeft(Self *isol,
                                              Self *hint = 0);

  bool ReorderOnextRingBeforeAddFace(Self *second);

  /** Disconnection methods. */
  inline bool IsOriginDisconnected()
  { return ( this == this->GetOnext() ); }
  inline bool IsDestinationDisconnected()
  { return ( this->GetSym()->IsOriginDisconnected() ); }
  inline bool IsDisconnected()
  {
    return ( this->IsOriginDisconnected()
             && this->IsDestinationDisconnected() );
  }

  void Disconnect();

  inline void SetIdent(const LineCellIdentifier & User_Value) { this->m_LineCellIdent = User_Value; }
  inline LineCellIdentifier GetIdent() { return ( this->m_LineCellIdent ); }

public:
  // Reserved OriginRefType designated to represent the absence of Origin
  static const OriginRefType m_NoPoint;

protected:
  OriginRefType      m_Origin;    // Geometrical information
  PrimalDataType     m_Data;      // User data associated to this edge.
  bool               m_DataSet;   // Indicates if the data is set.
  LineCellIdentifier m_LineCellIdent;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGeometricalQuadEdge.hxx"
#endif

#endif
