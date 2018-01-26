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
#ifndef itkQuadEdgeMesh_h
#define itkQuadEdgeMesh_h

#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include "vcl_cstdarg.h"
#endif
#include <cstdarg>
#include <queue>
#include <vector>
#include <list>

#include "itkMesh.h"

#include "itkQuadEdgeMeshTraits.h"
#include "itkQuadEdgeMeshLineCell.h"
#include "itkQuadEdgeMeshPolygonCell.h"

#include "itkQuadEdgeMeshFrontIterator.h"
#include "itkConceptChecking.h"


namespace itk
{
/**
 * \class QuadEdgeMesh
 *
 * \brief Mesh class for 2D manifolds embedded in ND space.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/306
 *
 * \ingroup ITKQuadEdgeMesh
 */
template< typename TPixel, unsigned int VDimension,
          typename TTraits = QuadEdgeMeshTraits< TPixel, VDimension, bool, bool > >
class ITK_TEMPLATE_EXPORT QuadEdgeMesh:public Mesh< TPixel, VDimension, TTraits >
{
public:
  /** Input template parameters. */
  typedef TTraits Traits;
  typedef TPixel  PixelType;

  /** Standard typedefs. */
  typedef QuadEdgeMesh                       Self;
  typedef Mesh< TPixel, VDimension, Traits > Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;

  /** Convenient constants obtained from MeshTraits. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      Traits::PointDimension);
  itkStaticConstMacro(MaxTopologicalDimension, unsigned int,
                      Traits::MaxTopologicalDimension);

  /** Types defined in superclass. */
  typedef typename Superclass::CellPixelType   CellPixelType;
  typedef typename Superclass::CoordRepType    CoordRepType;
  typedef typename Superclass::PointIdentifier PointIdentifier;
  typedef typename Superclass::PointHashType   PointHashType;
  typedef typename Superclass::PointType       PointType;
  typedef typename Superclass::CellTraits      CellTraits;

  typedef typename CellTraits::PointIdInternalIterator PointIdInternalIterator;
  typedef typename CellTraits::PointIdIterator         PointIdIterator;

  // Point section:
  typedef typename Superclass::PointsContainer        PointsContainer;
  typedef typename Superclass::PointsContainerPointer PointsContainerPointer;
  typedef CoordRepType                                CoordRepArrayType[
    itkGetStaticConstMacro(PointDimension)];

  // Point data section:
  typedef typename Superclass::PointDataContainer PointDataContainer;
  typedef typename Superclass::PointDataContainerPointer
  PointDataContainerPointer;
  typedef typename Superclass::PointDataContainerIterator
  PointDataContainerIterator;
  typedef typename Superclass::PointsContainerConstIterator
  PointsContainerConstIterator;
  typedef typename Superclass::PointsContainerIterator
  PointsContainerIterator;

  // Cell section:
  typedef typename Superclass::CellIdentifier        CellIdentifier;
  typedef typename Superclass::CellType              CellType;
  typedef typename Superclass::CellAutoPointer       CellAutoPointer;
  typedef typename Superclass::CellFeatureIdentifier CellFeatureIdentifier;
  typedef typename Superclass::CellFeatureCount      CellFeatureCount;
  typedef typename Superclass::CellMultiVisitorType  CellMultiVisitorType;
  typedef typename Superclass::CellsContainer        CellsContainer;
  typedef typename Superclass::CellsContainerPointer CellsContainerPointer;

  typedef typename Superclass::CellsContainerConstIterator
  CellsContainerConstIterator;
  typedef typename Superclass::CellsContainerIterator
  CellsContainerIterator;

  typedef typename Superclass::CellLinksContainer CellLinksContainer;
  typedef typename Superclass::CellLinksContainerPointer
  CellLinksContainerPointer;
  typedef typename Superclass::CellLinksContainerIterator
  CellLinksContainerIterator;

  // Cell data section:
  typedef typename Superclass::CellDataContainer CellDataContainer;
  typedef typename Superclass::CellDataContainerPointer
  CellDataContainerPointer;
  typedef typename Superclass::CellDataContainerIterator
  CellDataContainerIterator;

  // Point / Cell correspondance section:
  typedef typename Superclass::PointCellLinksContainer
  PointCellLinksContainer;
  typedef typename Superclass::PointCellLinksContainerIterator
  PointCellLinksContainerIterator;

  // BoundaryAssignMents section:
  typedef typename Superclass::BoundaryAssignmentsContainer
  BoundaryAssignmentsContainer;
  typedef typename Superclass::BoundaryAssignmentsContainerPointer
  BoundaryAssignmentsContainerPointer;
  typedef typename Superclass::BoundaryAssignmentsContainerVector
  BoundaryAssignmentsContainerVector;

  // Miscellaneous section:
  typedef typename Superclass::BoundingBoxPointer BoundingBoxPointer;
  typedef typename Superclass::BoundingBoxType    BoundingBoxType;
  typedef typename Superclass::RegionType         RegionType;
  typedef typename Superclass::InterpolationWeightType
  InterpolationWeightType;

  /** Specific types for a quad-edge structure. */
  typedef typename Traits::PrimalDataType PrimalDataType;
  typedef typename Traits::DualDataType   DualDataType;
  typedef typename Traits::QEPrimal       QEPrimal;
  typedef typename Traits::QEDual         QEDual;
  typedef typename Traits::QEPrimal       QEType;
  // See the TODO entry dated from 2005-05-28
  // struct QEType : public QEPrimal, public QEDual {}
  typedef typename Traits::VertexRefType VertexRefType;
  typedef typename Traits::FaceRefType   FaceRefType;
  typedef typename Traits::VectorType    VectorType;

  /** Possible specialized cell types. */
  typedef QuadEdgeMeshLineCell< CellType >    EdgeCellType;
  typedef QuadEdgeMeshPolygonCell< CellType > PolygonCellType;

  /** Free insertion indexes. */
  typedef std::queue< PointIdentifier > FreePointIndexesType;
  typedef std::queue< CellIdentifier >  FreeCellIndexesType;

  /** Auxiliary types. */
  typedef std::vector< PointIdentifier > PointIdList;
  typedef std::list< QEPrimal * >        EdgeListType;
  typedef EdgeListType *                 EdgeListPointerType;

  /** Reserved PointIdentifier designated to represent the absence of Point */
  static const PointIdentifier m_NoPoint;

  /** Reserved CellIdentifier designated to represent the absence of Face */
  static const CellIdentifier m_NoFace;

public:

  /** Basic Object interface. */
  itkNewMacro(Self);
  itkTypeMacro(QuadEdgeMesh, Mesh);

#if !defined( ITK_WRAPPING_PARSER )
  /** FrontIterator definitions */
  itkQEDefineFrontIteratorMethodsMacro(Self);
#endif

public:

  // Multithreading framework: not tested yet.
  bool RequestedRegionIsOutsideOfTheBufferedRegion() override
  {
    return ( false );
  }

  void Initialize() override;

  /** another way of deleting all the cells */
  virtual void Clear();

  CellsContainer * GetEdgeCells() { return m_EdgeCellsContainer; }
  const CellsContainer * GetEdgeCells() const { return m_EdgeCellsContainer; }
  void SetEdgeCells(CellsContainer *edgeCells)
  { m_EdgeCellsContainer = edgeCells; }
  void SetEdgeCell(CellIdentifier cellId, CellAutoPointer & cellPointer)
  { m_EdgeCellsContainer->InsertElement( cellId, cellPointer.ReleaseOwnership() ); }

  /** Overloaded to avoid a bug in Mesh that prevents proper inheritance
   * Refer to
   * http://public.kitware.com/pipermail/insight-users/2005-March/012459.html
   * and
   * http://public.kitware.com/pipermail/insight-users/2005-April/012613.html
   */
  void CopyInformation(const DataObject *data) override { (void)data; }
  void Graft(const DataObject *data) override;

  /** squeeze the point container to be able to write the file properly */
  void SqueezePointsIds();

  /** overloaded method for backward compatibility */
  void BuildCellLinks() {}

#if !defined( ITK_WRAPPING_PARSER )
  /** overloaded method for backward compatibility */
  void SetBoundaryAssignments(int dimension,
                              BoundaryAssignmentsContainer *container)
  {
    (void)dimension;
    (void)container;
  }

  /** overloaded method for backward compatibility */
  BoundaryAssignmentsContainerPointer GetBoundaryAssignments(int dimension)
  {
    (void)dimension;
    return ( (BoundaryAssignmentsContainerPointer)0 );
  }

  /** overloaded method for backward compatibility */
  const BoundaryAssignmentsContainerPointer GetBoundaryAssignments(
    int dimension) const
  {
    (void)dimension;
    return ( (BoundaryAssignmentsContainerPointer)nullptr );
  }

#endif

  /** overloaded method for backward compatibility */
  void SetBoundaryAssignment(int dimension, CellIdentifier cellId,
                             CellFeatureIdentifier featureId,
                             CellIdentifier boundaryId)
  {
    (void)dimension;
    (void)cellId;
    (void)featureId;
    (void)boundaryId;
  }

  /** overloaded method for backward compatibility */
  bool GetBoundaryAssignment(int dimension, CellIdentifier cellId,
                             CellFeatureIdentifier featureId,
                             CellIdentifier *boundaryId) const
  {
    (void)dimension;
    (void)cellId;
    (void)featureId;
    (void)boundaryId;
    return ( false ); // ALEX: is it the good way?
  }

  /** overloaded method for backward compatibility */
  bool RemoveBoundaryAssignment(int dimension, CellIdentifier cellId,
                                CellFeatureIdentifier featureId)
  {
    (void)dimension;
    (void)cellId;
    (void)featureId;
    return ( false ); // ALEX: is it the good way?
  }

  /** overloaded method for backward compatibility */
  bool GetCellBoundaryFeature(int dimension, CellIdentifier cellId,
                              CellFeatureIdentifier featureId,
                              CellAutoPointer & cellAP) const
  {
    (void)dimension;
    (void)cellId;
    (void)featureId;
    (void)cellAP;
    return ( false );
  }

  /** overloaded method for backward compatibility */
  CellIdentifier GetCellBoundaryFeatureNeighbors(int dimension,
                                                CellIdentifier cellId,
                                                CellFeatureIdentifier featureId,
                                                std::set< CellIdentifier > *cellSet)
  {
    (void)dimension;
    (void)cellId;
    (void)featureId;
    (void)cellSet;
    return NumericTraits<CellIdentifier>::ZeroValue();
  }

  /** NOTE ALEX: this method do not use CellFeature and thus could be recoded */
  CellIdentifier GetCellNeighbors(CellIdentifier itkNotUsed(cellId),
                                 std::set< CellIdentifier > * itkNotUsed(cellSet))
  {
    return NumericTraits<CellIdentifier>::ZeroValue();
  }

  /** overloaded method for backward compatibility */
  bool GetAssignedCellBoundaryIfOneExists(int dimension,
                                          CellIdentifier cellId,
                                          CellFeatureIdentifier featureId,
                                          CellAutoPointer & cellAP) const
  {
    (void)dimension;
    (void)cellId;
    (void)featureId;
    (void)cellAP;
    return ( false ); // ALEX: is it the good way?
  }

  /** overloaded method for backward compatibility */
  void SetCell(CellIdentifier cId, CellAutoPointer & cell);

  /** Methods to simplify point/edge insertion/search. */
  virtual PointIdentifier FindFirstUnusedPointIndex();

  virtual CellIdentifier  FindFirstUnusedCellIndex();

  virtual void PushOnContainer(EdgeCellType *newEdge);

  // Adding Point/Edge/Face methods
  virtual PointIdentifier AddPoint(const PointType & p);

  /** */
  virtual QEPrimal * AddEdge(const PointIdentifier & orgPid,
                             const PointIdentifier & destPid);

  virtual QEPrimal * AddEdgeWithSecurePointList(const PointIdentifier & orgPid,
                                                const PointIdentifier & destPid);

  /** Add a polygonal face to the Mesh, suppose QE layer ready */
  virtual void      AddFace(QEPrimal *e);

  /** Add a polygonal face to the Mesh. The list of points
   * is expected to be ordered counter-clock wise. The inside
   * of the new face will be on the left side of the edges
   * formed by consecutive points in this list. */
  virtual QEPrimal * AddFace(const PointIdList & points);

  virtual QEPrimal * AddFaceWithSecurePointList(const PointIdList & points);

  virtual QEPrimal * AddFaceWithSecurePointList(const PointIdList & points,
                                                bool CheckEdges);

  /** Adds a triangular face to the Mesh */
  virtual QEPrimal * AddFaceTriangle(const PointIdentifier & aPid,
                                     const PointIdentifier & bPid,
                                     const PointIdentifier & cPid);

  /** Deletion methods */
  virtual void DeletePoint(const PointIdentifier & pid);

  virtual void DeleteEdge(const PointIdentifier & orgPid,
                          const PointIdentifier & destPid);

  virtual void DeleteEdge(QEPrimal *e);

  virtual void LightWeightDeleteEdge(EdgeCellType *e);

  virtual void LightWeightDeleteEdge(QEPrimal *e);

  virtual void DeleteFace(FaceRefType faceToDelete);

  //
  bool GetPoint(PointIdentifier pid, PointType *pt) const
  {
    return ( Superclass::GetPoint(pid, pt) );
  }

  virtual PointType  GetPoint(const PointIdentifier & pid) const;

  virtual VectorType GetVector(const PointIdentifier & pid) const;

  virtual QEPrimal *  GetEdge() const;

  virtual QEPrimal *  GetEdge(const CellIdentifier & eid) const;

  virtual QEPrimal *  FindEdge(const PointIdentifier & pid0) const;

  virtual QEPrimal *  FindEdge(const PointIdentifier & pid0,
                               const PointIdentifier & pid1) const;

  virtual EdgeCellType *  FindEdgeCell(const PointIdentifier & pid0,
                                       const PointIdentifier & pid1) const;

  ///  Compute the euclidian length of argument edge
  CoordRepType ComputeEdgeLength(QEPrimal *e);

  PointIdentifier ComputeNumberOfPoints() const;

  CellIdentifier ComputeNumberOfFaces() const;

  CellIdentifier ComputeNumberOfEdges() const;

  PointIdentifier Splice(QEPrimal *a, QEPrimal *b);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  // End concept checking
#endif

  // for reusability of a mesh in the MeshToMesh filter
  void ClearFreePointAndCellIndexesLists()
  {
    while ( !this->m_FreePointIndexes.empty() )
      {
      this->m_FreePointIndexes.pop();
      }
    while ( !this->m_FreeCellIndexes.empty() )
      {
      this->m_FreeCellIndexes.pop();
      }
  }

  CellIdentifier GetNumberOfFaces() const { return ( m_NumberOfFaces ); }
  CellIdentifier GetNumberOfEdges() const { return ( m_NumberOfEdges ); }

protected:
  /** Constructor and Destructor. */
  QuadEdgeMesh();
  ~QuadEdgeMesh() override;

  /** Release the memory of each one of the cells independently. */
  virtual void ClearCellsContainer();

  CellsContainerPointer m_EdgeCellsContainer;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMesh);

  CellIdentifier m_NumberOfFaces;
  CellIdentifier m_NumberOfEdges;

protected:
  FreePointIndexesType m_FreePointIndexes;
  FreeCellIndexesType  m_FreeCellIndexes;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMesh.hxx"
#endif

#endif
