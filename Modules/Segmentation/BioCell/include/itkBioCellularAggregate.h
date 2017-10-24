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
#ifndef itkBioCellularAggregate_h
#define itkBioCellularAggregate_h

#include "itkDefaultDynamicMeshTraits.h"
#include "itkMesh.h"
#include "itkImage.h"
#include "itkBioCell.h"
#include "itkPolygonCell.h"

#include <iostream>
#include <vector>

namespace itk
{
namespace bio
{
/** \class CellularAggregate
 * \brief Base class for different types of cellular groups,
 * including bacterial colonies and pluricellular organisms
 *
 * This class represents an aggregation of bio::Cell objects.
 *
 * \ingroup ITKBioCell
 */
template< unsigned int NSpaceDimension = 3 >
class ITK_TEMPLATE_EXPORT CellularAggregate:public CellularAggregateBase
{
public:
  /** Standard class typedefs. */
  typedef CellularAggregate          Self;
  typedef CellularAggregateBase      Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /*** Run-time type information (and related methods). */
  itkTypeMacro(BioCellularAggregate, CellularAggregateBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  itkStaticConstMacro(SpaceDimension, unsigned int, NSpaceDimension);

  /*** Type to be used for data associated with each point in the mesh. */
  typedef    Cell< NSpaceDimension > BioCellType;
  typedef    BioCellType *           PointPixelType;
  typedef    double                  CellPixelType;

  /** Mesh Traits */
  typedef DefaultDynamicMeshTraits<
    PointPixelType,                     // PixelType
    NSpaceDimension,                    // Points Dimension
    NSpaceDimension,                    // Max.Topological Dimension
    double,                             // Type for coordinates
    double,                             // Type for interpolation
    CellPixelType                       // Type for values in the cells
    >  MeshTraits;

  /** Mesh Traits */
  typedef Mesh<  PointPixelType,
                 NSpaceDimension,
                 MeshTraits  >               MeshType;

  /** Mesh Associated types */
  typedef typename MeshType::Pointer       MeshPointer;
  typedef typename MeshType::ConstPointer  MeshConstPointer;
  typedef typename MeshType::PointType     PointType;
  typedef typename BioCellType::VectorType VectorType;

  typedef typename MeshType::PointsContainer              PointsContainer;
  typedef typename MeshType::PointDataContainer           PointDataContainer;
  typedef typename MeshType::CellsContainer               VoronoiRegionsContainer;
  typedef typename PointsContainer::Iterator              PointsIterator;
  typedef typename PointDataContainer::Iterator           CellsIterator;
  typedef typename VoronoiRegionsContainer::Iterator      VoronoiIterator;
  typedef typename PointsContainer::ConstIterator         PointsConstIterator;
  typedef typename PointDataContainer::ConstIterator      CellsConstIterator;
  typedef typename VoronoiRegionsContainer::ConstIterator VoronoiConstIterator;
  typedef typename MeshType::CellAutoPointer              CellAutoPointer;

  /**   Voronoi region around a bio::Cell */
  typedef CellInterface<
    typename MeshType::CellPixelType,
    typename MeshType::CellTraits >      CellInterfaceType;

  typedef PolygonCell<  CellInterfaceType >           VoronoiRegionType;
  typedef typename VoronoiRegionType::SelfAutoPointer VoronoiRegionAutoPointer;

  /** Convenient typedefs. */
  typedef float                                    ImagePixelType;
  typedef Image< ImagePixelType, NSpaceDimension > SubstrateType;
  typedef typename SubstrateType::Pointer          SubstratePointer;
  typedef ImagePixelType                           SubstrateValueType;
  typedef std::vector< SubstratePointer >          SubstratesVector;

public:
  unsigned int GetNumberOfCells() const;

  static unsigned int GetDimension() { return SpaceDimension; }

  void SetGrowthRadiusLimit(double value);

  void SetGrowthRadiusIncrement(double value);

  itkGetModifiableObjectMacro(Mesh, MeshType);

  virtual void AdvanceTimeStep();

  virtual void SetEgg(BioCellType *cell, const PointType & position);

  virtual void Add(CellBase *cell);

  virtual void Add(CellBase *cell, const VectorType & perturbation);

  virtual void Add(CellBase *cellA, CellBase *cellB, double perturbationLength) ITK_OVERRIDE;

  virtual void Remove(CellBase *cell) ITK_OVERRIDE;

  virtual void GetVoronoi(IdentifierType cellId, VoronoiRegionAutoPointer &) const;

  void DumpContent(std::ostream & os) const;

  virtual void AddSubstrate(SubstrateType *substrate);

  virtual SubstratesVector & GetSubstrates();

  virtual SubstrateValueType GetSubstrateValue(IdentifierType cellId,
                                               unsigned int substrateId) const ITK_OVERRIDE;

  virtual void KillAll();

protected:
  CellularAggregate();
  virtual ~CellularAggregate() ITK_OVERRIDE;
  CellularAggregate(const Self &);
  void operator=(const Self &);

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void ComputeForces();

  virtual void UpdatePositions();

  virtual void ComputeClosestPoints();

  virtual void ClearForces();

private:

  MeshPointer      m_Mesh;
  SubstratesVector m_Substrates;
  double           m_FrictionForce;
  SizeValueType    m_Iteration;
  SizeValueType    m_ClosestPointComputationInterval;
};
} // end namespace bio
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBioCellularAggregate.hxx"
#endif

#endif
