/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkFastMarchingQuadEdgeMeshFilterBase_h
#define itkFastMarchingQuadEdgeMeshFilterBase_h

#include "itkFastMarchingBase.h"
#include "itkFastMarchingTraits.h"

namespace itk
{
/**
 *\class FastMarchingQuadEdgeMeshFilterBase
  \brief Fast Marching Method on QuadEdgeMesh

  The speed function is specified by the input mesh. Data associated to each
  point is considered as the speed function. The speed function is set using
  the method SetInput().

  If the speed function is constant and of value one, fast marching results is
  an approximate geodesic function from the initial alive points.

  Implementation of this class is based on
  "Fast Marching Methods on Triangulated Domains", Kimmel, R., and Sethian, J.A.,
  Proc. Nat. Acad. Sci., 95, pp. 8341-8435, 1998.

  \ingroup ITKFastMarching
*/
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT FastMarchingQuadEdgeMeshFilterBase : public FastMarchingBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastMarchingQuadEdgeMeshFilterBase);

  using Self = FastMarchingQuadEdgeMeshFilterBase;
  using Superclass = FastMarchingBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Traits = typename Superclass::Traits;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingQuadEdgeMeshFilterBase, FastMarchingBase);

  using InputMeshType = typename Superclass::InputDomainType;
  using InputMeshPointer = typename Superclass::InputDomainPointer;
  using InputPixelType = typename Superclass::InputPixelType;
  using InputPointType = typename InputMeshType::PointType;
  using InputPointIdentifierType = typename InputMeshType::PointIdentifier;

  using OutputMeshType = typename Superclass::OutputDomainType;
  using OutputMeshPointer = typename Superclass::OutputDomainPointer;
  using OutputPixelType = typename Superclass::OutputPixelType;
  using OutputPointType = typename OutputMeshType::PointType;
  using OutputVectorType = typename OutputPointType::VectorType;
  using OutputVectorRealType = typename OutputVectorType::RealValueType;
  using OutputQEType = typename OutputMeshType::QEType;
  using OutputPointIdentifierType = typename OutputMeshType::PointIdentifier;
  using OutputPointsContainer = typename OutputMeshType::PointsContainer;
  using OutputPointsContainerPointer = typename OutputPointsContainer::Pointer;
  using OutputPointsContainerIterator = typename OutputPointsContainer::Iterator;
  using OutputPointDataContainer = typename OutputMeshType::PointDataContainer;
  using OutputPointDataContainerPointer = typename OutputPointDataContainer::Pointer;

  using OutputCellsContainer = typename OutputMeshType::CellsContainer;
  using OutputCellsContainerPointer = typename OutputCellsContainer::Pointer;
  using OutputCellsContainerConstIterator = typename OutputCellsContainer::ConstIterator;
  using OutputCellType = typename OutputMeshType::CellType;


  using NodeType = typename Traits::NodeType;
  using NodePairType = typename Traits::NodePairType;
  using NodePairContainerType = typename Traits::NodePairContainerType;
  using NodePairContainerPointer = typename Traits::NodePairContainerPointer;
  using NodePairContainerConstIterator = typename Traits::NodePairContainerConstIterator;

  //  using NodeContainerType = typename Traits::NodeContainerType;
  //  using NodeContainerPointer = typename Traits::NodeContainerPointer;
  //  using NodeContainerConstIterator = typename Traits::NodeContainerConstIterator;

  using LabelType = typename Superclass::LabelType;

  using NodeLabelMapType = std::map<NodeType, LabelType>;
  using NodeLabelMapIterator = typename NodeLabelMapType::iterator;
  using NodeLabelMapConstIterator = typename NodeLabelMapType::const_iterator;

protected:
  FastMarchingQuadEdgeMeshFilterBase();
  ~FastMarchingQuadEdgeMeshFilterBase() override = default;

  NodeLabelMapType m_Label;

  IdentifierType
  GetTotalNumberOfNodes() const override;

  void
  SetOutputValue(OutputMeshType * oMesh, const NodeType & iNode, const OutputPixelType & iValue) override;

  const OutputPixelType
  GetOutputValue(OutputMeshType * oMesh, const NodeType & iNode) const override;

  unsigned char
  GetLabelValueForGivenNode(const NodeType & iNode) const override;

  void
  SetLabelValueForGivenNode(const NodeType & iNode, const LabelType & iLabel) override;

  void
  UpdateNeighbors(OutputMeshType * oMesh, const NodeType & iNode) override;

  void
  UpdateValue(OutputMeshType * oMesh, const NodeType & iNode) override;

  const OutputVectorRealType
  Solve(OutputMeshType *             oMesh,
        const NodeType &             iId,
        const OutputPointType &      iCurrentPoint,
        const OutputVectorRealType & iF,
        const NodeType &             iId1,
        const OutputPointType &      iP1,
        const bool &                 iIsFar1,
        const OutputVectorRealType   iVal1,
        const NodeType &             iId2,
        const OutputPointType &      iP2,
        const bool &                 iIsFar2,
        const OutputVectorRealType & iVal2) const;


  const OutputVectorRealType
  ComputeUpdate(const OutputVectorRealType & iVal1,
                const OutputVectorRealType & iVal2,
                const OutputVectorRealType & iNorm1,
                const OutputVectorRealType & iSqNorm1,
                const OutputVectorRealType & iNorm2,
                const OutputVectorRealType & iSqNorm2,
                const OutputVectorRealType & iDot,
                const OutputVectorRealType & iF) const;

  bool
  UnfoldTriangle(OutputMeshType *                  oMesh,
                 const OutputPointIdentifierType & iId,
                 const OutputPointType &           iP,
                 const OutputPointIdentifierType & iId1,
                 const OutputPointType &           iP1,
                 const OutputPointIdentifierType & iId2,
                 const OutputPointType &           iP2,
                 OutputVectorRealType &            oNorm,
                 OutputVectorRealType &            oSqNorm,
                 OutputVectorRealType &            oDot1,
                 OutputVectorRealType &            oDot2,
                 OutputPointIdentifierType &       oId) const;

  bool
  CheckTopology(OutputMeshType * oMesh, const NodeType & iNode) override;

  void
  InitializeOutput(OutputMeshType * oMesh) override;

private:
  const InputMeshType * m_InputMesh;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastMarchingQuadEdgeMeshFilterBase.hxx"
#endif

#endif // itkFastMarchingQuadEdgeMeshFilterBase_h
