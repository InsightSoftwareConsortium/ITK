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

#ifndef itkFastMarchingQuadEdgeMeshFilterBase_h
#define itkFastMarchingQuadEdgeMeshFilterBase_h

#include "itkFastMarchingBase.h"
#include "itkFastMarchingTraits.h"

namespace itk
{
/** \class FastMarchingQuadEdgeMeshFilterBase
  \brief Fast Marching Method on QuadEdgeMesh

  The speed function is specified by the input mesh. Data associated to each
  point is considered as the speed function. The speed function is set using
  the method SetInput().

  If the speed function is contant and of value one, fast marching results is
  an approximate geodesic function from the initial alive points.

  Implementation of this class is based on
  "Fast Marching Methods on Triangulated Domains", Kimmel, R., and Sethian, J.A.,
  Proc. Nat. Acad. Sci., 95, pp. 8341-8435, 1998.

  \ingroup ITKFastMarching
*/
template< typename TInput, typename TOutput >
class ITK_TEMPLATE_EXPORT FastMarchingQuadEdgeMeshFilterBase :
    public FastMarchingBase< TInput, TOutput >
{
public:
  typedef FastMarchingQuadEdgeMeshFilterBase     Self;
  typedef FastMarchingBase< TInput, TOutput >    Superclass;
  typedef SmartPointer< Self >                   Pointer;
  typedef SmartPointer< const Self >             ConstPointer;
  typedef typename Superclass::Traits            Traits;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingQuadEdgeMeshFilterBase, FastMarchingBase);

  typedef typename Superclass::InputDomainType     InputMeshType;
  typedef typename Superclass::InputDomainPointer  InputMeshPointer;
  typedef typename Superclass::InputPixelType      InputPixelType;
  typedef typename InputMeshType::PointType        InputPointType;
  typedef typename InputMeshType::PointIdentifier  InputPointIdentifierType;

  typedef typename Superclass::OutputDomainType     OutputMeshType;
  typedef typename Superclass::OutputDomainPointer  OutputMeshPointer;
  typedef typename Superclass::OutputPixelType      OutputPixelType;
  typedef typename OutputMeshType::PointType        OutputPointType;
  typedef typename OutputPointType::VectorType      OutputVectorType;
  typedef typename OutputVectorType::RealValueType  OutputVectorRealType;
  typedef typename OutputMeshType::QEType           OutputQEType;
  typedef typename OutputMeshType::PointIdentifier  OutputPointIdentifierType;
  typedef typename OutputMeshType::PointsContainer  OutputPointsContainer;
  typedef typename OutputPointsContainer::Pointer   OutputPointsContainerPointer;
  typedef typename OutputPointsContainer::Iterator  OutputPointsContainerIterator;
  typedef typename OutputMeshType::PointDataContainer
                                                    OutputPointDataContainer;
  typedef typename OutputPointDataContainer::Pointer
                                                    OutputPointDataContainerPointer;

  typedef typename OutputMeshType::CellsContainer   OutputCellsContainer;
  typedef typename OutputCellsContainer::Pointer    OutputCellsContainerPointer;
  typedef typename OutputCellsContainer::ConstIterator
                                                    OutputCellsContainerConstIterator;
  typedef typename OutputMeshType::CellType         OutputCellType;


  typedef typename Traits::NodeType                 NodeType;
  typedef typename Traits::NodePairType             NodePairType;
  typedef typename Traits::NodePairContainerType    NodePairContainerType;
  typedef typename Traits::NodePairContainerPointer NodePairContainerPointer;
  typedef typename Traits::NodePairContainerConstIterator
    NodePairContainerConstIterator;

//  typedef typename Traits::NodeContainerType        NodeContainerType;
//  typedef typename Traits::NodeContainerPointer     NodeContainerPointer;
//  typedef typename Traits::NodeContainerConstIterator
//    NodeContainerConstIterator;

  typedef typename Superclass::LabelType LabelType;

  typedef std::map< NodeType, LabelType >           NodeLabelMapType;
  typedef typename NodeLabelMapType::iterator       NodeLabelMapIterator;
  typedef typename NodeLabelMapType::const_iterator NodeLabelMapConstIterator;

protected:

  FastMarchingQuadEdgeMeshFilterBase();
  virtual ~FastMarchingQuadEdgeMeshFilterBase() ITK_OVERRIDE;

  NodeLabelMapType m_Label;

  IdentifierType GetTotalNumberOfNodes() const ITK_OVERRIDE;

  void SetOutputValue( OutputMeshType* oMesh,
                      const NodeType& iNode,
                      const OutputPixelType& iValue ) ITK_OVERRIDE;

  const OutputPixelType GetOutputValue( OutputMeshType* oMesh,
                                  const NodeType& iNode ) const ITK_OVERRIDE;

  unsigned char GetLabelValueForGivenNode( const NodeType& iNode ) const ITK_OVERRIDE;

  void SetLabelValueForGivenNode( const NodeType& iNode,
                                  const LabelType& iLabel ) ITK_OVERRIDE;

  void UpdateNeighbors( OutputMeshType* oMesh,
                        const NodeType& iNode ) ITK_OVERRIDE;

  void UpdateValue( OutputMeshType* oMesh,
                    const NodeType& iNode ) ITK_OVERRIDE;

  const OutputVectorRealType
  Solve( OutputMeshType* oMesh,
         const NodeType& iId, const OutputPointType& iCurrentPoint,
         const OutputVectorRealType& iF,
         const NodeType& iId1, const OutputPointType& iP1,
         const bool& iIsFar1, const OutputVectorRealType iVal1,
         const NodeType& iId2, const OutputPointType& iP2,
         const bool& iIsFar2, const OutputVectorRealType& iVal2 )
  const;


  const OutputVectorRealType
  ComputeUpdate(
    const OutputVectorRealType& iVal1, const OutputVectorRealType& iVal2,
    const OutputVectorRealType& iNorm1, const OutputVectorRealType& iSqNorm1,
    const OutputVectorRealType& iNorm2, const OutputVectorRealType& iSqNorm2,
    const OutputVectorRealType& iDot, const OutputVectorRealType& iF )
    const;

  bool UnfoldTriangle(
    OutputMeshType* oMesh,
    const OutputPointIdentifierType& iId, const OutputPointType& iP,
    const OutputPointIdentifierType& iId1, const OutputPointType& iP1,
    const OutputPointIdentifierType& iId2, const OutputPointType &iP2,
    OutputVectorRealType& oNorm, OutputVectorRealType& oSqNorm,
    OutputVectorRealType& oDot1, OutputVectorRealType& oDot2,
    OutputPointIdentifierType& oId ) const;

  bool CheckTopology( OutputMeshType* oMesh,
                      const NodeType& iNode ) ITK_OVERRIDE;

  void InitializeOutput( OutputMeshType* oMesh ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastMarchingQuadEdgeMeshFilterBase);

  const InputMeshType *m_InputMesh;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFastMarchingQuadEdgeMeshFilterBase.hxx"
#endif

#endif // itkFastMarchingQuadEdgeMeshFilterBase_h
