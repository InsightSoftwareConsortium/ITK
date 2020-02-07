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

#ifndef itkUpdateWhitakerSparseLevelSet_h
#define itkUpdateWhitakerSparseLevelSet_h

#include "itkImage.h"
#include "itkDiscreteLevelSetImage.h"
#include "itkWhitakerSparseLevelSetImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkLabelImageToLabelMapFilter.h"

namespace itk
{
/**
 *  \class UpdateWhitakerSparseLevelSet
 *  \brief Base class for updating the level-set function
 *
 *  \tparam VDimension Dimension of the input space
 *  \tparam TLevelSetValueType Output type (float or double) of the levelset function
 *  \tparam TEquationContainer Container of the system of levelset equations
 *  \ingroup ITKLevelSetsv4
 */
template <unsigned int VDimension, typename TLevelSetValueType, typename TEquationContainer>
class ITK_TEMPLATE_EXPORT UpdateWhitakerSparseLevelSet : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(UpdateWhitakerSparseLevelSet);

  using Self = UpdateWhitakerSparseLevelSet;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = Object;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(UpdateWhitakerSparseLevelSet, Object);

  static constexpr unsigned int ImageDimension = VDimension;

  using LevelSetOutputType = TLevelSetValueType;

  using LevelSetType = WhitakerSparseLevelSetImage<LevelSetOutputType, ImageDimension>;
  using LevelSetPointer = typename LevelSetType::Pointer;
  using LevelSetInputType = typename LevelSetType::InputType;
  using LevelSetOffsetType = typename LevelSetType::OffsetType;

  using LevelSetLabelMapType = typename LevelSetType::LabelMapType;
  using LevelSetLabelMapPointer = typename LevelSetType::LabelMapPointer;

  using LevelSetLabelObjectType = typename LevelSetType::LabelObjectType;
  using LevelSetLabelObjectPointer = typename LevelSetType::LabelObjectPointer;
  using LevelSetLabelObjectLengthType = typename LevelSetType::LabelObjectLengthType;
  using LevelSetLabelObjectLineType = typename LevelSetType::LabelObjectLineType;

  using LevelSetLayerIdType = typename LevelSetType::LayerIdType;
  using LevelSetLayerType = typename LevelSetType::LayerType;
  using LevelSetLayerIterator = typename LevelSetType::LayerIterator;
  using LevelSetLayerConstIterator = typename LevelSetType::LayerConstIterator;
  using LevelSetOutputRealType = typename LevelSetType::OutputRealType;

  using LevelSetLayerMapType = typename LevelSetType::LayerMapType;
  using LevelSetLayerMapIterator = typename LevelSetType::LayerMapIterator;
  using LevelSetLayerMapConstIterator = typename LevelSetType::LayerMapConstIterator;

  using EquationContainerType = TEquationContainer;
  using EquationContainerPointer = typename EquationContainerType::Pointer;

  using TermContainerType = typename EquationContainerType::TermContainerType;
  using TermContainerPointer = typename EquationContainerType::TermContainerPointer;

  using LabelImageType = Image<LevelSetLayerIdType, ImageDimension>;
  using LabelImagePointer = typename LabelImageType::Pointer;

  using LabelMapToLabelImageFilterType = LabelMapToLabelImageFilter<LevelSetLabelMapType, LabelImageType>;
  using LabelImageToLabelMapFilterType = LabelImageToLabelMapFilter<LabelImageType, LevelSetLabelMapType>;

  itkGetModifiableObjectMacro(OutputLevelSet, LevelSetType);

  /** Update function for initializing and computing the output level set */
  void
  Update();

  /** Set/Get the sparse levet set image */
  itkSetObjectMacro(InputLevelSet, LevelSetType);
  itkGetModifiableObjectMacro(InputLevelSet, LevelSetType);

  /** Set/Get the TimeStep for the update */
  itkSetMacro(TimeStep, LevelSetOutputType);
  itkGetMacro(TimeStep, LevelSetOutputType);

  /** Set/Get the RMS change for the update */
  itkGetMacro(RMSChangeAccumulator, LevelSetOutputType);

  /** Set/Get the Equation container for computing the update */
  itkSetObjectMacro(EquationContainer, EquationContainerType);
  itkGetModifiableObjectMacro(EquationContainer, EquationContainerType);

  /** Set/Get the current level set id */
  itkSetMacro(CurrentLevelSetId, IdentifierType);
  itkGetMacro(CurrentLevelSetId, IdentifierType);

  /** Set the update map for all points in the zero layer */
  void
  SetUpdate(const LevelSetLayerType & update);

protected:
  UpdateWhitakerSparseLevelSet();
  ~UpdateWhitakerSparseLevelSet() override = default;

  /** Update zero level set layer by moving relevant points to layers -1 or 1 */
  void
  UpdateLayerZero();

  /** Update -1 level set layer by moving relevant points to layers -2 or 0 */
  void
  UpdateLayerMinus1();

  /** Update +1 level set layer by moving relevant points to layers 0 or 2 */
  void
  UpdateLayerPlus1();

  /** Update zero level set layer by moving relevant points to layers -3 or -1 */
  void
  UpdateLayerMinus2();

  /** Update +2 level set layer by moving relevant points to layers 1 or 3 */
  void
  UpdateLayerPlus2();

  /** Move identified points into 0 level set layer */
  void
  MovePointIntoZeroLevelSet();

  /** Move identified points into -1 level set layer */
  void
  MovePointFromMinus1();

  /** Move identified points into +1 level set layer */
  void
  MovePointFromPlus1();

  /** Move identified points into -2 level set layer */
  void
  MovePointFromMinus2();

  /** Move identified points into +2 level set layer */
  void
  MovePointFromPlus2();

private:
  LevelSetOutputType m_TimeStep;
  LevelSetOutputType m_RMSChangeAccumulator;
  IdentifierType     m_CurrentLevelSetId;

  EquationContainerPointer m_EquationContainer;

  LevelSetLayerType m_Update;
  LevelSetPointer   m_InputLevelSet;
  LevelSetPointer   m_OutputLevelSet;

  LevelSetPointer   m_TempLevelSet;
  LevelSetLayerType m_TempPhi;

  LevelSetLayerIdType m_MinStatus;
  LevelSetLayerIdType m_MaxStatus;

  LabelImagePointer m_InternalImage;

  LevelSetOffsetType m_Offset;

  using NeighborhoodIteratorType = ShapedNeighborhoodIterator<LabelImageType>;

  using NodePairType = std::pair<LevelSetInputType, LevelSetOutputType>;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkUpdateWhitakerSparseLevelSet.hxx"
#endif
#endif // itkUpdateWhitakerSparseLevelSet_h
