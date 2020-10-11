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

#ifndef itkUpdateMalcolmSparseLevelSet_h
#define itkUpdateMalcolmSparseLevelSet_h

#include "itkImage.h"
#include "itkDiscreteLevelSetImage.h"
#include "itkMalcolmSparseLevelSetImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkLabelImageToLabelMapFilter.h"

namespace itk
{
/**
 *  \class UpdateMalcolmSparseLevelSet
 *  \brief Base class for updating the Malcolm representation of level-set function
 *
 *  \tparam VDimension Dimension of the input space
 *  \tparam TEquationContainer Container of the system of levelset equations
 *  \ingroup ITKLevelSetsv4
 */
template <unsigned int VDimension, typename TEquationContainer>
class ITK_TEMPLATE_EXPORT UpdateMalcolmSparseLevelSet : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(UpdateMalcolmSparseLevelSet);

  using Self = UpdateMalcolmSparseLevelSet;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = Object;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(UpdateMalcolmSparseLevelSet, Object);

  static constexpr unsigned int ImageDimension = VDimension;

  using LevelSetType = MalcolmSparseLevelSetImage<ImageDimension>;
  using LevelSetPointer = typename LevelSetType::Pointer;
  using LevelSetInputType = typename LevelSetType::InputType;
  using LevelSetOutputType = typename LevelSetType::OutputType;
  using LevelSetOffsetType = typename LevelSetType::OffsetType;

  using LevelSetLabelMapType = typename LevelSetType::LabelMapType;
  using LevelSetLabelMapPointer = typename LevelSetType::LabelMapPointer;

  using LevelSetLabelObjectType = typename LevelSetType::LabelObjectType;
  using LevelSetLabelObjectPointer = typename LevelSetType::LabelObjectPointer;
  using LevelSetLabelObjectLengthType = typename LevelSetType::LabelObjectLengthType;
  using LevelSetLabelObjectLineType = typename LevelSetType::LabelObjectLineType;

  using LevelSetLayerType = typename LevelSetType::LayerType;
  using LevelSetLayerIterator = typename LevelSetType::LayerIterator;
  using LevelSetLayerConstIterator = typename LevelSetType::LayerConstIterator;
  using LevelSetOutputRealType = typename LevelSetType::OutputRealType;

  using LevelSetLayerMapType = typename LevelSetType::LayerMapType;
  using LevelSetLayerMapIterator = typename LevelSetType::LayerMapIterator;
  using LevelSetLayerMapConstIterator = typename LevelSetType::LayerMapConstIterator;

  using EquationContainerType = TEquationContainer;
  using EquationContainerPointer = typename EquationContainerType::Pointer;
  using TermContainerPointer = typename EquationContainerType::TermContainerPointer;

  itkGetModifiableObjectMacro(OutputLevelSet, LevelSetType);

  /** Update function for initializing and computing the output level set */
  void
  Update();

  /** Set/Get the sparse levet set image */
  itkSetObjectMacro(InputLevelSet, LevelSetType);
  itkGetModifiableObjectMacro(InputLevelSet, LevelSetType);

  /** Set/Get the RMS change for the update */
  itkGetMacro(RMSChangeAccumulator, LevelSetOutputRealType);

  /** Set/Get the Equation container for computing the update */
  itkSetObjectMacro(EquationContainer, EquationContainerType);
  itkGetModifiableObjectMacro(EquationContainer, EquationContainerType);

  /** Set/Get the current level set id */
  itkSetMacro(CurrentLevelSetId, IdentifierType);
  itkGetMacro(CurrentLevelSetId, IdentifierType);

protected:
  UpdateMalcolmSparseLevelSet();
  ~UpdateMalcolmSparseLevelSet() override = default;

  // output
  LevelSetPointer m_OutputLevelSet;

  LevelSetLayerType m_Update;

  IdentifierType           m_CurrentLevelSetId;
  LevelSetOutputRealType   m_RMSChangeAccumulator;
  EquationContainerPointer m_EquationContainer;

  using LabelImageType = Image<int8_t, ImageDimension>;
  using LabelImagePointer = typename LabelImageType::Pointer;

  LabelImagePointer m_InternalImage;

  using NeighborhoodIteratorType = ShapedNeighborhoodIterator<LabelImageType>;

  bool m_IsUsingUnPhasedPropagation{ true };

  /** Compute the updates for all points in the 0 layer and store in UpdateContainer */
  void
  FillUpdateContainer();

  /** Update the zero layer for all points with values stored in UpdateContainer
   *  Move points to -1 or +1 layers */
  void
  EvolveWithUnPhasedPropagation();

  /** Update separately the zero layer for points with positive/negative update values
   *  Move points to -1 or +1 layers */
  void
  EvolveWithPhasedPropagation(LevelSetLayerType & ioList, LevelSetLayerType & ioUpdate, const bool & iContraction);

  /** Make sure the layers are of single pixel thickness only. This method is related
    to the minimal interface function described in the original paper. */
  void
  CompactLayersToSinglePixelThickness();

private:
  // input
  LevelSetPointer m_InputLevelSet;

  LevelSetOffsetType m_Offset;

  using NodePairType = std::pair<LevelSetInputType, LevelSetOutputType>;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkUpdateMalcolmSparseLevelSet.hxx"
#endif

#endif // itkUpdateMalcolmSparseLevelSet_h
