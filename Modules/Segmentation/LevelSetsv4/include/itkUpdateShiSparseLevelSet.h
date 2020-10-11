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

#ifndef itkUpdateShiSparseLevelSet_h
#define itkUpdateShiSparseLevelSet_h

#include "itkImage.h"
#include "itkDiscreteLevelSetImage.h"
#include "itkShiSparseLevelSetImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkLabelImageToLabelMapFilter.h"

namespace itk
{
/**
 *  \class UpdateShiSparseLevelSet
 *  \brief Base class for updating the Shi representation of level-set function
 *
 *  \tparam VDimension Dimension of the input space
 *  \tparam TEquationContainer Container of the system of levelset equations
 *  \ingroup ITKLevelSetsv4
 */
template <unsigned int VDimension, typename TEquationContainer>
class ITK_TEMPLATE_EXPORT UpdateShiSparseLevelSet : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(UpdateShiSparseLevelSet);

  using Self = UpdateShiSparseLevelSet;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = Object;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(UpdateShiSparseLevelSet, Object);

  static constexpr unsigned int ImageDimension = VDimension;

  using LevelSetType = ShiSparseLevelSetImage<ImageDimension>;
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
  UpdateShiSparseLevelSet();
  ~UpdateShiSparseLevelSet() override = default;

  // output
  LevelSetPointer m_OutputLevelSet;

  IdentifierType           m_CurrentLevelSetId;
  LevelSetOutputRealType   m_RMSChangeAccumulator;
  EquationContainerPointer m_EquationContainer;

  using LabelImageType = Image<int8_t, ImageDimension>;
  using LabelImagePointer = typename LabelImageType::Pointer;

  LabelImagePointer m_InternalImage;

  using NeighborhoodIteratorType = ShapedNeighborhoodIterator<LabelImageType>;

  /** Update +1 level set layers by checking the direction of the movement towards -1 */
  // this is the same as Procedure 2
  // Input is a update image point m_UpdateImage
  // Input is also ShiSparseLevelSetImagePointer
  void
  UpdateLayerPlusOne();

  /** Update -1 level set layers by checking the direction of the movement towards +1 */
  void
  UpdateLayerMinusOne();

  /** Return true if there is a pixel from the opposite layer (+1 or -1) moving in the same direction */
  bool
  Con(const LevelSetInputType &      idx,
      const LevelSetOutputType &     currentStatus,
      const LevelSetOutputRealType & currentUpdate) const;

private:
  // input
  LevelSetPointer    m_InputLevelSet;
  LevelSetOffsetType m_Offset;

  using NodePairType = std::pair<LevelSetInputType, LevelSetOutputType>;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkUpdateShiSparseLevelSet.hxx"
#endif

#endif // itkUpdateShiSparseLevelSet_h
