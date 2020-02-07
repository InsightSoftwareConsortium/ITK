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

#ifndef itkLevelSetSparseImage_h
#define itkLevelSetSparseImage_h

#include "itkDiscreteLevelSetImage.h"
#include "itkObjectFactory.h"

#include "itkLabelObject.h"
#include "itkLabelMap.h"
#include "itkLexicographicCompare.h"

namespace itk
{

/**
 *  \class LevelSetSparseImage
 *  \brief Base class for the sparse representation of a level-set function on one Image.
 *
 *  \tparam TImage Input image type of the level set function
 *  \todo Think about using image iterators instead of GetPixel()
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TOutput, unsigned int VDimension>
class ITK_TEMPLATE_EXPORT LevelSetSparseImage : public DiscreteLevelSetImage<TOutput, VDimension>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetSparseImage);

  using Self = LevelSetSparseImage;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = DiscreteLevelSetImage<TOutput, VDimension>;

  /** Run-time type information */
  itkTypeMacro(LevelSetSparseImage, DiscreteLevelSetImage);

  static constexpr unsigned int Dimension = Superclass::Dimension;

  using InputType = typename Superclass::InputType;
  using OutputType = typename Superclass::OutputType;
  using OutputRealType = typename Superclass::OutputRealType;
  using GradientType = typename Superclass::GradientType;
  using HessianType = typename Superclass::HessianType;
  using LevelSetDataType = typename Superclass::LevelSetDataType;

  using LayerIdType = int8_t;
  using LayerIdListType = std::list<LayerIdType>;

  using LabelObjectType = LabelObject<LayerIdType, VDimension>;
  using LabelObjectPointer = typename LabelObjectType::Pointer;
  using LabelObjectLengthType = typename LabelObjectType::LengthType;
  using LabelObjectLineType = typename LabelObjectType::LineType;

  using LabelMapType = LabelMap<LabelObjectType>;
  using LabelMapPointer = typename LabelMapType::Pointer;
  using LabelMapConstPointer = typename LabelMapType::ConstPointer;
  using RegionType = typename LabelMapType::RegionType;

  using LayerType = std::map<InputType, OutputType, Functor::LexicographicCompare>;
  using LayerIterator = typename LayerType::iterator;
  using LayerConstIterator = typename LayerType::const_iterator;

  using LayerMapType = std::map<LayerIdType, LayerType>;
  using LayerMapIterator = typename LayerMapType::iterator;
  using LayerMapConstIterator = typename LayerMapType::const_iterator;

  /** Returns the layer affiliation of a given location inputIndex */
  virtual LayerIdType
  Status(const InputType & inputIndex) const;

  /** Return the const reference to a layer map with given id  */
  const LayerType &
  GetLayer(LayerIdType value) const;

  /** Return the pointer to a layer map with given id  */
  LayerType &
  GetLayer(LayerIdType value);

  /** Set a layer map with id to the given layer pointer */
  void
  SetLayer(LayerIdType value, const LayerType & layer);

  /** Set/Get the label map for computing the sparse representation */
  virtual void
  SetLabelMap(LabelMapType * labelMap);
  itkGetModifiableObjectMacro(LabelMap, LabelMapType);

  /** Graft data object as level set object */
  void
  Graft(const DataObject * data) override;

  /** Return the label object pointer with a given id */
  template <typename TLabel>
  typename LabelObject<TLabel, VDimension>::Pointer
  GetAsLabelObject();

protected:
  LevelSetSparseImage() = default;
  ~LevelSetSparseImage() override = default;

  LayerMapType    m_Layers;
  LabelMapPointer m_LabelMap;
  LayerIdListType m_InternalLabelList;

  /** Initialize the sparse field layers */
  virtual void
  InitializeLayers() = 0;

  virtual void
  InitializeInternalLabelList() = 0;

  bool
  IsInsideDomain(const InputType & inputIndex) const override;

  /** Initialize the label map point and the sparse-field layers */
  void
  Initialize() override;

  /** Copy level set information from data object */
  void
  CopyInformation(const DataObject * data) override;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetSparseImage.hxx"
#endif

#endif // itkLevelSetSparseImage_h
