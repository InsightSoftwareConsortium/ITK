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

#ifndef itkBinaryImageToLevelSetImageAdaptor_h
#define itkBinaryImageToLevelSetImageAdaptor_h

#include "itkBinaryImageToLevelSetImageAdaptorBase.h"

#include "itkLevelSetDenseImage.h"
#include "itkImageToImageFilter.h"

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"

#include "itkShiSparseLevelSetImage.h"
#include "itkMalcolmSparseLevelSetImage.h"

namespace itk
{
/**
 *\class BinaryImageToLevelSetImageAdator
 *  \brief Converts one binary image to the appropriate level-set type
 *  provided by the template argument TLevelSet.
 *
 *  \tparam TInputImage Binary Input Image Type
 *  \tparam TLevelSet   Output Level-Set Type
 *
 *  \note TLevelSet must inherits from LevelSetImage
 *
 *  \sa LevelSetImage
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInputImage, typename TLevelSet>
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor
{};


/** \brief Partial template specialization for LevelSetDenseImage
 */
template <typename TInputImage, typename TLevelSetImage>
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor<TInputImage, LevelSetDenseImage<TLevelSetImage>>
  : public BinaryImageToLevelSetImageAdaptorBase<TInputImage, LevelSetDenseImage<TLevelSetImage>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptor);

  using LevelSetType = LevelSetDenseImage<TLevelSetImage>;

  using Self = BinaryImageToLevelSetImageAdaptor;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = BinaryImageToLevelSetImageAdaptorBase<TInputImage, LevelSetType>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(BinaryImageToLevelSetImageAdaptorBase, Object);

  using InputImageType = TInputImage;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImageIndexType = typename InputImageType::IndexType;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputPixelRealType = typename NumericTraits<InputImagePixelType>::RealType;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using LevelSetPointer = typename LevelSetType::Pointer;
  using LevelSetImageType = typename LevelSetType::ImageType;

  using SignedDistanceTransformFilterType = ImageToImageFilter<InputImageType, LevelSetImageType>;
  using SignedDistanceTransformFilterPointer = typename SignedDistanceTransformFilterType::Pointer;

  /** Set the signed distance image filter.  Defaults to a
   * SignedMaurerDistanceMapImageFilter. */
  itkSetObjectMacro(SignedDistanceTransformFilter, SignedDistanceTransformFilterType);
  itkGetModifiableObjectMacro(SignedDistanceTransformFilter, SignedDistanceTransformFilterType);

  /**
   * Input is a binary image m_InputImage
   * Output is a WhitakerSparseLevelSetImagePointer  */
  void
  Initialize() override;

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptor();

  /** Destructor */
  ~BinaryImageToLevelSetImageAdaptor() override;

private:
  SignedDistanceTransformFilterPointer m_SignedDistanceTransformFilter;
};

////////////////////////////////////////////////////////////////////////////////

/**
 *\class BinaryImageToSparseLevelSetImageAdaptorBase
 *  \brief Abstract class for converting binary image to sparse level-set
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT BinaryImageToSparseLevelSetImageAdaptorBase
  : public BinaryImageToLevelSetImageAdaptorBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToSparseLevelSetImageAdaptorBase);

  using Self = BinaryImageToSparseLevelSetImageAdaptorBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = BinaryImageToLevelSetImageAdaptorBase<TInput, TOutput>;

  /** Run-time type information */
  itkTypeMacro(BinaryImageToSparseLevelSetImageAdaptorBase, BinaryImageToLevelSetImageAdaptorBase);

  using InputImageType = typename Superclass::InputImageType;
  using InputImagePixelType = typename Superclass::InputImagePixelType;
  using InputImageIndexType = typename Superclass::InputImageIndexType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageRegionType = typename Superclass::InputImageRegionType;
  using InputPixelRealType = typename Superclass::InputPixelRealType;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using LevelSetType = typename Superclass::LevelSetType;
  using LevelSetPointer = typename Superclass::LevelSetPointer;

  using LevelSetInputType = typename LevelSetType::InputType;
  using LevelSetOutputType = typename LevelSetType::OutputType;

  using LevelSetLabelObjectType = typename LevelSetType::LabelObjectType;
  using LayerIdType = typename LevelSetLabelObjectType::LabelType;
  using LevelSetLabelObjectPointer = typename LevelSetType::LabelObjectPointer;
  using LevelSetLabelObjectLengthType = typename LevelSetType::LabelObjectLengthType;
  using LevelSetLabelObjectLineType = typename LevelSetType::LabelObjectLineType;

  using LevelSetLabelMapType = typename LevelSetType::LabelMapType;
  using LevelSetLabelMapPointer = typename LevelSetType::LabelMapPointer;

  using LevelSetLayerType = typename LevelSetType::LayerType;
  using LevelSetLayerIterator = typename LevelSetType::LayerIterator;
  using LevelSetLayerConstIterator = typename LevelSetType::LayerConstIterator;

  using InternalImageType = Image<signed char, ImageDimension>;
  using InternalImagePointer = typename InternalImageType::Pointer;

  using LayerPairType = std::pair<LevelSetInputType, LevelSetOutputType>;

  using InputIteratorType = ImageRegionIteratorWithIndex<InputImageType>;
  using InternalIteratorType = ImageRegionIteratorWithIndex<InternalImageType>;

  using NeighborhoodIteratorType = ShapedNeighborhoodIterator<InternalImageType>;

protected:
  BinaryImageToSparseLevelSetImageAdaptorBase()
    : Superclass()
  {}
  ~BinaryImageToSparseLevelSetImageAdaptorBase() override = default;

  LevelSetLabelMapPointer m_LabelMap;

  InternalImagePointer m_InternalImage;
};

////////////////////////////////////////////////////////////////////////////////
/** \brief Partial template specialization for WhitakerSparseLevelSetImage
 */
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT
  BinaryImageToLevelSetImageAdaptor<TInput, WhitakerSparseLevelSetImage<TOutput, TInput::ImageDimension>>
  : public BinaryImageToSparseLevelSetImageAdaptorBase<TInput,
                                                       WhitakerSparseLevelSetImage<TOutput, TInput::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptor);

  using LevelSetType = WhitakerSparseLevelSetImage<TOutput, TInput::ImageDimension>;

  using Self = BinaryImageToLevelSetImageAdaptor;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = BinaryImageToSparseLevelSetImageAdaptorBase<TInput, LevelSetType>;


  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(BinaryImageToLevelSetImageAdaptor, BinaryImageToSparseLevelSetImageAdaptorBase);

  using InputImageType = typename Superclass::InputImageType;
  using InputImagePixelType = typename Superclass::InputImagePixelType;
  using InputImageIndexType = typename Superclass::InputImageIndexType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageRegionType = typename Superclass::InputImageRegionType;
  using InputPixelRealType = typename Superclass::InputPixelRealType;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using LevelSetPointer = typename Superclass::LevelSetPointer;

  using LevelSetInputType = typename Superclass::LevelSetInputType;
  using LevelSetOutputType = typename Superclass::LevelSetOutputType;

  using LevelSetLabelObjectType = typename Superclass::LevelSetLabelObjectType;
  using LayerIdType = typename Superclass::LayerIdType;
  using LevelSetLabelObjectPointer = typename Superclass::LevelSetLabelObjectPointer;
  using LevelSetLabelObjectLengthType = typename Superclass::LevelSetLabelObjectLengthType;
  using LevelSetLabelObjectLineType = typename Superclass::LevelSetLabelObjectLineType;

  using LevelSetLabelMapType = typename Superclass::LevelSetLabelMapType;
  using LevelSetLabelMapPointer = typename Superclass::LevelSetLabelMapPointer;

  using LevelSetLayerType = typename Superclass::LevelSetLayerType;
  using LevelSetLayerIterator = typename Superclass::LevelSetLayerIterator;
  using LevelSetLayerConstIterator = typename Superclass::LevelSetLayerConstIterator;

  using InternalImageType = typename Superclass::InternalImageType;
  using InternalImagePointer = typename Superclass::InternalImagePointer;

  using LayerPairType = typename Superclass::LayerPairType;

  using InputIteratorType = typename Superclass::InputIteratorType;
  using InternalIteratorType = typename Superclass::InternalIteratorType;

  using NeighborhoodIteratorType = typename Superclass::NeighborhoodIteratorType;

  void
  Initialize() override;

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptor();

  /** Destructor */
  ~BinaryImageToLevelSetImageAdaptor() override;

private:
  /** Fill layer adjacent (OutputLayer) to the layer (LayerToBeScanned) */
  void
  PropagateToOuterLayers(LayerIdType LayerToBeScanned, LayerIdType OutputLayer, LayerIdType TestValue);

  /** Fill the layer corresponding to zero level set */
  void
  FindActiveLayer();

  /** Fill layers adjacent to the zero level set (i.e. layer -1 and +1 )*/
  void
  FindPlusOneMinusOneLayer();
};

////////////////////////////////////////////////////////////////////////////////
/** \brief Partial template specialization for ShiSparseLevelSetImage
 */
template <typename TInput>
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor<TInput, ShiSparseLevelSetImage<TInput::ImageDimension>>
  : public BinaryImageToSparseLevelSetImageAdaptorBase<TInput, ShiSparseLevelSetImage<TInput::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptor);

  using LevelSetType = ShiSparseLevelSetImage<TInput::ImageDimension>;

  using Self = BinaryImageToLevelSetImageAdaptor;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = BinaryImageToSparseLevelSetImageAdaptorBase<TInput, LevelSetType>;

  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(BinaryImageToLevelSetImageAdaptor, BinaryImageToSparseLevelSetImageAdaptorBase);

  using InputImageType = typename Superclass::InputImageType;

  using InputImagePixelType = typename Superclass::InputImagePixelType;
  using InputImageIndexType = typename Superclass::InputImageIndexType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageRegionType = typename Superclass::InputImageRegionType;
  using InputPixelRealType = typename Superclass::InputPixelRealType;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  //  using LevelSetType = typename Superclass::LevelSetType;
  using LevelSetPointer = typename Superclass::LevelSetPointer;

  using LevelSetInputType = typename Superclass::LevelSetInputType;
  using LevelSetOutputType = typename Superclass::LevelSetOutputType;

  using LevelSetLabelObjectType = typename Superclass::LevelSetLabelObjectType;
  using LayerIdType = typename Superclass::LayerIdType;
  using LevelSetLabelObjectPointer = typename Superclass::LevelSetLabelObjectPointer;
  using LevelSetLabelObjectLengthType = typename Superclass::LevelSetLabelObjectLengthType;
  using LevelSetLabelObjectLineType = typename Superclass::LevelSetLabelObjectLineType;

  using LevelSetLabelMapType = typename Superclass::LevelSetLabelMapType;
  using LevelSetLabelMapPointer = typename Superclass::LevelSetLabelMapPointer;

  using LevelSetLayerType = typename Superclass::LevelSetLayerType;
  using LevelSetLayerIterator = typename Superclass::LevelSetLayerIterator;
  using LevelSetLayerConstIterator = typename Superclass::LevelSetLayerConstIterator;

  using InternalImageType = typename Superclass::InternalImageType;
  using InternalImagePointer = typename Superclass::InternalImagePointer;

  using LayerPairType = typename Superclass::LayerPairType;

  using InputIteratorType = typename Superclass::InputIteratorType;
  using InternalIteratorType = typename Superclass::InternalIteratorType;

  using NeighborhoodIteratorType = typename Superclass::NeighborhoodIteratorType;

  void
  Initialize() override;

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptor();

  /** Destructor */
  ~BinaryImageToLevelSetImageAdaptor() override;

  /** Find the active layer separating the foreground and background regions */
  void
  FindActiveLayer();

private:
};


////////////////////////////////////////////////////////////////////////////////
/** \brief Partial template specialization for MalcolmSparseLevelSetImage
 */
template <typename TInput>
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor<TInput, MalcolmSparseLevelSetImage<TInput::ImageDimension>>
  : public BinaryImageToSparseLevelSetImageAdaptorBase<TInput, MalcolmSparseLevelSetImage<TInput::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptor);

  using LevelSetType = MalcolmSparseLevelSetImage<TInput::ImageDimension>;

  using Self = BinaryImageToLevelSetImageAdaptor;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = BinaryImageToSparseLevelSetImageAdaptorBase<TInput, LevelSetType>;


  /** Method for creation through object factory */
  itkNewMacro(Self);

  /** Run-time type information */
  itkTypeMacro(BinaryImageToLevelSetImageAdaptor, BinaryImageToSparseLevelSetImageAdaptorBase);

  using InputImageType = typename Superclass::InputImageType;

  using InputImagePixelType = typename Superclass::InputImagePixelType;
  using InputImageIndexType = typename Superclass::InputImageIndexType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageRegionType = typename Superclass::InputImageRegionType;
  using InputPixelRealType = typename Superclass::InputPixelRealType;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;


  using LevelSetPointer = typename Superclass::LevelSetPointer;
  using LevelSetInputType = typename Superclass::LevelSetInputType;
  using LevelSetOutputType = typename Superclass::LevelSetOutputType;

  using LevelSetLabelObjectType = typename Superclass::LevelSetLabelObjectType;
  using LayerIdType = typename Superclass::LayerIdType;
  using LevelSetLabelObjectPointer = typename Superclass::LevelSetLabelObjectPointer;
  using LevelSetLabelObjectLengthType = typename Superclass::LevelSetLabelObjectLengthType;
  using LevelSetLabelObjectLineType = typename Superclass::LevelSetLabelObjectLineType;

  using LevelSetLabelMapType = typename Superclass::LevelSetLabelMapType;
  using LevelSetLabelMapPointer = typename Superclass::LevelSetLabelMapPointer;

  using LevelSetLayerType = typename Superclass::LevelSetLayerType;
  using LevelSetLayerIterator = typename Superclass::LevelSetLayerIterator;
  using LevelSetLayerConstIterator = typename Superclass::LevelSetLayerConstIterator;

  using InternalImageType = typename Superclass::InternalImageType;
  using InternalImagePointer = typename Superclass::InternalImagePointer;

  using LayerPairType = typename Superclass::LayerPairType;

  using InputIteratorType = typename Superclass::InputIteratorType;
  using InternalIteratorType = typename Superclass::InternalIteratorType;

  using NeighborhoodIteratorType = typename Superclass::NeighborhoodIteratorType;

  void
  Initialize() override;

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptor();

  /** Destructor */
  ~BinaryImageToLevelSetImageAdaptor() override;

  /** Find the active layer separating the foreground and background regions */
  void
  FindActiveLayer();

  /** Ensure that the 0 level set layer is only of single pixel thickness */
  void
  CreateMinimalInterface();
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryImageToLevelSetImageAdaptor.hxx"
#endif
#endif // itkBinaryImageToLevelSetImageAdaptorBase_h
