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
#ifndef itkBinaryReconstructionLabelMapFilter_h
#define itkBinaryReconstructionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk
{
/**
 *\class BinaryReconstructionLabelMapFilter
 * \brief Mark the objects at least partially at the same position as the objects in a binary image
 *
 * The attribute is accessed through the accessor given with TAttributeAccessor.
 * The LabelObjects from the input LabelMap are marked with "true" if at least one of their pixel is
 * at the same position than an object in the binary marker image.
 * In the marker image, the pixels with a value equal to ForegroundValue are considered to be in the
 * objects.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage,
          typename TMarkerImage,
          typename TAttributeAccessor =
            typename Functor::AttributeLabelObjectAccessor<typename TImage::LabelObjectType>>
class ITK_TEMPLATE_EXPORT BinaryReconstructionLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryReconstructionLabelMapFilter);

  /** Standard class type aliases. */
  using Self = BinaryReconstructionLabelMapFilter;
  using Superclass = InPlaceLabelMapFilter<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using PixelType = typename ImageType::PixelType;
  using IndexType = typename ImageType::IndexType;
  using LabelObjectType = typename ImageType::LabelObjectType;

  using MarkerImageType = TMarkerImage;
  using MarkerImagePointer = typename MarkerImageType::Pointer;
  using MarkerImageConstPointer = typename MarkerImageType::ConstPointer;
  using MarkerImagePixelType = typename MarkerImageType::PixelType;

  using AttributeAccessorType = TAttributeAccessor;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BinaryReconstructionLabelMapFilter, InPlaceLabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  /*  itkConceptMacro(InputEqualityComparableCheck,
      (Concept::EqualityComparable<PixelType>));
    itkConceptMacro(IntConvertibleToInputCheck,
      (Concept::Convertible<int, PixelType>));
    itkConceptMacro(InputOStreamWritableCheck,
      (Concept::OStreamWritable<PixelType>));*/
  // End concept checking
#endif

  /** Set/Get the marker image */
  itkSetInputMacro(MarkerImage, MarkerImageType);
  itkGetInputMacro(MarkerImage, MarkerImageType);

  /** Set the input image */
  void
  SetInput1(TImage * input)
  {
    this->SetInput(input);
  }

  /** Set the marker image */
  void
  SetInput2(TMarkerImage * input)
  {
    this->SetMarkerImage(input);
  }

  /**
   * Set/Get the value used as "foreground" in the output image.
   * Defaults to NumericTraits<MaskPixelType>::max().
   */
  itkSetMacro(ForegroundValue, MarkerImagePixelType);
  itkGetConstMacro(ForegroundValue, MarkerImagePixelType);

protected:
  BinaryReconstructionLabelMapFilter();
  ~BinaryReconstructionLabelMapFilter() override = default;

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  MarkerImagePixelType m_ForegroundValue;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryReconstructionLabelMapFilter.hxx"
#endif

#endif
