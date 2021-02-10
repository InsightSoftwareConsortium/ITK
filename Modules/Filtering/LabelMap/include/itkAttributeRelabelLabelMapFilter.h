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
#ifndef itkAttributeRelabelLabelMapFilter_h
#define itkAttributeRelabelLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk
{
/**
 *\class AttributeRelabelLabelMapFilter
 * \brief relabel objects according to their shape attributes
 *
 * AttributeRelabelLabelMapFilter relabel a label map according to the
 * attribute values of the objects.
 * The attribute is provide by an attribute accessor given in template parameter.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa AttributeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage,
          typename TAttributeAccessor =
            typename Functor::AttributeLabelObjectAccessor<typename TImage::LabelObjectType>>
class ITK_TEMPLATE_EXPORT AttributeRelabelLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AttributeRelabelLabelMapFilter);

  /** Standard class type aliases. */
  using Self = AttributeRelabelLabelMapFilter;
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

  using AttributeAccessorType = TAttributeAccessor;
  using AttributeValueType = typename AttributeAccessorType::AttributeValueType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AttributeRelabelLabelMapFilter, InPlaceLabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  /*  itkConceptMacro(InputEqualityComparableCheck,
      (Concept::EqualityComparable<InputImagePixelType>));
    itkConceptMacro(IntConvertibleToInputCheck,
      (Concept::Convertible<int, InputImagePixelType>));
    itkConceptMacro(InputOStreamWritableCheck,
      (Concept::OStreamWritable<InputImagePixelType>));*/
  // End concept checking
#endif

  /**
   * Set/Get the order of labeling of the objects. By default, the objects with
   * the highest attribute values are labeled first. Set ReverseOrdering to true
   * make the one with the smallest attributes be labeled first.
   */
  itkSetMacro(ReverseOrdering, bool);
  itkGetConstReferenceMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

protected:
  AttributeRelabelLabelMapFilter();
  ~AttributeRelabelLabelMapFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  class ReverseComparator
  {
  public:
    bool
    operator()(const typename LabelObjectType::Pointer & a, const typename LabelObjectType::Pointer & b)
    {
      return m_Accessor(a) < m_Accessor(b);
    }
    ReverseComparator()
      : m_Accessor()
    {}

  private:
    AttributeAccessorType m_Accessor;
  };

  class Comparator
  {
  public:
    bool
    operator()(const typename LabelObjectType::Pointer & a, const typename LabelObjectType::Pointer & b)
    {
      return m_Accessor(a) > m_Accessor(b);
    }
    Comparator()
      : m_Accessor()
    {}

  private:
    AttributeAccessorType m_Accessor;
  };

private:
  bool m_ReverseOrdering;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAttributeRelabelLabelMapFilter.hxx"
#endif

#endif
