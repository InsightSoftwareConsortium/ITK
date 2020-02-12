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
#ifndef itkAttributeKeepNObjectsLabelMapFilter_h
#define itkAttributeKeepNObjectsLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk
{
/**
 *\class AttributeKeepNObjectsLabelMapFilter
 * \brief keep N objects according to their attribute value
 *
 * AttributeKeepNObjectsLabelMapFilter keeps the N objects in a label collection image
 * with the highest (or lowest) attribute value. The attribute is provided by an
 * attribute accessor given in template parameter.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage,
          typename TAttributeAccessor =
            typename Functor::AttributeLabelObjectAccessor<typename TImage::LabelObjectType>>
class ITK_TEMPLATE_EXPORT AttributeKeepNObjectsLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(AttributeKeepNObjectsLabelMapFilter);

  /** Standard class type aliases. */
  using Self = AttributeKeepNObjectsLabelMapFilter;
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
  itkTypeMacro(AttributeKeepNObjectsLabelMapFilter, InPlaceLabelMapFilter);

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
   * Set/Get the ordering of the objects. By default, the ones with the
   * highest value are kept. Turming ReverseOrdering to true make this filter
   * keep the objects with the smallest values
   */
  itkSetMacro(ReverseOrdering, bool);
  itkGetConstReferenceMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

  /**
   * Set/Get the number of objects to keep
   */
  itkSetMacro(NumberOfObjects, SizeValueType);
  itkGetConstReferenceMacro(NumberOfObjects, SizeValueType);

protected:
  AttributeKeepNObjectsLabelMapFilter();
  ~AttributeKeepNObjectsLabelMapFilter() override = default;

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
  bool          m_ReverseOrdering;
  SizeValueType m_NumberOfObjects;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAttributeKeepNObjectsLabelMapFilter.hxx"
#endif

#endif
