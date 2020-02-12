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
#ifndef itkAttributeUniqueLabelMapFilter_h
#define itkAttributeUniqueLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk
{
/**
 *\class AttributeUniqueLabelMapFilter
 * \brief Make sure that the objects are not overlapping
 *
 * AttributeUniqueLabelMapFilter search the overlapping zones in the overlapping
 * objects and keeps only a single object on all the pixels of the image.
 * The object to keep is selected according to the value of the attribute
 * provided by the accessor. If the attribute values are equal for a set of
 * overlapping objects, the label is used to keep the priority consistent in the
 * whole image.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage,
          typename TAttributeAccessor =
            typename Functor::AttributeLabelObjectAccessor<typename TImage::LabelObjectType>>
class ITK_TEMPLATE_EXPORT AttributeUniqueLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(AttributeUniqueLabelMapFilter);

  /** Standard class type aliases. */
  using Self = AttributeUniqueLabelMapFilter;
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

  using LineType = typename LabelObjectType::LineType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AttributeUniqueLabelMapFilter, InPlaceLabelMapFilter);

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
  AttributeUniqueLabelMapFilter();
  ~AttributeUniqueLabelMapFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  bool m_ReverseOrdering;

private:
  struct LineOfLabelObject
  {
    using LineType = typename LabelObjectType::LineType;

    LineOfLabelObject(const LineType l, LabelObjectType * lo)
    {
      this->line = l;
      this->labelObject = lo;
    }
    LineType          line;
    LabelObjectType * labelObject;
  };

  class LineOfLabelObjectComparator
  {
  public:
    bool
    operator()(const LineOfLabelObject & lla, const LineOfLabelObject & llb)
    {
      for (int i = ImageDimension - 1; i >= 0; i--)
      {
        if (lla.line.GetIndex()[i] > llb.line.GetIndex()[i])
        {
          return true;
        }
        else if (lla.line.GetIndex()[i] < llb.line.GetIndex()[i])
        {
          return false;
        }
      }
      return false;
    }
  };

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAttributeUniqueLabelMapFilter.hxx"
#endif

#endif
