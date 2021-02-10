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
#ifndef itkShapePositionLabelMapFilter_h
#define itkShapePositionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/**
 *\class ShapePositionLabelMapFilter
 * \brief Mark a single pixel in the label object which correspond to a position given by an attribute
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, BinaryShapeOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ShapePositionLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ShapePositionLabelMapFilter);

  /** Standard class type aliases. */
  using Self = ShapePositionLabelMapFilter;
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

  using AttributeType = typename LabelObjectType::AttributeType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ShapePositionLabelMapFilter, InPlaceLabelMapFilter);

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
   * Set/Get the attribute to use to get the object position. The default
   * is "Centroid".
   */
  itkGetConstMacro(Attribute, AttributeType);
  itkSetMacro(Attribute, AttributeType);
  void
  SetAttribute(const std::string & s)
  {
    this->SetAttribute(LabelObjectType::GetAttributeFromName(s));
  }

protected:
  ShapePositionLabelMapFilter();
  ~ShapePositionLabelMapFilter() override = default;

  void
  ThreadedProcessLabelObject(LabelObjectType * labelObject) override;

  template <typename TAttributeAccessor>
  void
  TemplatedThreadedProcessLabelObject(const TAttributeAccessor & accessor, bool physical, LabelObjectType * labelObject)
  {
    using AttributeValueType = typename TAttributeAccessor::AttributeValueType;
    AttributeValueType position = accessor(labelObject);
    // change it to an index position if it is physical
    IndexType idx;
    if (physical)
    {
      using CoordinateType = double;
      Point<CoordinateType, ImageDimension> point;
      // copy the position to a point, required by TransformPhysicalPointToIndex
      for (unsigned int i = 0; i < ImageDimension; i++)
      {
        // FIXME: This is a bug. The cast should be as in the following line
        // where CoordinateType is used as the type to cast to. We are temporarily
        // keeping this original line here to avoid confusing the patch for 64 bits.
        point[i] = static_cast<OffsetValueType>(position[i]); // FIXME: use next line instead.
        // point[i] = static_cast<CoordinateType>( position[i] );
      }
      this->GetOutput()->TransformPhysicalPointToIndex(point, idx);
    }
    else
    {
      // copy the position to the index, to avoid warnings
      for (unsigned int i = 0; i < ImageDimension; i++)
      {
        idx[i] = static_cast<IndexValueType>(position[i]);
      }
    }
    // clear the label object
    labelObject->Clear();
    // and mark only the pixel we are interested in
    labelObject->AddIndex(idx);
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  AttributeType m_Attribute;
}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShapePositionLabelMapFilter.hxx"
#endif

#endif
