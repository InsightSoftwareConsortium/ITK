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
#ifndef itkShapeKeepNObjectsLabelMapFilter_h
#define itkShapeKeepNObjectsLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkShapeLabelObjectAccessors.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *\class ShapeKeepNObjectsLabelMapFilter
 * \brief Keep N objects according to their shape attributes.
 *
 * The ShapeKeepNObjectsLabelMapFilter keeps N objects in a label collection image with the
 * highest (or lowest) attribute value. The attributes values are those of the ShapeLabelObject.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, BinaryShapeKeepNObjectsImageFilter, LabelStatisticsKeepNObjectsImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ShapeKeepNObjectsLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShapeKeepNObjectsLabelMapFilter);

  /** Standard class type aliases. */
  using Self = ShapeKeepNObjectsLabelMapFilter;
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
  itkTypeMacro(ShapeKeepNObjectsLabelMapFilter, InPlaceLabelMapFilter);

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
   * highest value are kept. Turning ReverseOrdering to true make this filter
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

  /**
   * Set/Get the attribute to use to select the object to keep.
   * Default is "Size".
   */
  itkGetConstMacro(Attribute, AttributeType);
  itkSetMacro(Attribute, AttributeType);

  void
  SetAttribute(const std::string & s)
  {
    this->SetAttribute(LabelObjectType::GetAttributeFromName(s));
  }

protected:
  ShapeKeepNObjectsLabelMapFilter();
  ~ShapeKeepNObjectsLabelMapFilter() override = default;

  void
  GenerateData() override;

  template <typename TAttributeAccessor>
  void
  TemplatedGenerateData(const TAttributeAccessor &)
  {
    // Allocate the output
    this->AllocateOutputs();

    ImageType * output = this->GetOutput();
    ImageType * output2 = this->GetOutput(1);

    // set the background value for the second output - this is not done in the
    // superclasses
    output2->SetBackgroundValue(output->GetBackgroundValue());

    using LabelObjectPointer = typename LabelObjectType::Pointer;
    using VectorType = std::vector<LabelObjectPointer>;

    ProgressReporter progress(this, 0, 2 * output->GetNumberOfLabelObjects());

    // get the label objects in a vector, so they can be sorted
    VectorType labelObjects;
    labelObjects.reserve(output->GetNumberOfLabelObjects());
    typename ImageType::Iterator it(output);
    while (!it.IsAtEnd())
    {
      labelObjects.push_back(it.GetLabelObject());
      progress.CompletedPixel();
      ++it;
    }

    // instantiate the comparator and sort the vector
    if (m_NumberOfObjects < output->GetNumberOfLabelObjects())
    {
      auto end = labelObjects.begin() + m_NumberOfObjects;
      if (m_ReverseOrdering)
      {
        Functor::LabelObjectReverseComparator<LabelObjectType, TAttributeAccessor> comparator;
        std::nth_element(labelObjects.begin(), end, labelObjects.end(), comparator);
      }
      else
      {
        Functor::LabelObjectComparator<LabelObjectType, TAttributeAccessor> comparator;
        std::nth_element(labelObjects.begin(), end, labelObjects.end(), comparator);
      }
      progress.CompletedPixel();

      // and remove the last objects of the map
      for (typename VectorType::const_iterator it2 = end; it2 != labelObjects.end(); it2++)
      {
        output2->AddLabelObject(*it2);
        output->RemoveLabelObject(*it2);
        progress.CompletedPixel();
      }
    }
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  bool m_ReverseOrdering;

  SizeValueType m_NumberOfObjects;
  AttributeType m_Attribute;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShapeKeepNObjectsLabelMapFilter.hxx"
#endif

#endif
