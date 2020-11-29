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
#ifndef itkShapeRelabelLabelMapFilter_h
#define itkShapeRelabelLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkLabelObject.h"
#include "itkShapeLabelObjectAccessors.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *\class ShapeRelabelLabelMapFilter
 * \brief Relabels objects according to their shape attributes.
 *
 * The ShapeRelabelImageFilter relabels a label collection image according to the shape attributes of
 * the objects. The label produced are always consecutives.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ShapeRelabelLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ShapeRelabelLabelMapFilter);

  /** Standard class type aliases. */
  using Self = ShapeRelabelLabelMapFilter;
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
  itkTypeMacro(ShapeRelabelLabelMapFilter, InPlaceLabelMapFilter);

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
   * the highest attribute values are labeled first. Setting ReverseOrdering to true
   * causes the object with the smallest attributes to be labeled first.
   */
  itkSetMacro(ReverseOrdering, bool);
  itkGetConstReferenceMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

  /**
   * Set/Get the attribute to use.
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
  ShapeRelabelLabelMapFilter();
  ~ShapeRelabelLabelMapFilter() override = default;

  void
  GenerateData() override;

  template <typename TAttributeAccessor>
  void
  TemplatedGenerateData(const TAttributeAccessor &)
  {
    // Allocate the output
    this->AllocateOutputs();

    ImageType * output = this->GetOutput();

    using LabelObjectPointer = typename LabelObjectType::Pointer;
    using VectorType = std::vector<LabelObjectPointer>;

    ProgressReporter progress(this, 0, 2 * output->GetNumberOfLabelObjects());

    // Get the label objects in a vector, so they can be sorted
    VectorType labelObjects;
    labelObjects.reserve(output->GetNumberOfLabelObjects());
    for (typename ImageType::Iterator it(output); !it.IsAtEnd(); ++it)
    {
      labelObjects.push_back(it.GetLabelObject());
      progress.CompletedPixel();
    }

    // Instantiate the comparator and sort the vector
    if (m_ReverseOrdering)
    {
      std::sort(labelObjects.begin(),
                labelObjects.end(),
                Functor::LabelObjectReverseComparator<LabelObjectType, TAttributeAccessor>());
    }
    else
    {
      std::sort(labelObjects.begin(),
                labelObjects.end(),
                Functor::LabelObjectComparator<LabelObjectType, TAttributeAccessor>());
    }
    //   progress.CompletedPixel();

    // and put back the objects in the map
    output->ClearLabels();
    PixelType                           label = NumericTraits<PixelType>::ZeroValue();
    typename VectorType::const_iterator it2 = labelObjects.begin();
    while (it2 != labelObjects.end())
    {
      // Avoid the background label if it is used
      if (label == output->GetBackgroundValue())
      {
        label++;
      }
      (*it2)->SetLabel(label);
      output->AddLabelObject(*it2);

      // Go to the next label
      label++;
      progress.CompletedPixel();

      ++it2;
    }
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  bool m_ReverseOrdering;

  AttributeType m_Attribute;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShapeRelabelLabelMapFilter.hxx"
#endif

#endif
