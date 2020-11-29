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
#ifndef itkShapeOpeningLabelMapFilter_h
#define itkShapeOpeningLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkShapeLabelObjectAccessors.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *\class ShapeOpeningLabelMapFilter
 * \brief Remove objects according to the value of their shape attribute.
 *
 * ShapeOpeningLabelMapFilter removes objects in a label collection image
 * with an attribute value smaller or greater than a threshold called Lambda.
 * The attributes are those of the ShapeLabelObject.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, BinaryShapeOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/KeepRegionsThatMeetSpecific,Keep Regions That Meet Specific Properties}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ShapeOpeningLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ShapeOpeningLabelMapFilter);

  /** Standard class type aliases. */
  using Self = ShapeOpeningLabelMapFilter;
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
  itkTypeMacro(ShapeOpeningLabelMapFilter, InPlaceLabelMapFilter);

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
   * Set/Get the threshold used to keep or remove the objects.
   */
  itkGetConstMacro(Lambda, double);
  itkSetMacro(Lambda, double);

  /**
   * Set/Get the ordering of the objects. By default, objects with
   * an attribute value smaller than Lamba are removed. Turning ReverseOrdering
   * to true makes this filter remove objects with an attribute value greater
   * than Lambda instead.
   */
  itkGetConstMacro(ReverseOrdering, bool);
  itkSetMacro(ReverseOrdering, bool);
  itkBooleanMacro(ReverseOrdering);

  /**
   * Set/Get the attribute to use to select the object to remove.
   * The default is "Size".
   */
  itkGetConstMacro(Attribute, AttributeType);
  itkSetMacro(Attribute, AttributeType);
  void
  SetAttribute(const std::string & s)
  {
    this->SetAttribute(LabelObjectType::GetAttributeFromName(s));
  }

protected:
  ShapeOpeningLabelMapFilter();
  ~ShapeOpeningLabelMapFilter() override = default;

  void
  GenerateData() override;

  template <typename TAttributeAccessor>
  void
  TemplatedGenerateData(const TAttributeAccessor & accessor)
  {
    // Allocate the output
    this->AllocateOutputs();

    ImageType * output = this->GetOutput();
    ImageType * output2 = this->GetOutput(1);
    itkAssertInDebugAndIgnoreInReleaseMacro(this->GetNumberOfIndexedOutputs() == 2);
    itkAssertInDebugAndIgnoreInReleaseMacro(output2 != nullptr);

    // set the background value for the second output - this is not done in the
    // superclasses
    output2->SetBackgroundValue(output->GetBackgroundValue());

    ProgressReporter progress(this, 0, output->GetNumberOfLabelObjects());

    typename ImageType::Iterator it(output);
    while (!it.IsAtEnd())
    {
      typename LabelObjectType::LabelType label = it.GetLabel();
      LabelObjectType *                   labelObject = it.GetLabelObject();

      if ((!m_ReverseOrdering && accessor(labelObject) < m_Lambda) ||
          (m_ReverseOrdering && accessor(labelObject) > m_Lambda))
      {
        // must increment the iterator before removing the object to avoid
        // invalidating the iterator
        ++it;
        output2->AddLabelObject(labelObject);
        output->RemoveLabel(label);
      }
      else
      {
        ++it;
      }

      progress.CompletedPixel();
    }
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  double m_Lambda;

  bool m_ReverseOrdering;

  AttributeType m_Attribute;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShapeOpeningLabelMapFilter.hxx"
#endif

#endif
