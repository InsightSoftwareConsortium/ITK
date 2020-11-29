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
#ifndef itkStatisticsRelabelLabelMapFilter_h
#define itkStatisticsRelabelLabelMapFilter_h

#include "itkShapeRelabelLabelMapFilter.h"
#include "itkStatisticsLabelObject.h"
#include "itkStatisticsLabelObjectAccessors.h"

namespace itk
{
/**
 *\class StatisticsRelabelLabelMapFilter
 * \brief relabel objects according to their shape attributes
 *
 * StatisticsRelabelLabelMapFilter relabel a label collection image according to the statistics attributes of
 * the objects. The label produced are always consecutives.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \sa StatisticsLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT StatisticsRelabelLabelMapFilter : public ShapeRelabelLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(StatisticsRelabelLabelMapFilter);

  /** Standard class type aliases. */
  using Self = StatisticsRelabelLabelMapFilter;
  using Superclass = ShapeRelabelLabelMapFilter<TImage>;
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
  itkTypeMacro(StatisticsRelabelLabelMapFilter, ShapeRelabelLabelMapFilter);

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

protected:
  StatisticsRelabelLabelMapFilter();
  ~StatisticsRelabelLabelMapFilter() override = default;

  void
  GenerateData() override;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStatisticsRelabelLabelMapFilter.hxx"
#endif

#endif
