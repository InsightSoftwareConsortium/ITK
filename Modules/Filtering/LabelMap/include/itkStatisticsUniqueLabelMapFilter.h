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
#ifndef itkStatisticsUniqueLabelMapFilter_h
#define itkStatisticsUniqueLabelMapFilter_h

#include "itkShapeUniqueLabelMapFilter.h"
#include "itkStatisticsLabelObject.h"

namespace itk
{
/**
 *\class StatisticsUniqueLabelMapFilter
 * \brief Remove some pixels in the label object according to the value of
 * their statistics attribute to ensure that a pixel is not in multiple
 * objects.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, BinaryShapeOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT StatisticsUniqueLabelMapFilter : public ShapeUniqueLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(StatisticsUniqueLabelMapFilter);

  /** Standard class type aliases. */
  using Self = StatisticsUniqueLabelMapFilter;
  using Superclass = ShapeUniqueLabelMapFilter<TImage>;
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
  itkTypeMacro(StatisticsUniqueLabelMapFilter, ShapeUniqueLabelMapFilter);

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
  StatisticsUniqueLabelMapFilter();
  ~StatisticsUniqueLabelMapFilter() override = default;

  void
  GenerateData() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkStatisticsUniqueLabelMapFilter.hxx"
#endif

#endif
