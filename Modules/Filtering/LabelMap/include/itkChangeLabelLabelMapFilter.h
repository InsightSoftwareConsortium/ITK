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
#ifndef itkChangeLabelLabelMapFilter_h
#define itkChangeLabelLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include <map>

namespace itk
{
/**
 *\class ChangeLabelLabelMapFilter
 * \brief  Replace the label Ids of selected LabelObjects with new label Ids.
 *
 * This filter takes as input a label map and a list of pairs of Label Ids, to
 * produce as output a new label map where the label Ids have been replaced
 * according to the pairs in the list.
 *
 * Labels that are relabeled to the same label Id are automatically
 * merged and optimized into a single LabelObject. The background
 * label can also be changed. Any object relabeled to the output
 * background will automatically be removed.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter, ChangeLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ChangeLabelLabelMapFilter : public InPlaceLabelMapFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ChangeLabelLabelMapFilter);

  /** Standard class type aliases. */
  using Self = ChangeLabelLabelMapFilter;
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

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ChangeLabelLabelMapFilter, InPlaceLabelMapFilter);

  using ChangeMapType = typename std::map<PixelType, PixelType>;
  using ChangeMapIterator = typename ChangeMapType::const_iterator;

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
   */
  void
  SetChangeMap(const ChangeMapType & changeMap);

  const ChangeMapType &
  GetChangeMap() const;

  void
  SetChange(const PixelType & oldLabel, const PixelType & newLabel);

  void
  ClearChangeMap();

protected:
  ChangeLabelLabelMapFilter() = default;
  ~ChangeLabelLabelMapFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  ChangeMapType m_MapOfLabelToBeReplaced;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkChangeLabelLabelMapFilter.hxx"
#endif

#endif
