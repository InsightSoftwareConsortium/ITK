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
#ifndef itkLabelUniqueLabelMapFilter_h
#define itkLabelUniqueLabelMapFilter_h

#include "itkAttributeUniqueLabelMapFilter.h"
#include "itkLabelObject.h"
#include "itkLabelObjectAccessors.h"
#include <set>


namespace itk
{
/**
 *\class LabelUniqueLabelMapFilter
 * \brief Make sure that the objects are not overlapping
 *
 * AttributeUniqueLabelMapFilter search the overlapping zones in the overlapping
 * objects and keeps only a single object on all the pixels of the image.
 * The object to keep is selected according to their label.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa AttributeLabelObject
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TImage>
class LabelUniqueLabelMapFilter
  : public AttributeUniqueLabelMapFilter<TImage,
                                         typename Functor::LabelLabelObjectAccessor<typename TImage::LabelObjectType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelUniqueLabelMapFilter);

  /** Standard class type aliases. */
  using Self = LabelUniqueLabelMapFilter;
  using Superclass =
    AttributeUniqueLabelMapFilter<TImage, typename Functor::LabelLabelObjectAccessor<typename TImage::LabelObjectType>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using ImageType = TImage;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using PixelType = typename ImageType::PixelType;
  using IndexType = typename ImageType::IndexType;

  using AttributeAccessorType = typename Superclass::AttributeAccessorType;
  using AttributeValueType = typename Superclass::AttributeValueType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelUniqueLabelMapFilter, AttributeUniqueLabelMapFilter);

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
  LabelUniqueLabelMapFilter() = default;
  ~LabelUniqueLabelMapFilter() override = default;
}; // end of class

} // end namespace itk

#endif
