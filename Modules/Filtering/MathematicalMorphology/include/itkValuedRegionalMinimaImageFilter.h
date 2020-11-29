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
#ifndef itkValuedRegionalMinimaImageFilter_h
#define itkValuedRegionalMinimaImageFilter_h

#include "itkValuedRegionalExtremaImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class ValuedRegionalMinimaImageFilter
 *
 * \brief Transforms the image so that any pixel that is not a
 * regional minima is set to the maximum value for the pixel
 * type. Pixels that are regional minima retain their value.
 *
 * Regional minima are flat zones surrounded by pixels of higher
 * value. A completely flat image will be marked as a regional minima
 * by this filter.
 *
 * This code was contributed in the Insight Journal paper:
 * "Finding regional extrema - methods and performance"
 * by Beare R., Lehmann G.
 * https://www.insight-journal.org/browse/publication/65
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa ValuedRegionalMaximaImageFilter, ValuedRegionalExtremaImageFilter,
 * \sa HMinimaImageFilter
 * \ingroup MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 *
 * \sphinx
 * \sphinxexample{Filtering/MathematicalMorphology/ValuedRegionalMinimalImage,Valued Regional Minimal Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ValuedRegionalMinimaImageFilter
  : public ValuedRegionalExtremaImageFilter<TInputImage,
                                            TOutputImage,
                                            std::less<typename TInputImage::PixelType>,
                                            std::less<typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ValuedRegionalMinimaImageFilter);

  using Self = ValuedRegionalMinimaImageFilter;

  using Superclass = ValuedRegionalExtremaImageFilter<TInputImage,
                                                      TOutputImage,
                                                      std::less<typename TInputImage::PixelType>,
                                                      std::less<typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = TInputImage;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ValuedRegionalMinimaImageFilter, ValuedRegionalExtremaImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputPixelTypeComparable, (Concept::LessThanComparable<InputImagePixelType>));
  itkConceptMacro(InputHasPixelTraitsCheck, (Concept::HasPixelTraits<InputImagePixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputImagePixelType>));
  // End concept checking
#endif

protected:
  ValuedRegionalMinimaImageFilter() { this->SetMarkerValue(NumericTraits<typename TOutputImage::PixelType>::max()); }

  ~ValuedRegionalMinimaImageFilter() override = default;
}; // end
   // ValuedRegionalMinimaImageFilter
} // end namespace itk
#endif
