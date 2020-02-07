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
#ifndef itkValuedRegionalMaximaImageFilter_h
#define itkValuedRegionalMaximaImageFilter_h

#include "itkValuedRegionalExtremaImageFilter.h"
#include "itkConceptChecking.h"

#include <functional>

namespace itk
{
/** \class ValuedRegionalMaximaImageFilter
 *
 * \brief Transforms the image so that any pixel that is not a
 * regional maxima is set to the minimum value for the pixel
 * type. Pixels that are regional maxima retain their value.
 *
 * Regional maxima are flat zones surrounded by pixels of lower
 * value. A completely flat image will be marked as a regional maxima
 * by this filter.
 *
 * This code was contributed in the Insight Journal paper:
 * "Finding regional extrema - methods and performance"
 * by Beare R., Lehmann G.
 * https://hdl.handle.net/1926/153
 * http://www.insight-journal.org/browse/publication/65
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa ValuedRegionalMinimaImageFilter
 * \sa ValuedRegionalExtremaImageFilter
 * \sa HMinimaImageFilter
 *
 * \ingroup MathematicalMorphologyImageFilters
 * \ingroup ITKITKMathematicalMorphology
 *
 * \sphinx
 * \sphinxexample{Filtering/MathematicalMorphology/ValuedRegionalMaximaImage,Valued Regional Maxima Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ValuedRegionalMaximaImageFilter
  : public ValuedRegionalExtremaImageFilter<TInputImage,
                                            TOutputImage,
                                            std::greater<typename TInputImage::PixelType>,
                                            std::greater<typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ValuedRegionalMaximaImageFilter);

  using Self = ValuedRegionalMaximaImageFilter;

  using Superclass = ValuedRegionalExtremaImageFilter<TInputImage,
                                                      TOutputImage,
                                                      std::greater<typename TInputImage::PixelType>,
                                                      std::greater<typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = TInputImage;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ValuedRegionalMaximaImageFilter, ValuedRegionalExtremaImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputPixelTypeComparable, (Concept::GreaterThanComparable<InputImagePixelType>));
  itkConceptMacro(InputHasPixelTraitsCheck, (Concept::HasPixelTraits<InputImagePixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputImagePixelType>));
  // End concept checking
#endif

protected:
  ValuedRegionalMaximaImageFilter()
  {
    this->SetMarkerValue(NumericTraits<typename TOutputImage::PixelType>::NonpositiveMin());
  }

  ~ValuedRegionalMaximaImageFilter() override = default;
}; // end
   // ValuedRegionalMaximaImageFilter
} // end namespace itk

#endif
