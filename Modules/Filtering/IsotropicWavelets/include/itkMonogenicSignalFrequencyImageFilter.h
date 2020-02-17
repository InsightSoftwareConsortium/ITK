/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkMonogenicSignalFrequencyImageFilter_h
#define itkMonogenicSignalFrequencyImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkVectorImage.h>
#include <itkFrequencyFFTLayoutImageRegionConstIteratorWithIndex.h>
#include "itkRieszFrequencyFunction.h"
namespace itk
{
/** \class MonogenicSignalFrequencyImageFilter
 * Analytical or monogenic signal filter, analogous to Hilbert transform in nD.
 * Require input to be a complex image.
 * Output is a VectorImage<TInputImage::PixelType, ImageDimension + 1>,
 * the first index is the original input, and the next are the Riesz Components for each dimension.
 *
 * This filter is used to generate a monogenic signal in the frequency domain.
 * \f$ f_m = { f_0, R_x, R_y, R_z } \f$
 *
 * The monogenic signal can be used to perform phase analysis for feature detection.
 * \sa PhaseAnalysisImageFilter
 *
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage,
          typename TFrequencyImageRegionConstIterator =
            FrequencyFFTLayoutImageRegionConstIteratorWithIndex<TInputImage>>
class MonogenicSignalFrequencyImageFilter
  : public ImageToImageFilter<TInputImage, VectorImage<typename TInputImage::PixelType, TInputImage::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MonogenicSignalFrequencyImageFilter);

  /** Standard class type alias. */
  using Self = MonogenicSignalFrequencyImageFilter;
  using Superclass =
    ImageToImageFilter<TInputImage, VectorImage<typename TInputImage::PixelType, TInputImage::ImageDimension>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MonogenicSignalFrequencyImageFilter, ImageToImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /// This ensure that InputPixelType is complex<float||double>
  itkConceptMacro(InputPixelTypeIsComplexAndFloatCheck,
                  (Concept::IsFloatingPoint<typename TInputImage::PixelType::value_type>));
#endif

  /** Some convenient type alias. */
  using InputFrequencyImageRegionConstIterator = TFrequencyImageRegionConstIterator;
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** RieszFunction type alias. */
  using RieszFunctionType = RieszFrequencyFunction<typename InputImageType::PixelType, ImageDimension>;
  using RieszFunctionPointer = typename RieszFunctionType::Pointer;

  /** Get the riesz function type */
  itkGetModifiableObjectMacro(Evaluator, RieszFunctionType);

protected:
  MonogenicSignalFrequencyImageFilter();
  ~MonogenicSignalFrequencyImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateOutputInformation() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  RieszFunctionPointer m_Evaluator;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMonogenicSignalFrequencyImageFilter.hxx"
#endif

#endif
